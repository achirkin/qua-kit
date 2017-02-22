-----------------------------------------------------------------------------
-- |
-- Module      :  Lib.ParseGeoJSON
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Parse any GeoJSON into our internal representation consisting of Blocks and Walls.
--
-----------------------------------------------------------------------------

module Lib.ParseGeoJSON
  ( parseBlocks, parseGeoJSONBS, parseGeoJSONValue, toGeoJSONValue
  , GeoJSONProps
  ) where

import Data.Aeson as JSON
import Data.Semigroup
import Data.Maybe (mapMaybe)
import Data.ByteString (ByteString)
import           Data.Geospatial
import           Data.LinearRing
import qualified Control.Lens as Lens


import Numeric.EasyTensor
import Lib.ExtraTypes
import Lib.Scenario as S

type GeoJSONProps = (GeospatialGeometry, JSON.Object)

-- | Parse Strict ByteString containg utf8-encoded JSON into a Block-Wall representation
parseGeoJSONBS :: ByteString -> Either String (Scenario (GeospatialGeometry, JSON.Object))
parseGeoJSONBS b = S.fromList . Lens.view (geofeatures . traverse . Lens.lens parseFeature const) <$>
    (eitherDecodeStrict' b :: Either String (GeoFeatureCollection JSON.Object))
 where
   parseFeature f = let blocks = parseBlocks $ Lens.view geometry f
                    in map ((Lens.view geometry f, Lens.view properties f) <$) blocks

-- | Parse Aeson's Value type into a Block-Wall representation
parseGeoJSONValue :: JSON.Value -> Either String (Scenario (GeospatialGeometry, JSON.Object))
parseGeoJSONValue b = fromResult
    $ Lens.view (geofeatures . traverse . Lens.lens parseFeature const) <$>
     (fromJSON b :: Result (GeoFeatureCollection JSON.Object))
  where
    fromResult (Error s) = Left s
    fromResult (Success a) = Right $ S.fromList a
    parseFeature f = let blocks = parseBlocks $ Lens.view geometry f
                     in map ((Lens.view geometry f, Lens.view properties f) <$) blocks

toGeoJSONValue :: Scenario (GeospatialGeometry, JSON.Object) -> JSON.Value
toGeoJSONValue sc = toJSON . GeoFeatureCollection Nothing $ mkFeature . Lens.view content <$> S.toList sc
  where
    mkFeature (geom, props) = GeoFeature Nothing geom props Nothing

-- | Parse GeoJSON geometry type into a number of Blocks
parseBlocks :: GeospatialGeometry -> [Block ()]
parseBlocks = mapMaybe parseBlock . getMultiPolygonPoints
  where
    -- build walls based on center location and linear ring coordinates
    buildWalls :: Vec2f -> [Vec2f] -> [Wall]
    buildWalls c (a:b:xs) = Wall ((b+a)/2 - c) ((b-a)/2) : buildWalls c (b:xs)
    buildWalls _ [_] = []
    buildWalls _ [] = []

    -- transform list of points into a proper 2D vector, discarding extra coordinates
    toVec :: [Double] -> Vec2f
    toVec (a:b:_) = vec2 (realToFrac a) (realToFrac b)
    toVec [a]     = vec2 (realToFrac a) 0
    toVec []      = 0

    -- calculate center -- average coordinate
    avg :: [Vec2f] -> Vec2f
    -- omit first point, because it occurs twice in a list according to GeoJSON spec
    avg (_:xs) = let (s, n) = foldr (\x (s',n') -> (s' + x, n' + 1)) (0,0) xs
                 in s / n
    avg []     = 0

    -- Convert any type of Geospatial geometry into proper multipolygon series of points.
    -- So, nesting levels are: blocks -> polygons -> linearrings -> points
    getMultiPolygonPoints :: GeospatialGeometry -> [[[[Vec2f]]]]
    getMultiPolygonPoints NoGeometry       = []
    getMultiPolygonPoints Point{}          = []
    getMultiPolygonPoints MultiPoint{}     = []
    getMultiPolygonPoints Line{}           = []
    getMultiPolygonPoints MultiLine{}      = []
    getMultiPolygonPoints (Polygon p)      = [[map toVec . fromLinearRing <$>_unGeoPolygon p]]
    getMultiPolygonPoints (MultiPolygon p) = [_unGeoMultiPolygon p >>= concat . getMultiPolygonPoints . Polygon . GeoPolygon]
    getMultiPolygonPoints (Collection xs)  = xs >>= getMultiPolygonPoints

    -- Area of a single polygon ring (i.e. simple polygon)
    -- Determinse CCW vs CW vertex orders plus if it is degenerate
    -- Actual formula uses factor 0.5, so this is a doubled area
    getRingArea :: [Vec2f] -> Scf
    getRingArea (a:b:xs) = det2 a b + getRingArea (b:xs)
    getRingArea [_]      = 0
    getRingArea []       = 0


    -- Here I do only partial checking of polygons.
    --   I fix their orientation, try to fix holes (only orientation checks)
    --   I do not take a proper union of polygons, just take the biggest one.
    --   TODO: for the current case study this should be enough, but in general this will not work!
    parseBlock :: [[[Vec2f]]] -> Maybe (Block ())
    parseBlock [] = Nothing
    parseBlock xs' = parsePoly <$> bestPoly
      where
        -- Transform a proper polygon into a building block.
        --   I assume: all rings are in proper order -- CCW outer, CW holes
        --             area of a polygon is positive, which means there are more than two walls
        parsePoly :: [[Vec2f]] -> Block ()
        parsePoly rs = Block
           { _bCenter = center + mid
           , _bWalls  = concatMap (buildWalls center) rs
           , _bBound  = bb
           , _content = ()
           }
          where
            -- center of a building block
            center = (/ realToFrac (length rs)) . getSum $ foldMap (Sum . avg) rs

        -- lower left and upper right corner of a building block
        bb@(MinMax ll ur) = option mmBound id
                          $ foldMap (foldMap (foldMap (Option . Just . minMax))) xs'
        mid = ll/2 + ur/2
        -- shift all coordinates to reduce numerical errors
        xs = map (map (\x -> x - mid)) <$> xs'


        -- size of a building block - for normalization
        diameter = normL2 $ ur - ll

        -- threshold for considering something squared be zero
        eps2 = diameter * diameter * 0.00001

        -- Just get a polygon with the largest area.
        --   The better option would be to calculate union of polygons, but I do not have time for this now.
        bestPoly = case foldMap (\(s, x) -> Option . Just . Max $ Arg x (abs s)) $ fixPolygon <$> xs of
                     Option (Just (Max (Arg x s))) -> if s >= eps2 then Just x else Nothing
                     Option Nothing                -> Nothing

        -- Remove degenerate rings (zero area),
        -- Make outer ring CCW, change other rings accordingly.
        --   Here I assume that the first (outer) ring fully contains all other rings.
        fixPolygon :: [[Vec2f]] -> (Scf, [[Vec2f]])
        fixPolygon (r1:rs) = let ra  = getRingArea r1
                                 -- Here I assume that all inner linear rings are properly defined:
                                 --   each inner polygon is fully inside a hole;
                                 --   Thus, if I add up all rings' areas, the result will have
                                 --   the same sign as an outermost ring.
                                 --   I want to enforce at least outer hole be in CW order,
                                 --   this is why I need to reverse all rings if total area is positive.
                                 totalHoleArea = foldMap (Sum . getRingArea) rs
                                 rs' = if totalHoleArea > 0 then reverse <$> rs else rs
                                -- First (outer) ring is CCW
          in      if ra >  eps2 then (ra, r1 : rs')
                                -- First (outer) ring is CW
             else if ra < -eps2 then (- ra, reverse r1 : rs')
                                -- The first ring is degenerate means whole polygon is screwed up.
                                -- But I still hope for the good and try next ring.
                                else fixPolygon rs
        fixPolygon [] = (0, [])
