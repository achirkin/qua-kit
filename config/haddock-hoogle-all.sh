#!/bin/sh
stack build --haddock --haddock-deps --haddock-hyperlink-source --stack-yaml=apps/hs/qua-view/stack.yaml --haddock-arguments --hoogle
stack build --haddock --haddock-deps --haddock-hyperlink-source --stack-yaml=apps/hs/qua-server/stack.yaml
stack build --haddock --haddock-deps --haddock-hyperlink-source --stack-yaml=apps/hs/helen/stack.yaml
stack build --haddock --haddock-deps --haddock-hyperlink-source --stack-yaml=services/siren/stack.yaml
stack build --haddock --haddock-deps --haddock-hyperlink-source --stack-yaml=libs/hs/luci-connect/stack.yaml
stack build --haddock --haddock-deps --haddock-hyperlink-source --stack-yaml=libs/hs/qua-types/stack.yaml
stack exec hoogle -- generate \
                  --local=$(stack path --stack-yaml=apps/hs/qua-server/stack.yaml --snapshot-doc-root) \
                  --local=$(stack path --stack-yaml=apps/hs/qua-view/stack.yaml --snapshot-doc-root) \
                  --local=$(stack path --stack-yaml=apps/hs/qua-view/stack.yaml --local-doc-root) \
                  --local=$(stack path --stack-yaml=apps/hs/qua-server/stack.yaml --local-doc-root) \
                  --local=$(stack path --stack-yaml=apps/hs/helen/stack.yaml --local-doc-root) \
                  --local=$(stack path --stack-yaml=services/siren/stack.yaml --local-doc-root) \
                  --local=$(stack path --stack-yaml=libs/hs/luci-connect/stack.yaml --local-doc-root) \
                  --local=$(stack path --stack-yaml=libs/hs/qua-types/stack.yaml --local-doc-root)
