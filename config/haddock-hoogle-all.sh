#!/bin/sh
stack haddock --stack-yaml=apps/hs/qua-view/stack.yaml
stack haddock --stack-yaml=apps/hs/qua-server/stack.yaml
stack haddock --stack-yaml=apps/hs/helen/stack.yaml
stack haddock --stack-yaml=services/siren/stack.yaml
stack haddock --stack-yaml=libs/hs/luci-connect/stack.yaml
stack exec hoogle -- generate \
                  --local=$(stack path --stack-yaml=apps/hs/qua-server/stack.yaml --snapshot-doc-root) \
                  --local=$(stack path --stack-yaml=apps/hs/qua-view/stack.yaml --snapshot-doc-root) \
                  --local=$(stack path --stack-yaml=apps/hs/qua-view/stack.yaml --local-doc-root) \
                  --local=$(stack path --stack-yaml=apps/hs/qua-server/stack.yaml --local-doc-root) \
                  --local=$(stack path --stack-yaml=apps/hs/helen/stack.yaml --local-doc-root) \
                  --local=$(stack path --stack-yaml=services/siren/stack.yaml --local-doc-root) \
                  --local=$(stack path --stack-yaml=libs/hs/luci-connect/stack.yaml --local-doc-root)
