module Search(matches) where

import Types

matches :: Query -> Entry -> Bool
matches q e | isAll $ queryMatchType q   = Prelude.all (flip elem $ entryTags e) $ queryTags q
            | isSome $ queryMatchType q  = any (flip elem $ entryTags e) $ queryTags q
            | otherwise                  = not $ matches (query (queryTags q) Types.some) e