{-# LANGUAGE OverloadedLists #-}
module Base (normaliseFileType) where
import Data.Char
import Data.Map as M

normaliseFileType :: String -> String
normaliseFileType = headCapt . lookUpAbbr

headCapt :: String -> String
headCapt [] = []
headCapt (h:t) = toUpper h : t

lookUpAbbr :: String -> String
lookUpAbbr ft = case ft `M.lookup` abbrs of
                     Nothing -> ft
                     (Just ft') -> ft'

-- TODO : consider better method than coding by myself
abbrs :: Map String String
abbrs =
    [
        ("hs", "haskell")
      , ("py", "python")
    ]

