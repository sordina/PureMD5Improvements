{-# LANGUAGE TemplateHaskell #-}

import Data.Digest.Pure.MD5
import Test.QuickCheck.All (quickCheckAll)

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8      as B

twoPartMD5 :: String -> String -> MD5Digest
twoPartMD5 x y = let initial = md5Update md5InitialContext (B.pack x)
                     update  = md5Update initial           (B.pack y)
                 in  md5Finalize update                    (B.pack " ")

prop_md5 :: String -> String -> Bool
prop_md5 x y = show (md5 (L.pack (x ++ y))) == show (twoPartMD5 x y)

main :: IO ()
main = $quickCheckAll >> return ()
