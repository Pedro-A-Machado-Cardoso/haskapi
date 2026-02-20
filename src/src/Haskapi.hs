module Haskapi where

-- import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

main :: IO ()
main = do
    response <- parseRequest "http://httpbin.org/get"
    httpLBS response >>= print