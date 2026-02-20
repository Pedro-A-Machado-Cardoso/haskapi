module Main (main) where

import           Network.HTTP.Simple

main :: IO ()
main = do
    response <- parseRequest "http://httpbin.org/get"
    httpLBS response >>= print
