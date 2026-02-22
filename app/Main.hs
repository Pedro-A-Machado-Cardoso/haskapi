{-# LANGUAGE OverloadedStrings #-}

module Main where
    import System.Environment
    import API.Registry
    import Data.Text.IO as T

    main :: IO ()
    main = do
        args <- getArgs
        case args of
            [] ->
                Prelude.putStrLn "Missing API name (e.g. chuck)"

            (apiName:rest) ->
                case parseApi apiName of
                    Nothing ->
                        Prelude.putStrLn "Unknown API"

                    Just api -> do
                        result <- dispatch api rest
                        T.putStrLn result

