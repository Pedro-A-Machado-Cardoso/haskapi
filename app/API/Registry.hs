{-# LANGUAGE OverloadedStrings #-}

module API.Registry (dispatch, parseApi) where

    import qualified API.Chuck as Chuck
    import Network.HTTP.Req
    import Data.Text as T

    data SupportedApi = 
        Chuck

    parseApi :: String -> Maybe SupportedApi
    parseApi s =
        case T.toLower (T.pack s) of
            "chuck" -> Just Chuck
            _      -> Nothing

    dispatch :: SupportedApi -> [String] -> IO Text
    dispatch Chuck args = do
        result <- runReq defaultHttpConfig $ Chuck.runChuck (Prelude.map T.pack args)
        return result
        