{-# LANGUAGE OverloadedStrings #-}

module API.Registry (dispatch, parseApi) where

    import qualified API.Chuck as Chuck
    import qualified API.PlayerDB as PlayerDB
    import Network.HTTP.Req
    import Data.Text as T
    
    data SupportedApi =
        Chuck |
        PlayerDB

    parseApi :: String -> Maybe SupportedApi
    parseApi s =
        case T.toLower (T.pack s) of
            "chuck"     -> Just Chuck
            "playerdb"  -> Just PlayerDB
            "player"    -> Just PlayerDB
            _      -> Nothing

    dispatch :: SupportedApi -> [String] -> IO Text
    dispatch Chuck args = do
        runReq defaultHttpConfig $ Chuck.runChuck       (Prelude.map T.pack args)
    dispatch PlayerDB args = do
        runReq defaultHttpConfig $ PlayerDB.runPlayerDB (Prelude.map T.pack args)

