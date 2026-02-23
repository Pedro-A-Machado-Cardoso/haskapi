{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module API.Custom (runCustom) where -- The following is an API with most of its config being in the form of arguments.
    import Http.Client (ApiRequest(..), requestJSON, valueToText)
    import Network.HTTP.Req
    import Data.Text as T
    import Data.Aeson (Value(..), decode)
    import qualified Data.Text.Encoding as TE
    import qualified Data.ByteString.Lazy as BL
    import Data.List.Split (splitOn)
    import Data.Maybe (fromMaybe)

    textToValueMaybe :: Text -> Maybe Value
    textToValueMaybe t =
        decode (BL.fromStrict (TE.encodeUtf8 t))
    
    textToMethod :: Text -> Maybe ApiRequest
    textToMethod m = case T.toLower m of
        "get" -> Just GetRequest
        "delete" -> Just DeleteRequest
        _ -> Nothing

    makeUrl :: Text -> Text -> Either (Url 'Http) (Url 'Https)
    makeUrl ssl url = case T.toLower ssl of
        "https" -> Right (https url)
        "http"  -> Left (http url)
        _       -> error "Ssl must be http or https."

    splitEnd :: [Text] -> [[Text]]
    splitEnd xs = Data.List.Split.splitOn ["*END"] xs

    pairUp :: [a] -> [(a, a)]
    pairUp [] = []
    pairUp (k:v:rest) = (k, v) : pairUp rest
    pairUp [_] = error "Headers must come in key-value pairs."

    buildHeaders :: (Text, Text) -> Option scheme
    buildHeaders (k, v) = header (TE.encodeUtf8 k) (TE.encodeUtf8 v)

    custom :: Text -> Text -> Text -> Text -> [Text] -> IO Text
    custom ssl method url body rest = do
        let sections = Prelude.filter (not . Prelude.null) (splitEnd rest)
        let (path, headers) = case sections of
                [p, h] -> (p, (pairUp h))
                [_] -> error "No headers found. Make sure to split."
                _ -> error "Too many arguments. End the command with \"*END\"."
        let opts = foldMap buildHeaders headers
        let mBody = case (body, T.toLower method) of
                ("None", method_l) -> fromMaybe GetRequest (textToMethod method_l)
                (_, "post") -> PostRequest (fromMaybe (Object mempty) (textToValueMaybe body))
                (_, "put")  -> PutRequest (fromMaybe (Object mempty) (textToValueMaybe body))
                (_, _)      -> error "Unknown request type."
        case makeUrl ssl url of
            Left u -> do
                v <- runReq defaultHttpConfig
                        (requestJSON u path opts mBody)
                pure (valueToText v)

            Right u -> do
                v <- runReq defaultHttpConfig
                        (requestJSON u path opts mBody)
                pure (valueToText v)
        
    runCustom :: [Text] -> IO Text
    runCustom args =
        case args of
            ("help":_) ->
                error "Help command issued..."
            (ssl:method:url:body:rest) ->
                custom ssl method url body rest
            _ ->
                error "Incorrect usage."