{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Http.Client (requestElement, requestText, ApiRequest(..)) where
    import Network.HTTP.Req
    import Data.Aeson (Value, FromJSON, withObject, (.:))
    import Data.Aeson.Key (fromText)
    import Data.Aeson.Types (Parser, parseEither)
    import Data.Text (Text, unpack)
    import Data.Aeson (encode)
    import Data.Text.Lazy (toStrict)
    import Data.Text.Lazy.Encoding (decodeUtf8)

    data ApiRequest = GetRequest
        | PostRequest Value
        | PutRequest Value
        | DeleteRequest

    -- requestNoBody
    --     :: Url scheme
    --     -> Option scheme
    --     -> Req Value
    -- requestNoBody url opts = do
    --     response <- req GET url NoReqBody jsonResponse opts

    --     pure (responseBody response)

    -- requestWithBody
    --     :: HttpMethod method
    --     => method
    --     -> Url scheme
    --     -> Option scheme
    --     -> Value
    --     -> Req Value
    -- requestWithBody method url opts body = do

    --     response <- req method url (ReqBodyJson body) jsonResponse opts
    --     pure (responseBody response)

    requestJSON
        :: FromJSON a
        => Url scheme
        -> [Text]
        -> Option scheme
        -> ApiRequest
        -> Req a
    requestJSON baseUrl segments opts apiReq = do
        let url = foldl (/:) baseUrl segments
        response <- case apiReq of
            GetRequest      -> req GET    url  NoReqBody          jsonResponse opts
            DeleteRequest   -> req DELETE url  NoReqBody          jsonResponse opts
            PostRequest body -> req POST  url (ReqBodyJson body)  jsonResponse opts
            PutRequest  body -> req PUT   url (ReqBodyJson body)  jsonResponse opts
        pure (responseBody response)

    requestText
        :: Url scheme
        -> [Text]
        -> Option scheme
        -> ApiRequest
        -> Req Text
    requestText baseUrl segments opts apiReq =
        fmap valueToText (requestJSON baseUrl segments opts apiReq)

    requestElement
        :: FromJSON a
        => Url scheme
        -> [Text]
        -> Option scheme
        -> ApiRequest
        -> Text
        -> Text
        -> Req (Either String a)
    requestElement baseUrl segments opts apiReq key label = do
        v <- requestJSON baseUrl segments opts apiReq
        pure (parseEither (getElement key label) v)

    getElement :: FromJSON a => Text -> Text -> Value -> Parser a
    getElement key label v = withObject (unpack label) (\f -> f .: fromText key) v

    valueToText :: Value -> Text
    valueToText = toStrict . decodeUtf8 . encode
        

