module Http.Client (requestElement, requestText, ApiRequest(..), requestJSON, getElement, findByKey, valueToText) where
    import Network.HTTP.Req
    import Data.Aeson ( Value, FromJSON, withObject, (.:), encode, Value(..) )
    import Data.Aeson.Key (fromText)
    import Data.Aeson.Types (Parser, parseEither )
    import Data.Text (Text, unpack)
    import Data.Text.Lazy.Encoding (decodeUtf8)
    import qualified Data.Aeson.KeyMap as KM
    import qualified Data.Aeson.Key    as K
    import qualified Data.Vector as V
    import Data.Text.Lazy (toStrict)

    data ApiRequest = GetRequest
        | PostRequest Value
        | PutRequest Value
        | DeleteRequest

    -- Find ALL values matching a key anywhere in the JSON tree
    findByKey :: Text -> Value -> [Value]
    findByKey key val = case val of
        Object obj ->
            let found  = maybe [] pure $ KM.lookup (K.fromText key) obj
                deeper = concatMap (findByKey key) (KM.elems obj)
            in found ++ deeper
        Array arr -> concatMap (findByKey key) (V.toList arr)
        _         -> []

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
    getElement key label = withObject (unpack label) (\f -> f .: fromText key)

    valueToText :: Value -> Text
    valueToText = toStrict . decodeUtf8 . encode


