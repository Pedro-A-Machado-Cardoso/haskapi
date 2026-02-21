module Http.Client (request) where
    import Network.HTTP.Req
    import Data.Aeson (Value)
    import Data.Maybe (fromMaybe)

request
  :: HttpMethod
  -> Url scheme
  -> [Text]
  -> Option scheme
  -> Maybe Value
  -> Req Value
    request method baseUrl segments opts mBody = do
        let url = foldl (/:) baseUrl segments

        let body =
            case mBody of
            Nothing -> NoReqBody
            Just v  -> ReqBodyJson v

        response <-
            req
            method
            url
            body
            jsonResponse
            opts

        pure (responseBody response)

