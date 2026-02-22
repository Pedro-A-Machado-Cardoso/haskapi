{-# LANGUAGE OverloadedStrings #-}

module API.Chuck (runChuck) where
    import Http.Client (requestElement, requestText, ApiRequest(..))
    import Network.HTTP.Req
    import Data.Text as T

    baseUrl :: Text
    baseUrl = "api.chucknorris.io" 

    data Endpoint = 
        Random | Category | Search

    parseEndpoint :: Text -> Maybe Endpoint
    parseEndpoint t =
        case T.toLower t of
            "random" -> Just Random
            "category" -> Just Category
            "search" -> Just Search
            _        -> Nothing

    random :: Maybe Text -> Req Text
    random Nothing = do 
        result <- requestElement (https baseUrl)
                            ["jokes", "random"]
                            mempty
                            GetRequest
                            "value"
                            "Joke"
        case result of
            Left err  -> return $ T.pack err  
            Right val -> return val  
    random (Just cat) = do
        result <- requestElement (https baseUrl)
                            ["jokes", "random"]
                            ("category" =: cat)
                            GetRequest
                            "value"
                            "Joke"
        case result of
            Left err  -> return $ T.pack err  
            Right val -> return val    

    categories :: Req Text
    categories = requestText
                    (https baseUrl)
                    ["jokes", "categories"]
                    mempty
                    GetRequest

    search :: Text -> Req Text
    search q = do 
        result <- requestElement
                    (https baseUrl)
                    ["jokes", "search"]
                    ("query" =: q)
                    GetRequest
                    "result"
                    "Results"
        case result of
            Left err  -> return $ T.pack err  
            Right val -> return val  

    -- runWithConfig :: Config -> IO ()
    -- runWithConfig cfg =
    --     runReq defaultHttpConfig $
    --         case endpoint cfg of
    --             Random -> random (Just (argument cfg))

    runChuck :: [Text] -> Req Text
    runChuck args = do
        case args of
            [] ->
                error "No arguments."
            (endpoint:rest) ->
                case parseEndpoint endpoint of
                    Just Random -> case Prelude.length rest of
                        0 -> random Nothing
                        _ -> random (Just (rest !! 0))
                    Just Category -> categories
                    Just Search -> case Prelude.length rest of
                        0 -> error "Missing search parameter."
                        _ -> search (rest !! 0)
                    Nothing -> error "Missing search endpoint."


        -- case parseArgs args of
        --     Left err ->
        --         putStrLn err

        --     Right cfg ->
        --         runWithConfig cfg