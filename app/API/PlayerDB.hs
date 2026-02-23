{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module API.PlayerDB (runPlayerDB) where
    import Http.Client (ApiRequest(..), requestJSON, findByKey)
    import Network.HTTP.Req
    import Data.Text as T
    import Data.Aeson
    import qualified Data.Text.Encoding as TE
    import Data.Maybe (fromMaybe)
    
    data Endpoint =
        Minecraft | Hytale | Steam | Xbox

    data Field =
        Username | Id | Avatar | Meta

    parseEndpoint :: Text -> Maybe Endpoint
    parseEndpoint t =
        case T.toLower t of
            "minecraft" -> Just Minecraft
            "mc"        -> Just Minecraft
            "hytale"    -> Just Hytale
            "steam"     -> Just Steam
            "gabenland" -> Just Steam -- :)
            "xbox"      -> Just Xbox
            _           -> Nothing

    parseField :: Text -> Maybe Field
    parseField t =
        case T.toLower t of
            "user"      -> Just Username
            "username"  -> Just Username
            "id"        -> Just Id
            "avatar"    -> Just Avatar
            "meta"      -> Just Meta
            "facebook"  -> Just Meta
            _           -> Nothing

    headers :: Text -> Option scheme
    headers agent = header "user-agent" (TE.encodeUtf8 agent)

    baseUrl :: Url 'Https
    baseUrl = https "playerdb.co"

    requestPlayer :: FromJSON a => Text -> Text -> Text -> Req a
    requestPlayer game arg agent = do
        requestJSON
            baseUrl
            ["api", "player", game, arg]
            (headers agent)
            GetRequest

    requestInfo :: Text -> Text -> Text -> Text -> Req Value
    requestInfo game arg agent info = do
        response <- requestJSON baseUrl ["api", "player", game, arg] (headers agent) GetRequest :: Req Value
        let results = findByKey info response
        return (Prelude.head results)

    valueToText :: Value -> Maybe Text
    valueToText (String t) = Just t
    valueToText _          = Nothing

    runPlayerDB :: [Text] -> Req Text
    runPlayerDB args = do
        case args of
            []  ->
                error "No arguments."
            [_] ->
                error "Not enough arguments."
            (endpoint:field:rest) ->
                case (parseEndpoint endpoint, parseField field) of
                    (Just Minecraft, Nothing)   -> requestPlayer "minecraft"    (Prelude.head rest) (Prelude.last rest)
                    (Just Hytale, Nothing)      -> requestPlayer "hytale"       (Prelude.head rest) (Prelude.last rest)
                    (Just Steam, Nothing)       -> requestPlayer "steam"        (Prelude.head rest) (Prelude.last rest)
                    (Just Xbox, Nothing)        -> requestPlayer "xbox"         (Prelude.head rest) (Prelude.last rest)
                    (Nothing, Nothing)          -> error "Missing search endpoint, you may try 'player <endpoint> <field> <name/id>'"
                    (Just Minecraft, _)         -> fmap (fromMaybe "default" . valueToText) (requestInfo "minecraft"      (Prelude.head rest) (Prelude.last rest) field)
                    (Just Hytale, _)            -> fmap (fromMaybe "default" . valueToText) (requestInfo "hytale"         (Prelude.head rest) (Prelude.last rest) field)
                    (Just Steam, _)             -> fmap (fromMaybe "default" . valueToText) (requestInfo "steam"          (Prelude.head rest) (Prelude.last rest) field)
                    (Just Xbox, _)              -> fmap (fromMaybe "default" . valueToText) (requestInfo "xbox"           (Prelude.head rest) (Prelude.last rest) field)
                    (Nothing, _)                -> error "Missing search endpoint, you may try 'player <endpoint> <field> <name/id>"