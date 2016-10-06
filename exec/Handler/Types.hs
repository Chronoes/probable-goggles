module Handler.Types where

import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Data.Aeson ((.=), (.:), (.:?), ToJSON, FromJSON, Value(Object), parseJSON, toJSON, toEncoding, object, pairs)


data FileBody = FileBody {
    downloadStatus :: Int,
    mimeType :: Maybe String,
    content :: Maybe Text
} deriving (Show)

instance FromJSON FileBody where
    parseJSON (Object v) =
        FileBody <$>
        v .: "status" <*>
        v .:? "mime-type" <*>
        v .:? "content"

instance ToJSON FileBody where
    toJSON (FileBody s Nothing Nothing) =
        object ["status" .= s]
    toJSON (FileBody s m c) =
        object ["status" .= s, "mime-type" .= m, "content" .= c]

    toEncoding (FileBody s Nothing Nothing) =
        pairs ("status" .= s)
    toEncoding (FileBody s m c) =
        pairs ("status" .= s <> "mime-type" .= m <> "content" .= c)
