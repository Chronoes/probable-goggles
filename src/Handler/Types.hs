module Handler.Types where

import Data.Maybe (fromJust)
import Data.Either (either)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Data.Aeson ((.=), (.:), (.:?), ToJSON, FromJSON, Value(Object), parseJSON, toJSON, toEncoding, object, pairs)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Base64.Lazy as S64

data FileBody = FileBody {
    downloadStatus :: Int,
    mimeType :: Maybe String,
    rawContent :: Maybe Text
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


decodeContent :: Maybe Text -> Either String ByteString
decodeContent Nothing = Left "No content to decode"
decodeContent (Just c) = S64.decode $ encodeUtf8 c

decodeContent' :: Maybe Text -> ByteString
decodeContent' c = either (const "") id . S64.decode . encodeUtf8 $ fromJust c

encodeContent :: ByteString -> Maybe Text
encodeContent = Just . decodeUtf8 . S64.encode
