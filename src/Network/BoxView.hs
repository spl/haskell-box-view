module Network.BoxView (
  DocInfo(..),
  DocStatus(..),
  DocEntriesQuery(..),
  DownloadFormat(..),
  Dim(..),
  UpdateInfo(..),
  uploadDoc,
  downloadDoc,
  downloadThumb,
  getDocInfo,
  getDocEntries,
  updateDocInfo,
  deleteDoc,
) where

--------------------------------------------------------------------------------

import Control.Applicative ((<$>), (<*>), (<*), (<|>), some)
import Control.Category ((>>>))
import Control.Monad (liftM, ap)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (Value(String), ToJSON(..), FromJSON(..), (.=), (.:))
import Data.Aeson.TH (deriveJSON, deriveToJSON, defaultOptions, Options(..))
import qualified Data.Aeson as A
import qualified Data.Attoparsec.Text as P
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.Char (toLower, toUpper)
import Data.Default.Class (Default(..))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import Data.Time.Clock (UTCTime)
import Network.HTTP.Conduit (Request, Response, Manager)
import qualified Network.HTTP.Client.MultipartFormData as H
import qualified Network.HTTP.Conduit as H
import Network.HTTP.Types
import Network.Mime (MimeType)

--------------------------------------------------------------------------------

-- | Thumbnail dimensions
data Dim = Dim
  { width   :: !Int  -- ^ 16 <= width <= 1024
  , height  :: !Int  -- ^ 16 <= height <=  768
  }
  deriving (Eq, Read, Show)

dimsToText :: [Dim] -> Text
dimsToText =
  TS.intercalate ","
  . map (\Dim {..} -> TS.pack (show width) <> "x" <> TS.pack (show height))

dimsToJSON :: [Dim] -> Maybe Value
dimsToJSON []  = Nothing
dimsToJSON tds = Just $ String $ dimsToText tds

dimsFromJSON :: Monad m => Value -> m [Dim]
dimsFromJSON (String s) =
  case P.maybeResult $ P.parse (some (Dim <$> P.decimal <* P.char 'x' <*> P.decimal)) s of
    Nothing  -> fail $ "dimsFromJSON: Can't parse string: " ++ show s
    Just res -> return res
dimsFromJSON val        = fail $ "dimsFromJSON: Not a string: " ++ show val

dimsToFormPart :: [Dim] -> Maybe ByteString
dimsToFormPart []  = Nothing
dimsToFormPart tds = Just $ TS.encodeUtf8 $ dimsToText tds

dimToQuery :: Dim -> Query
dimToQuery Dim {..} = [ ("width",  Just $ showBS width)
                      , ("height", Just $ showBS height)
                      ]

--------------------------------------------------------------------------------

-- | Enumeration indicating the conversion status of a document
data DocStatus = Queued | Processing | Done | Error
  deriving (Eq, Enum, Bounded, Read, Show)

instance ToJSON DocStatus where
  toJSON = String . TS.toLower . TS.pack . show

instance FromJSON DocStatus where
  parseJSON = A.withText "DocStatus" $
    reads . firstUpper . TS.unpack >>> \case
      [(x, [])] -> return x
      []        -> fail "No result for DocStatus"
      res       -> fail $ "Ambiguous results for DocStatus: " ++ show res
    where
      firstUpper :: String -> String
      firstUpper ""     = ""
      firstUpper (c:cs) = toUpper c : cs

--------------------------------------------------------------------------------

-- | Represents a file that has been submitted to the View API
data DocInfo = DocInfo
  { docId         :: !Text       -- ^ Document ID
  , docStatus     :: !DocStatus  -- ^ Current status of conversion
  , docName       :: !Text       -- ^ Document name
  , docCreatedAt  :: !UTCTime    -- ^ Date of upload
  }
  deriving (Eq, Read, Show)

instance ToJSON DocInfo where
  toJSON (DocInfo {..}) = A.object
    [ "type"        .= String "document"
    , "id"          .= String docId
    , "status"      .= toJSON docStatus
    , "name"        .= String docName
    , "created_at"  .= toJSON docCreatedAt
    ]

instance FromJSON DocInfo where
  parseJSON = A.withObject "DocInfo" $ \o -> do
    "document" :: Text <- o .: "type"
    DocInfo <$> o .: "id"
            <*> o .: "status"
            <*> o .: "name"
            <*> o .: "created_at"

--------------------------------------------------------------------------------

-- | Internal type for 'FromJSON' instance. Used in 'getDocs'.
newtype DocInfoList = DocInfoList [DocInfo]

instance FromJSON DocInfoList where
  parseJSON = A.withObject "DocInfoList" $ \o1 -> do
    o2 <- o1 .: "document_collection"
    count <- o2 .: "total_count"
    entries <- o2 .: "entries"
    if count == length entries then
      return $ DocInfoList entries
    else
      fail $  "DocInfoList: total_count (" ++ show count
           ++ ") does not equal length of entries: " ++ show entries

--------------------------------------------------------------------------------

-- | Document metatdata that can be updated
data UpdateInfo = UpdateInfo
  { updateName       :: !(Maybe Text)  -- ^ Document name
  }
  deriving (Read, Show)

instance Default UpdateInfo where
  def = UpdateInfo Nothing

--------------------------------------------------------------------------------

-- | A document upload request for either a URL or a file.
data UploadRequest = UploadRequest
  { uploadSource      :: !(Either Text FilePath) -- ^ A URL or FilePath
  , uploadName        :: !Text   -- ^ Document name (empty string indicates no name)
  , uploadThumbnails  :: ![Dim]  -- ^ List of thumbnail dimensions (optional)
  , uploadNonSvg      :: !Bool   -- ^ Whether to also create the non-svg version
  }
  deriving (Read, Show)

instance ToJSON UploadRequest where
  toJSON (UploadRequest {..}) = A.object $
    [ either (("url" .=) . String) (("file" .=) . String . TS.pack) uploadSource
    , "name"     .= String uploadName
    , "non_svg"  .= String (TS.toLower $ TS.pack $ show uploadNonSvg)
    ]
    ++ maybeToList (("thumbnails" .=) . toJSON) (dimsToJSON uploadThumbnails)

instance FromJSON UploadRequest where
  parseJSON = A.withObject "UploadRequest" $ \o -> do
    UploadRequest <$> (Left <$> o .: "url" <|> Right <$> o .: "file")
                  <*> o .: "name"
                  <*> (o .: "thumbnails" >>= dimsFromJSON)
                  <*> o .: "non_svg"

fromUploadRequest :: MonadIO m => UploadRequest -> m Request
fromUploadRequest ur@(UploadRequest {..}) =
  case uploadSource of
    Left _ ->
      liftIO (H.parseUrl "https://view-api.box.com/1/documents") >>=
      setMethod POST >>=
      setJSONBody ur
    Right file ->
      liftIO (H.parseUrl "https://upload.view-api.box.com/1/documents") >>=
      H.formDataBody
        ( [ H.partFile "file" file
          , H.partBS "name" $ TS.encodeUtf8 uploadName
          , H.partBS "non_svg" $ TS.encodeUtf8 $ TS.toLower $ TS.pack $ show uploadNonSvg
          ]
          ++ maybeToList (H.partBS "thumbnails") (dimsToFormPart uploadThumbnails)
        )

--------------------------------------------------------------------------------

-- | Query parameters for fetching a list of documents
data DocEntriesQuery = DocEntriesQuery
  { queryLimit          :: !(Maybe Int)      -- ^ Number of documents (default=10, max=50)
  , queryCreatedBefore  :: !(Maybe UTCTime)  -- ^ Upper limit on the creation dates of documents (default=now)
  , queryCreatedAfter   :: !(Maybe UTCTime)  -- ^ lower limit on the creation timestamps of documents
  }
  deriving (Read, Show)

instance Default DocEntriesQuery where
  def = DocEntriesQuery Nothing Nothing Nothing

--------------------------------------------------------------------------------

-- | Format in which a document is downloaded
data DownloadFormat = DownloadOriginal | DownloadPdf | DownloadZip
  deriving (Eq, Enum, Bounded, Read, Show)

instance Default DownloadFormat where
  def = DownloadOriginal

-- | File extension of download. Default is original form.
mkExt :: DownloadFormat -> String
mkExt DownloadOriginal = ""
mkExt DownloadPdf      = ".pdf"
mkExt DownloadZip      = ".zip"

--------------------------------------------------------------------------------
-- Exported

-- | Upload a document according to the 'UploadRequest'
uploadDoc
  :: MonadIO m
  => ByteString       -- ^ API key
  -> UploadRequest    -- ^ Document upload request description
  -> Manager          -- ^ HTTP manager
  -> m DocInfo
uploadDoc apiKey uploadReq mgr = do
  req <- fromUploadRequest uploadReq >>=
         addAuthHeader apiKey
  rsp <- H.httpLbs req mgr
  case A.decode' $ H.responseBody rsp of
    Just body -> return body
    Nothing ->
      fail $  "uploadDoc: Can't decode JSON body from response: "
           ++ show rsp

-- | Get a document's metadata
getDocInfo
  :: MonadIO m
  => ByteString       -- ^ API key
  -> Text             -- ^ Document ID
  -> Manager          -- ^ HTTP manager
  -> m DocInfo
getDocInfo apiKey did mgr = do
  req <- liftIO (H.parseUrl $ "https://view-api.box.com/1/documents/" ++ TS.unpack did) >>=
         addAuthHeader apiKey
  rsp <- H.httpLbs req mgr
  case A.decode' $ H.responseBody rsp of
    Just obj -> return obj
    Nothing ->
      fail $  "getDocInfo: Can't decode JSON body from response: "
           ++ show rsp

-- | Get a document collection according to the optional 'DocEntriesQuery'
getDocEntries
  :: MonadIO m
  => ByteString       -- ^ API key
  -> DocEntriesQuery  -- ^ Query parameters (use 'def' for defaults)
  -> Manager          -- ^ HTTP manager
  -> m [DocInfo]
getDocEntries apiKey params mgr = do
  req <- liftIO (H.parseUrl "https://view-api.box.com/1/documents") >>=
         setJSONBody params >>=
         addAuthHeader apiKey
  rsp <- H.httpLbs req mgr
  case A.decode' $ H.responseBody rsp of
    Just (DocInfoList entries) -> return entries
    Nothing ->
      fail $  "getDocEntries: Can't decode JSON body from response: "
           ++ show rsp

-- | Update a document's metadata
updateDocInfo
  :: MonadIO m
  => ByteString       -- ^ API key
  -> UpdateInfo       -- ^ Metadata to be updated (use 'def' for defaults)
  -> Text             -- ^ Document ID
  -> Manager          -- ^ HTTP manager
  -> m DocInfo
updateDocInfo apiKey updateInfo did mgr = do
  req <- liftIO (H.parseUrl $ "https://view-api.box.com/1/documents/" ++ TS.unpack did) >>=
         setMethod PUT >>=
         setJSONBody updateInfo >>=
         addAuthHeader apiKey
  rsp <- H.httpLbs req mgr
  case A.decode' $ H.responseBody rsp of
    Just obj -> return obj
    Nothing ->
      fail $  "updateDocInfo: Can't decode JSON body from response: "
           ++ show rsp

-- | Download a document
downloadDoc
  :: MonadIO m
  => ByteString       -- ^ API key
  -> DownloadFormat   -- ^ Download format of the file
  -> Text             -- ^ Document ID
  -> Manager          -- ^ HTTP manager
  -> m (MimeType, BL.ByteString)
downloadDoc apiKey format did mgr = do
  let fileName = "content" ++ mkExt format
  req <- liftIO (H.parseUrl $ "https://view-api.box.com/1/documents/" ++ TS.unpack did ++ "/" ++ fileName) >>=
         addAuthHeader apiKey
  H.httpLbs req mgr >>= mimeTypeContent

-- | Download a document's thumbnail. If the thumbnail is ready, a 'Right' value
-- is returned with the MIME type and file contents. If the thumbnail is not
-- ready, a 'Left' value provides the number of seconds to wait before retrying
-- again.
--
-- Note: Thumbnails will always preserve the aspect ratio of the original
-- document but will best fit the dimensions requested. For example, if a 16×16
-- thumbnail is requested for a document with a 2:1 aspect ratio, a 16×8
-- thumbnail will be returned.
downloadThumb
  :: MonadIO m
  => ByteString       -- ^ API key
  -> Dim              -- ^ Dimensions
  -> Text             -- ^ Document ID
  -> Manager          -- ^ HTTP manager
  -> m (Either Int (MimeType, BL.ByteString))
downloadThumb apiKey dim did mgr = do
  req <- liftIO (H.parseUrl $ "https://view-api.box.com/1/documents/" ++ TS.unpack did ++ "/thumbnail") >>=
         setQuery (dimToQuery dim) >>=
         addAuthHeader apiKey
  rsp <- H.httpLbs req mgr
  case statusCode (H.responseStatus rsp) of
    202 -> Left `liftM` readHeader hRetryAfter rsp
    200 -> Right `liftM` mimeTypeContent rsp
    c   -> fail $ "downloadThumb: Unsupported HTTP status: " ++ show c

-- | Delete a document
deleteDoc
  :: MonadIO m
  => ByteString       -- ^ API key
  -> Text             -- ^ Document ID
  -> Manager          -- ^ HTTP manager
  -> m ()
deleteDoc apiKey did mgr = do
  req <- liftIO (H.parseUrl $ "https://view-api.box.com/1/documents/" ++ TS.unpack did) >>=
         setMethod DELETE >>=
         addAuthHeader apiKey
  _ <- H.httpLbs req mgr
  return ()

--------------------------------------------------------------------------------
-- Helpers

addHeader :: Monad m => ByteString -> ByteString -> Request -> m Request
addHeader name val req = return $
  req { H.requestHeaders = (CI.mk name, val) : H.requestHeaders req }

addAuthHeader :: Monad m => ByteString -> Request -> m Request
addAuthHeader apiKey = addHeader "Authorization" ("Token " <> apiKey)

setQuery :: Monad m => Query -> Request -> m Request
setQuery q req = return $ req { H.queryString = renderQuery True q }

setJSONBody :: (Monad m, ToJSON a) => a -> Request -> m Request
setJSONBody obj req =
  return req { H.requestBody = H.RequestBodyLBS $ A.encode $ obj } >>=
  addHeader "Content-Type" "application/json"

setMethod :: Monad m => StdMethod -> Request -> m Request
setMethod m req = return $ req { H.method = renderStdMethod m }

findHeader :: Monad m => HeaderName -> Response b -> m ByteString
findHeader hdr rsp = case lookup hdr $ H.responseHeaders rsp of
  Nothing -> fail $  "findHeader: Can't find " ++ show hdr
                  ++ " in: " ++ show (H.responseHeaders rsp)
  Just val -> return val

readHeader :: (Monad m, Read a) => HeaderName -> Response b -> m a
readHeader hdr rsp = findHeader hdr rsp >>= return . readsBS >>= \case
    [(x, [])] -> return x
    []        -> fail $ "readHeader: No result for " ++ show hdr
    _         -> fail $  "readHeader: Ambiguous results for " ++ show hdr
                      ++ " in: " ++ show (H.responseHeaders rsp)

mimeTypeContent :: Monad m => Response BL.ByteString -> m (MimeType, BL.ByteString)
mimeTypeContent rsp =
  (,) `liftM` findHeader hContentType rsp
      `ap`    return (H.responseBody rsp)

hRetryAfter :: HeaderName
hRetryAfter = "Retry-After"

maybeToList :: (a -> b) -> Maybe a -> [b]
maybeToList f = maybe [] (return . f)

showBS :: Show a => a -> ByteString
showBS = TS.encodeUtf8 . TS.pack . show

readsBS :: Read a => ByteString -> [(a, String)]
readsBS = reads . TS.unpack . TS.decodeUtf8

--------------------------------------------------------------------------------
-- Template Haskell declarations go at the end.

deriveJSON defaultOptions
  { omitNothingFields = True
  , fieldLabelModifier =
      let underscore "createdBefore" = "created_before"
          underscore "createdAfter"  = "created_after"
          underscore x               = x
          firstLower ""     = ""
          firstLower (c:cs) = toLower c : cs
      in underscore . firstLower . drop 5
  } ''DocEntriesQuery

deriveToJSON defaultOptions
  { omitNothingFields = True
  , fieldLabelModifier = map toLower . drop 6
  } ''UpdateInfo
