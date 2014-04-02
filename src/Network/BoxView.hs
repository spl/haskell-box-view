module Network.BoxView (
  ApiKey(..),
  DocId(..),
  SessionId(..),
  DocInfo(..),
  DocStatus(..),
  DocEntriesQuery(..),
  DownloadFormat(..),
  Dim(..),
  UpdateInfo(..),
  UploadRequest(..),
  SessionInfo(..),
  SessionTime(..),
  SessionTheme(..),
  AssetsInfo(..),
  uploadDoc,
  downloadDoc,
  downloadThumb,
  getDocInfo,
  getDocEntries,
  updateDocInfo,
  deleteDoc,
  createSession,
  getAssetsInfo,
  makeSessionViewUrl,
  makeSessionAssetsUrl,
) where

--------------------------------------------------------------------------------

import Control.Applicative ((<$>), (<*>), (<*), (<|>), some)
import Control.Arrow ((>>>), first)
import Control.Monad (liftM, ap, (>=>))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (Value(..), Object, ToJSON(..), FromJSON(..), (.=), (.:))
import Data.Aeson.TH (deriveJSON, deriveToJSON, defaultOptions, Options(..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Attoparsec.Text as P
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.Char (toLower, toUpper)
import Data.Default.Class (Default(..))
import qualified Data.HashMap.Strict as HM
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Monoid ((<>))
import Data.String (IsString(..))
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

-- | Box View API key.
--
-- Note: Use the 'IsString' instance (e.g. with @OverloadedStrings@) to
-- construct.
newtype ApiKey = ApiKey { fromApiKey :: ByteString }
  deriving (IsString)

instance Show ApiKey where
  show = fromByteString . fromApiKey

instance ToJSON ApiKey where
  toJSON = String . TS.decodeUtf8 . fromApiKey

instance FromJSON ApiKey where
  parseJSON = A.withText "ApiKey" $ return . ApiKey . TS.encodeUtf8

--------------------------------------------------------------------------------

-- | Unique document identifier.
--
-- Note: Use the 'IsString' instance (e.g. with @OverloadedStrings@) to
-- construct.
newtype DocId = DocId { fromDocId :: ByteString }
  deriving (Eq, IsString)

instance Show DocId where
  show = fromByteString . fromDocId

instance ToJSON DocId where
  toJSON = String . TS.decodeUtf8 . fromDocId

instance FromJSON DocId where
  parseJSON = A.withText "DocId" $ return . DocId . TS.encodeUtf8

--------------------------------------------------------------------------------

-- | Unique session identifier.
--
-- Note: Use the 'IsString' instance (e.g. with @OverloadedStrings@) to
-- construct.
newtype SessionId = SessionId { fromSessionId :: ByteString }
  deriving (Eq, IsString)

instance Show SessionId where
  show = fromByteString . fromSessionId

instance ToJSON SessionId where
  toJSON = String . TS.decodeUtf8 . fromSessionId

instance FromJSON SessionId where
  parseJSON = A.withText "SessionId" $ return . SessionId . TS.encodeUtf8

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
  { docId         :: !DocId      -- ^ Unique document identifier
  , docStatus     :: !DocStatus  -- ^ Current status of conversion
  , docName       :: !Text       -- ^ Document name
  , docCreatedAt  :: !UTCTime    -- ^ Time of upload
  }
  deriving (Eq, Show)

instance ToJSON DocInfo where
  toJSON (DocInfo {..}) = A.object
    [ "type"        .= String "document"
    , "id"          .= toJSON docId
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

-- | Internal type for 'FromJSON' instance. Used in 'getDocEntries'.
newtype DocInfoList = DocInfoList { fromDocInfoList :: [DocInfo] }

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
  deriving (Show)

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

data SessionInfo = SessionInfo
  { sessionId         :: !SessionId  -- ^ Unique session identifier
  , sessionExpiresAt  :: !UTCTime    -- ^ Time when the session expires
  }
  deriving (Eq, Show)

instance ToJSON SessionInfo where
  toJSON (SessionInfo {..}) = A.object
    [ "type"        .= String "session"
    , "id"          .= toJSON sessionId
    , "expires_at"  .= toJSON sessionExpiresAt
    ]

instance FromJSON SessionInfo where
  parseJSON = A.withObject "SessionInfo" $ \o -> do
    "session" :: Text <- o .: "type"
    SessionInfo <$> o .: "id"
                <*> o .: "expires_at"

--------------------------------------------------------------------------------

-- | Specifies when a session will expire
data SessionTime
  = SessionDefault             -- ^ Default expiration (duration: 60 minutes)
  | SessionDuration  !Int      -- ^ Duration in minutes until the session expires
  | SessionExpiresAt !UTCTime  -- ^ Time when the session expires

instance ToJSON SessionTime where
  toJSON SessionDefault        = A.object []
  toJSON (SessionDuration  n)  = A.object ["duration"   .= toJSON n]
  toJSON (SessionExpiresAt tm) = A.object ["expires_at" .= toJSON tm]

instance Default SessionTime where
  def = SessionDefault

--------------------------------------------------------------------------------

-- | Enumeration for session view theme
data SessionTheme = LightTheme | DarkTheme
  deriving (Eq, Enum, Bounded, Read, Show)

themeStyleToQuery :: SessionTheme -> Query
themeStyleToQuery ts =
  [("theme", Just $ case ts of { LightTheme -> "light"; DarkTheme  -> "dark" })]

--------------------------------------------------------------------------------

-- | This information is extrapolated from an undocumented file included in the
-- assets.
data AssetsInfo = AssetsInfo
  { assetsNumPages       :: !Int           -- ^ Number of pages in the document
  , assetsVersion        :: !Text          -- ^ Unknown
  , assetsDim            :: !Dim           -- ^ Dimensions of (most?) pages
  , assetsDimExceptions  :: !(IntMap Dim)  -- ^ Pages with exceptional dimensions
  , assetsLinks          :: !Value         -- ^ Information about (internal and
                                           -- external) links in the document
  }
  deriving (Show)

exceptionsToJSON :: IntMap Dim -> Value
exceptionsToJSON = toJSON . HM.fromList . map (first show) . IM.toList

exceptionsFromObject :: Object -> A.Parser (IntMap Dim)
exceptionsFromObject = HM.toList >>> (mapM parsePair >=> return . IM.fromList)
  where
    parsePair :: (Text, Value) -> A.Parser (Int, Dim)
    parsePair (t, v) = (read $ TS.unpack t,) <$> parseJSON v

instance ToJSON AssetsInfo where
  toJSON (AssetsInfo {..}) = A.object
    [ "numpages"      .= toJSON assetsNumPages
    , "version"       .= toJSON assetsVersion
    , "dimensions"    .= A.object
      [ "exceptions"  .= exceptionsToJSON assetsDimExceptions
      , "width"       .= width assetsDim
      , "height"      .= height assetsDim
      ]
    , "links"         .= assetsLinks
    ]

instance FromJSON AssetsInfo where
  parseJSON = A.withObject "AssetsInfo" $ \o -> do
    dimO <- o .: "dimensions"
    excO <- dimO .: "exceptions"
    AssetsInfo <$> o .: "numpages"
               <*> o .: "version"
               <*> parseJSON (Object dimO)
               <*> exceptionsFromObject excO
               <*> o .: "links"

--------------------------------------------------------------------------------
-- Exported

-- | Upload a document according to the 'UploadRequest'
uploadDoc
  :: MonadIO m
  => ApiKey
  -> UploadRequest    -- ^ Document upload request description
  -> Manager
  -> m DocInfo
uploadDoc apiKey uploadReq@(UploadRequest {..}) mgr = do
  req <- case uploadSource of
    Left _ ->
      newApiRequest apiKey "https://view-api.box.com/1/documents" >>=
      setMethod POST >>=
      setJSONBody uploadReq
    Right file ->
      newApiRequest apiKey "https://upload.view-api.box.com/1/documents" >>=
      H.formDataBody
        ( [ H.partFile "file" file
          , H.partBS "name" $ TS.encodeUtf8 uploadName
          , H.partBS "non_svg" $ TS.encodeUtf8 $ TS.toLower $ TS.pack $ show uploadNonSvg
          ]
          ++ maybeToList (H.partBS "thumbnails") (dimsToFormPart uploadThumbnails)
        )
  H.httpLbs req mgr >>= jsonContent "uploadDoc"

-- | Get a document's metadata
getDocInfo
  :: MonadIO m
  => ApiKey
  -> DocId
  -> Manager
  -> m DocInfo
getDocInfo apiKey did mgr = do
  req <- newApiRequest apiKey $ "https://view-api.box.com/1/documents/"
                              <> fromDocId did
  H.httpLbs req mgr >>= jsonContent "getDocInfo"

-- | Get a document collection according to the optional 'DocEntriesQuery'
getDocEntries
  :: MonadIO m
  => ApiKey
  -> DocEntriesQuery  -- ^ Query parameters (use 'def' for the default)
  -> Manager
  -> m [DocInfo]
getDocEntries apiKey params mgr = do
  req <- newApiRequest apiKey "https://view-api.box.com/1/documents" >>=
         setJSONBody params
  liftM fromDocInfoList $ H.httpLbs req mgr >>= jsonContent "getDocEntries"

-- | Update a document's metadata
updateDocInfo
  :: MonadIO m
  => ApiKey
  -> UpdateInfo       -- ^ Metadata to be updated (use 'def' for the default)
  -> DocId
  -> Manager
  -> m DocInfo
updateDocInfo apiKey updateInfo did mgr = do
  req <- newApiRequest apiKey ("https://view-api.box.com/1/documents/" <> fromDocId did) >>=
         setMethod PUT >>=
         setJSONBody updateInfo
  H.httpLbs req mgr >>= jsonContent "updateDocInfo"

-- | Download a document
downloadDoc
  :: MonadIO m
  => ApiKey
  -> DownloadFormat   -- ^ Download format of the file
  -> DocId
  -> Manager
  -> m (MimeType, BL.ByteString)
downloadDoc apiKey format did mgr = do
  let fileName = "content" ++ mkExt format
  req <- newApiRequest apiKey $ "https://view-api.box.com/1/documents/"
                              <> fromDocId did <> "/" <> fromString fileName
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
  => ApiKey
  -> Dim              -- ^ Requested dimensions
  -> DocId
  -> Manager
  -> m (Either Int (MimeType, BL.ByteString))
downloadThumb apiKey dim did mgr = do
  req <- newApiRequest apiKey ("https://view-api.box.com/1/documents/"
                               <> fromDocId did <> "/thumbnail") >>=
         setQuery (dimToQuery dim)
  rsp <- H.httpLbs req mgr
  case statusCode (H.responseStatus rsp) of
    200 -> Right `liftM` mimeTypeContent rsp
    202 -> Left `liftM` readHeader hRetryAfter rsp
    c   -> fail $ "downloadThumb: Unsupported HTTP status: " ++ show c

-- | Delete a document
deleteDoc
  :: MonadIO m
  => ApiKey
  -> DocId
  -> Manager
  -> m ()
deleteDoc apiKey did mgr = do
  req <- newApiRequest apiKey ("https://view-api.box.com/1/documents/" <> fromDocId did) >>=
         setMethod DELETE
  _ <- H.httpLbs req mgr
  return ()

-- | Create a session for viewing a document
--
-- Note: Sessions can only be created for documents that have a 'Done' status.
createSession
  :: MonadIO m
  => ApiKey
  -> DocId
  -> SessionTime      -- ^ Session time (use 'def' for the default)
  -> Manager
  -> m (Either Int SessionInfo)
createSession apiKey did sessionTime mgr = do
  let Object sessionObj = toJSON sessionTime
  req <- newApiRequest apiKey "https://view-api.box.com/1/sessions" >>=
         setMethod POST >>=
         setJSONBody (sessionObj <> HM.singleton "document_id" (toJSON did))
  rsp <- H.httpLbs req mgr
  case statusCode (H.responseStatus rsp) of
    201 -> Right `liftM` jsonContent "createSession" rsp
    202 -> Left `liftM` readHeader hRetryAfter rsp
    c   -> fail $ "downloadThumb: Unsupported HTTP status: " ++ show c

-- | Construct the URL for viewing a session
makeSessionViewUrl
  :: SessionId
  -> SessionTheme  -- ^ Session theme
  -> ByteString
makeSessionViewUrl sid themeStyle =
  "https://view-api.box.com/1/sessions/"
  <> fromSessionId sid <> "/view"
  <> renderQuery True (themeStyleToQuery themeStyle)

-- | Construct the URL for Viewer.js using the assets stored at Box
makeSessionAssetsUrl
  :: SessionId
  -> ByteString
makeSessionAssetsUrl sid =
  "https://view-api.box.com/1/sessions/"
  <> fromSessionId sid <> "/assets"

-- | Download the info.json file in a session's assets and extract its data
getAssetsInfo
  :: MonadIO m
  => SessionId
  -> Manager
  -> m AssetsInfo
getAssetsInfo sid mgr = do
  req <- newSessionRequest $ makeSessionAssetsUrl sid <> "/info.json"
  H.httpLbs req mgr >>= jsonContent "getAssetsInfo"

--------------------------------------------------------------------------------
-- Helpers

addHeader :: Monad m => ByteString -> ByteString -> Request -> m Request
addHeader name val req = return $
  req { H.requestHeaders = (CI.mk name, val) : H.requestHeaders req }

newApiRequest :: MonadIO m => ApiKey -> ByteString -> m Request
newApiRequest apiKey url =
  liftIO (H.parseUrl $ fromByteString url) >>=
  addHeader "Authorization" ("Token " <> fromApiKey apiKey)

newSessionRequest :: MonadIO m => ByteString -> m Request
newSessionRequest url = liftIO (H.parseUrl $ fromByteString url)

setQuery :: Monad m => Query -> Request -> m Request
setQuery q req = return $ req { H.queryString = renderQuery True q }

setJSONBody :: (Monad m, ToJSON a) => a -> Request -> m Request
setJSONBody obj req =
  return req { H.requestBody = H.RequestBodyLBS $ A.encode $ obj } >>=
  addHeader "Content-Type" contentTypeJson

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

jsonContent :: (Monad m, FromJSON a) => String -> Response BL.ByteString -> m a
jsonContent msg rsp = do
  (contentType, body) <- mimeTypeContent rsp
  if contentType == contentTypeJson then
    maybe (fail $ msg ++ ": Can't decode JSON from response: " ++ show rsp)
          return
          (A.decode' body)
  else
    fail $ msg ++ ": unknown content type: " ++ show contentType

hRetryAfter :: HeaderName
hRetryAfter = "Retry-After"

contentTypeJson :: ByteString
contentTypeJson = "application/json"

maybeToList :: (a -> b) -> Maybe a -> [b]
maybeToList f = maybe [] (return . f)

showBS :: Show a => a -> ByteString
showBS = fromString . show

fromByteString :: ByteString -> String
fromByteString = TS.unpack . TS.decodeUtf8

readsBS :: Read a => ByteString -> [(a, String)]
readsBS = reads . fromByteString

--------------------------------------------------------------------------------
-- Template Haskell declarations go at the end.

deriveJSON defaultOptions ''Dim

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
