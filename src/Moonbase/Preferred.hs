{-|
Module      : Moonbase.Util.Application
Copyright   : (c) Felix Schnizlein, 2014
License     : GPL-2
Maintainer  : felix@none.io
Stability   : experimental
Portability : POSIX

Implements the preferred mechanism of moonbase

Example:

> main :: IO ()
> main = moonbase myConfig $ do
>     setTheme myTheme
>     withPreferred [ mimeImages              ==> app "gimp"
>                   , mimeVideos <> mimeAudio ==> app "vlc"
>                   , mimeSources <> mimeTxt  ==> terminal "vim" ]
>     ...

-}

{-# LANGUAGE ExistentialQuantification #-}

module Moonbase.Preferred
    ( Mimetypes(..)
    , mime
    , Preferred
    , withPreferred
    , (==>)
    , makePreferred
    , userMimeApps
    , Application(..)
    , app
    , appWith
    
    , mimeImage
    , mimeImageTypes
    , mimeImages

    , mimeAudio
    , mimeAudioTypes
    , mimeAudios

    , mimeVideo
    , mimeVideoTypes
    , mimeVideos
    
    , mimeSource
    , mimeSourceTypes
    , mimeSources

    , mimeTextHtml
    , mimeTextXml
    , mimeTextTxt

    , mimeArchives 
    , mimePdf
    , mimeTorrent
    , mimeOpenDocuments
    ) where 

import Prelude hiding (foldl)
import Control.Applicative
import Control.Monad.State

import System.Directory
import System.FilePath.Posix
import System.Environment.XDG.BaseDir
import System.Environment.XDG.DesktopEntry hiding (Application)
import System.Environment.XDG.MimeApps

import Data.Monoid
import Data.Maybe
import Data.Foldable

import qualified Data.Map as M

import Moonbase
import Moonbase.Util.Application

-- | A list of mimetypes
data Mimetypes = Mimetypes [String]

instance Monoid Mimetypes where
    mempty = Mimetypes []
    mappend (Mimetypes a) (Mimetypes b) = Mimetypes $ a ++ b


-- | Creates a Mimetypes with one element
mime :: String -> Mimetypes
mime t = Mimetypes [t]

-- | Appends the preferred types to the moonbase configuration
withPreferred :: forall a. (Executable a) => [(Mimetypes, a)] -> Moonbase ()
withPreferred prefs = modify (\rt -> rt { preferred = value })
  where
      value = if null prefs
                 then Nothing
                 else (Just $ makePreferred prefs)

-- | Generate a tuple of Mimetypes and a executable
(==>) :: forall a. (Executable a) => Mimetypes -> a -> (Mimetypes, a)
mime ==> exec = (mime, exec)

-- | Synonym for Map.fromList for preferred 
makePreferred :: forall a. (Executable a) => [(Mimetypes, a)] -> Preferred
makePreferred prefs = Preferred $ genMap prefs M.empty

-- | generates a Map.Map from a list of (mimetypes, executables) tuples
genMap :: forall a. (Executable a) => [(Mimetypes, a)] -> M.Map String a -> M.Map String a
genMap ((Mimetypes xt, exec) : xs) m = genMap xs $ addEachMime exec xt m
  where
      addEachMime exec (x:xs) m = addEachMime exec xs $ M.insert x exec m

-- | get mimeapps files
userMimeApps :: IO FilePath
userMimeApps
    = do
        dir <- getUserDataDir
        return $ dir </> "applications" </> "mimeapps.list"

-- | set preferred 
setPreferred' :: Moonbase ()
setPreferred' = do
    rt       <- get
    mimeApps <- loadMimeApps'
    case preferred rt of
         Nothing    -> return ()
         Just prefs -> do
             path     <- liftIO $ userMimeApps
             mimeApps <- update mimeApps prefs
             liftIO $ saveMimeApps path mimeApps

    where
        update :: MimeApps -> Preferred -> Moonbase MimeApps
        update apps (Preferred m) = foldlMWithKey (updateMime ) apps m

        foldlMWithKey f z = foldlM (\z' (k,v) -> f z' k v) z . M.toAscList
            


        desktopFileName :: (Executable a) => a -> Moonbase FilePath
        desktopFileName exec = do
            userDir <- liftIO $ getUserDataDir
            return $ userDir </> "applications" </> execGetName exec <.> "desktop"

        updateMime :: (Executable a) => MimeApps -> String -> a -> Moonbase MimeApps
        updateMime mimeApps mime exec = do
            entryExists <- liftIO $ findEntry (execGetName exec)

            when (isNothing entryExists) $ do
                   path <- desktopFileName exec
                   liftIO $ saveEntry $
                     newBasicApplication path (execGetName exec) (execGetPath exec)

            return $ addDefault mime (execGetName exec <.> "desktop") mimeApps

-- | load existing mimeApps file
loadMimeApps' :: Moonbase MimeApps
loadMimeApps' = do
    dir <- io getUserDataDir
    io $ createDirectoryIfMissing True (dir ++ "/applications")

    exists <- io $ doesFileExist (dir ++ "/applications/mimeapps.list")

    if exists 
       then do
           push (Info "Loading mimeapps file...")
           io (loadMimeApps $ dir ++ "/applications/mimeapps.list")
           else
           push (Info "MimeApps doesn't exists: creating newone")
           >> return newMimeApps  

-- | Create a new image/* mimetype
mimeImage :: String -> Mimetypes
mimeImage image = Mimetypes ["image/" ++ image]

-- | Common image mime names
mimeImageTypes :: [String]
mimeImageTypes = [ "png", "jpeg", "jpg", "pjpg", "svg+xml", "x-freehand", "x-icon"
                 , "x-pcx", "x-rgb", "x-tga", "x-xbitmap", "x-xpixmap"
                 , "vnd.adobe.photoshop", "tiff", "gif", "bmp" ]

-- | Common image types
mimeImages :: Mimetypes
mimeImages = foldl (\m i -> m <> mimeImage i) (Mimetypes []) mimeImageTypes


-- | Create a new video/* mimetype
mimeVideo :: String -> Mimetypes
mimeVideo video = Mimetypes ["video/" ++ video]

-- | Common video mime names
mimeVideoTypes :: [String]
mimeVideoTypes = [ "x-matroska", "x-msvideo", "webm", "mp4", "mpeg", "ogg", "h264"
                 , "x-flv", "quicktime"]

-- | Common video types
mimeVideos :: Mimetypes
mimeVideos = foldl (\m v -> m <> mimeVideo v) (Mimetypes []) mimeVideoTypes

-- | Create a new audio/* mimetype
mimeAudio :: String -> Mimetypes
mimeAudio audio = Mimetypes ["audio/" ++ audio]

-- | Common audio mime names
mimeAudioTypes = [ "x-wav", "x-ms-wma", "x-mpegurl", "x-flac", "webm", "ogg"
                 , "mpeg", "mp4", "midi" ]

-- | Common audio mime types
mimeAudios :: Mimetypes
mimeAudios = foldl (\m v -> m <> mimeAudio v) (Mimetypes []) mimeAudioTypes

-- | Create a new text/* mimetype
mimeSource :: String -> Mimetypes
mimeSource text = Mimetypes ["text/" ++ text]

-- | Some source code names (including "plain")
mimeSourceTypes :: [String]
mimeSourceTypes = [ "x-c", "x-asm", "x-java-source" , "plain", "x-pascal", "html" ]

-- | Some source code mimetypes
mimeSources :: Mimetypes
mimeSources = foldl (\m v -> m <> mimeSource v) (Mimetypes []) mimeSourceTypes


-- | Text mimetype
mimeTextTxt :: Mimetypes
mimeTextTxt  = Mimetypes ["text/plain"]

-- | Html mimetype
mimeTextHtml :: Mimetypes
mimeTextHtml = Mimetypes ["text/html"]

-- | Xml mimetype
mimeTextXml :: Mimetypes
mimeTextXml  = Mimetypes ["application/xml"]

-- | Common archive mimetypes
mimeArchives :: Mimetypes
mimeArchives = Mimetypes [ "application/x-gzip", "application/zip", 
                           "application/x-xz", "application/x-tar",
                           "application/x-rar-compressed",
                           "application/x-debian-package",
                           "application/x-cpio", "application/x-bzip",
                           "application/x-bzip2",
                           "application/vnd.android.package-archive" ]

-- | Pdf mimetype
mimePdf :: Mimetypes
mimePdf = Mimetypes ["application/pdf"]

-- | Torrent mimetype
mimeTorrent :: Mimetypes
mimeTorrent = Mimetypes ["application/x-bittorrent"]

-- | All OpenDocuments mimetypes
mimeOpenDocuments :: Mimetypes
mimeOpenDocuments = Mimetypes [ "application/vnd.oasis.opendocument.chart"
                              , "application/vnd.oasis.opendocument.chart-template"
                              , "application/vnd.oasis.opendocument.database"
                              , "application/vnd.oasis.opendocument.formula"
                              , "application/vnd.oasis.opendocument.formula-template"
                              , "application/vnd.oasis.opendocument.graphics"
                              , "application/vnd.oasis.opendocument.graphics-template"
                              , "application/vnd.oasis.opendocument.image"
                              , "application/vnd.oasis.opendocument.image-template"
                              , "application/vnd.oasis.opendocument.presentation"
                              , "application/vnd.oasis.opendocument.presentation-template"
                              , "application/vnd.oasis.opendocument.spreadsheet"
                              , "application/vnd.oasis.opendocument.spreadsheet-template"
                              , "application/vnd.oasis.opendocument.text"
                              , "application/vnd.oasis.opendocument.text-master"
                              , "application/vnd.oasis.opendocument.text-template"
                              , "application/vnd.oasis.opendocument.text-web" ]
