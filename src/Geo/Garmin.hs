module Geo.Garmin where

import Data.Time
import Data.Char
import Data.List
import System.Cmd
import System.Exit
import System.Directory
import System.FilePath
import Control.Monad
import Control.Applicative
import Control.Exception

newtype MapName = MapName {
  mid :: String
} deriving (Eq, Ord, Show)

mapname' ::
  String
  -> MapName
mapname' s =
  if length s == 8 && all isDigit s
    then MapName s
    else error ("Invalid mapname: " ++ s)

incrementMapName ::
  MapName
  -> MapName
incrementMapName =
  mapname' . show . flip (((+) :: Integer -> Integer -> Integer) . read . mid) 1

data MapType =
  OpenStreetMap {
    mapname :: MapName
  } | Garmin -- todo Polish
  deriving (Eq, Show)

foldMapType ::
  (MapName -> a)
  -> a
  -> MapType
  -> a
foldMapType o _ (OpenStreetMap i) =
  o i
foldMapType _ g Garmin =
  g

extension ::
  Compression
  -> MapType
  -> String
extension c =
  let com None = id
      com Bzip = (++ ".bz2")
      com Gzip = (++ ".gz")
      com Zip  = (++ ".zip")
  in com c . foldMapType (const ".osm") ".img"

extension' ::
  Map
  -> String
extension' =
  extension <$> compression <*> mtype

data Compression =
  None
  | Bzip
  | Gzip
  | Zip
  deriving (Eq, Show)

data Source =
  File {
    file :: FilePath
  } |
  URL {
    url :: String
  }
  deriving (Eq, Show)

foldSource ::
  (FilePath -> a)
  -> (String -> a)
  -> Source
  -> a
foldSource f _ (File f') =
  f f'
foldSource _ u (URL u') =
  u u'

data Map =
  Map {
    mtype :: MapType,
    compression :: Compression,
    source :: Source
  }
  deriving (Eq, Show)

findM ::
  Monad m =>
  (a -> m Bool)
  -> [a]
  -> m (Maybe a)
findM _ [] =  
  return Nothing
findM p (x:xs) =
  p x >>= \z -> if z
                  then return (Just x)
                  else findM p xs

loop_ ::
  Monad m =>
  (a -> m Bool)
  -> (a -> m b)
  -> [a]
  -> m ()
loop_ _ _ [] =
  return ();
loop_ p a  (x:xs) =
  p x >>= flip when (a x >> loop_ p a xs)

work ::
  IO FilePath
work =
  do t <- getTemporaryDirectory
     let d = t </> "build.garmin"
     mkdir d
     return d

exists ::
  FilePath
  -> IO Bool
exists d =
  (||) <$> doesFileExist d <*> doesDirectoryExist d

system' ::
  String
  -> String
  -> IO ExitCode
system' c s =
  print (c ++ ' ' : s) >>
  system (c ++ ' ' : s)

bzip ::
  String
  -> IO ExitCode
bzip =
  system' "bzip2"

wget ::
  String
  -> IO ExitCode
wget =
  system' "wget"

java ::
  String
  -> IO ExitCode
java =
  system' "java -Xmx1536M"

splitter ::
  String -> IO ExitCode
splitter s =
  java ("-jar /opt/splitter/splitter.jar " ++ s)

mkgmap ::
  String
  -> IO ExitCode
mkgmap s =
  java ("-jar /opt/mkgmap/mkgmap.jar " ++  s)

-- | Change to the given directory, then execute the given action, then change back to the original directory.
chdir ::
  FilePath -- ^ The directory to change to.
  -> IO a  -- ^ The action to execute in the given directory.
  -> IO a  -- ^ The result of executing the given action.
chdir d a =
  bracket getCurrentDirectory setCurrentDirectory (\_ -> setCurrentDirectory d >> a)

rm ::
  FilePath -> IO ()
rm =
  (>>=) <$> doesFileExist <*> flip when . removeFile

mkdir ::
  FilePath
  -> IO ()
mkdir =
  createDirectoryIfMissing True

chdirw ::
  IO a
  -> IO a
chdirw =
  (work >>=) . flip chdir

withWorkFile ::
  String
  -> String
  -> (FilePath -> IO a)
  -> IO FilePath
withWorkFile pre post f =
  do _ <- work
     t <- getCurrentTime
     let k = pre ++ ((\c -> if isSpace c then '_' else c) <$> show t) ++ post
     _ <- f k
     return k

resolveSource ::
  Map
  -> IO FilePath
resolveSource m =
  foldSource return (\u -> withWorkFile "Source-" (extension' m) (\f -> wget ("-c " ++ u ++ " -O " ++ f))) $ source m

decompress ::
  Map
  -> FilePath
  -> IO FilePath
decompress m p =
  case compression m of
    None -> return p
    Bzip -> withWorkFile "Decompress-" (extension None (mtype m)) (\z -> bzip ("-dc " ++ p ++ " > " ++ z) >> return z)
    _    -> error "todo" -- todo other compression

cleanImg ::
  IO ()
cleanImg =
  mapM_ rm ["63240000.img", "63240000.tdb", "areas.list", "template.args", "osmmap.img", "osmmap.tdb"]

resolveMapType ::
  MapType
  -> FilePath
  -> IO FilePath
resolveMapType (OpenStreetMap i) f =
  chdirw (do _ <- splitter (f ++ " --cache --mapid=" ++ mid i)
             _ <- mkgmap ("--route --latin1 --net --code-page=1252 --transparent --gmapsupp -c template.args")
             loop_
               ((or <$>) . mapM doesFileExist)
               (mapM_ rm)
               ((\s -> [s ++ ".osm.gz", s ++ ".img"]) . mid <$> iterate incrementMapName i)
             cleanImg
             withWorkFile "Map-" ".img" (renameFile "gmapsupp.img"))
resolveMapType Garmin f =
  return f

resolveMap ::
  Map
  -> IO (FilePath, FilePath)
resolveMap m =
  do s <- resolveSource m 
     d <- decompress m s
     p <- resolveMapType (mtype m) d
     unless (s == d) (rm d)
     return (p, s)

resolveMaps ::
  [Map]
  -> ([FilePath] -> ExitCode -> IO a)
  -> IO a
resolveMaps m f =
  chdirw (do p <- mapM resolveMap m
             e <- mkgmap (("--route --latin1 --net --code-page=1252 --transparent --gmapsupp ") ++ intercalate " " (map fst p))
             cleanImg
             mapM_ (\(a, b) -> unless (a == b) (rm a)) p
             z <- getCurrentDirectory
             f ((z </>) <$> "gmapsupp.img" : map snd p) e)

