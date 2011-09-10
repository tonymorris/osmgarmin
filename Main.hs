module Main where

import Geo.Garmin
import System.Exit
import System.Directory
import System.Environment
import System.FilePath
import Data.Time.Clock
import Data.Time.Calendar

maps ::
  [Map]
maps =
  [
    Map {
      mtype = OpenStreetMap (mapname' "82345912"),
      compression = Bzip,
      source = URL "http://download.geofabrik.de/osm/australia-oceania.osm.bz2"
    }
  , 
    Map {
      mtype = Garmin,
      compression = None,
      source = URL "http://projects.tmorris.net/public/maps/scratch/Mount%20Barney/Brek/Mt_Barney_National_Park_Contours_Streams_etc.img"
    }
  ]

main ::
  IO ()
main =
  do a <- getArgs
     case a of
       [] -> putStrLn "Usage: osmgarmin <output-dir>" >> exitWith (ExitFailure 107)
       (o:_) ->  resolveMaps maps (\p e -> if e == ExitSuccess
                                             then
                                               do UTCTime d' t' <- getCurrentTime
                                                  let d = o </> showGregorian d' </> show t'
                                                  mkdir d
                                                  copyFile p (d </> takeFileName p)
                                             else
                                               print e)
