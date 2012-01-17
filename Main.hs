module Main where

import Geo.Garmin
import System.Exit
import System.Directory
import System.Environment
import System.FilePath

maps ::
  [Map]
maps =
  [
    Map {
      mtype = OpenStreetMap (mapname' "82345912"),
      compression = Bzip,
      source = URL "http://localhost/osm/australia-oceania.osm.bz2"
    }
  , 
    Map {
      mtype = Garmin,
      compression = None,
      source = URL "http://localhost/osm/Brek/Mt_Barney_National_Park_Contours_Streams_etc.img"
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
                                               do mkdir o 
                                                  copyFile p (o </> takeFileName p)
                                             else
                                               print e)
