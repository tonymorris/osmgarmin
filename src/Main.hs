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
      mtype = Pbf (mapname' "82345912"),
      compression = None,
      source = URL "http://download.geofabrik.de/australia-oceania-latest.osm.pbf"
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
