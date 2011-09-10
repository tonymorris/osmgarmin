module Main where

import Geo.Garmin
import System.Exit
import System.Directory
import System.FilePath
import Data.Time.Clock
import Data.Time.Calendar
import Control.Applicative

maps :: [Map]
maps = [Map {
  mtype = OpenStreetMap (mapname' "82345912"),
  compression = Bzip,
  source = URL "http://download.geofabrik.de/osm/australia-oceania.osm.bz2"
}, Map {
  mtype = Garmin,
  compression = None,
  source = URL "http://projects.tmorris.net/public/maps/scratch/Mount%20Barney/Brek/Mt_Barney_National_Park_Contours_Streams_etc.img"
}]

main :: IO ()
main = resolveMaps maps (\p e -> if e == ExitSuccess
                                   then do d <- ("/mnt/sdb/public/map/Garmin/" ++) . showGregorian . utctDay <$> getCurrentTime
                                           mkdir d
                                           mapM_ (\z -> copyFile z (d </> takeFileName z)) p
                                   else print e)
