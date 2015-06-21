module Main where

{-
import Geo.Garmin
import System.Exit
import System.Environment
import System.FilePath
-}
import System.Environment
import System.FilePath
import Data.Time
import Sys.Exit
import Data.NotZero
import Data.NotZeroOr

{-
maps ::
  [Map]
maps =
  [
    Map {
      mtype = Pbf (mapname' "82345912"),
      compression = None,
      source = URL "http://localhost:8000/map.pbf"
    }
  , Map {
      mtype = Garmin,
      compression = None,
      source = File "/home/tmorris/Desktop/Mt_Barney_National_Park.img"
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
-}

time ::
  UTCTime
  -> String
time t =
  let show2 = let s2 [x] = ['0', x]
                  s2 x = x
              in s2 . show
      (y, m, d) = toGregorian (utctDay t)
      TimeOfDay h n s = timeToTimeOfDay (utctDayTime t)
  in concat [show y, show2 m, show2 d, "-", show2 h, show2 n, show2 (floor s)]

getAustraliaOceania ::
  FilePath
  -> CreateProcess
getAustraliaOceania wd =
  procIn (wd </> "download") "wget"
    [
      "-q"
    , "-c"
    -- http://download.geofabrik.de/australia-oceania-latest.osm.pbf
    , "http://localhost:8000/map.pbf"
    , "-O"
    , "australia-oceania.osm.pbf"
    ]
    
splitAustraliaOceania ::
  FilePath
  -> CreateProcess
splitAustraliaOceania wd =
  procIn (wd </> "build" </> "australia-oceania") "java"
    [
      "-Xmx1536M"
    , "-jar"
    , "/home/tmorris/opt/splitter/splitter.jar"
    , wd </> "download" </> "australia-oceania.osm.pbf"
    , "--mapid=82345912"
    ]

gmapsuppAustraliaOceania ::
  FilePath
  -> CreateProcess
gmapsuppAustraliaOceania wd =
  procIn (wd </> "build" </> "australia-oceania") "java"
    [
      "-Xmx1536M"
    , "-jar"
    , "/home/tmorris/opt/mkgmap/mkgmap.jar"
    , "--add-pois-to-areas"
    , "--reduce-point-density-polygon=8"
    , "--remove-short-arcs"
    , "--route"
    , "--transparent"
    , "--gmapsupp"
    , "-c"
    , "template.args"
    , "--description=\"Australia and Oceania\""
    , "--country-name=\"Australia and Oceania\""
    , "--region-name=\"Australia and Oceania\""
    , "--region-abbr=AU"
    , "--country-abbr=AU"
    , "--drive-on=left"
    , "--check-roundabouts"
    ]

getMountBarney ::
  FilePath
  -> CreateProcess    
getMountBarney wd =
  procIn (wd </> "download") "wget"
    [
      "-q"
    , "-c"
    , "https://dl.dropboxusercontent.com/u/7810909/img/Mt_Barney_National_Park.img"
    , "-O"
    , "mt-barney-national-park.img"
    ]

gmapsuppMountBarneyAustraliaOceania ::
  FilePath
  -> CreateProcess
gmapsuppMountBarneyAustraliaOceania wd =
  procIn (wd </> "build" </> "australia-oceania_mt-barney") "java"
    [
      "-Xmx1536M"
     , "-jar"
     , "/home/tmorris/opt/mkgmap/mkgmap.jar"
     , "--add-pois-to-areas"
     , "--reduce-point-density-polygon=8"
     , "--remove-short-arcs"
     , "--route"
     , "--transparent"
     , "--gmapsupp"
     , "--description=\"Australia and Oceania and Mt Barney Contour\""
     , "--country-name=\"Australia and Oceania\""
     , "--country-abbr=AU"
     , "--region-name=\"Australia and Oceania\""
     , "--region-abbr=AU"
     , "--drive-on=left"
     , "--check-roundabouts"
     , wd </> "build" </> "australia-oceania" </> "gmapsupp.img"
     , wd </> "download" </> "mt-barney-national-park.img"
     ]

commands ::
  FilePath
  -> [CreateProcess]
commands =
  sequence
    [
      getAustraliaOceania
    , splitAustraliaOceania
    , gmapsuppAustraliaOceania
    , getMountBarney
    , gmapsuppMountBarneyAustraliaOceania
    ]
  

{-

refactor non-IO
copy to dist directory
copy to latest directory
move accessory functions to relevant package

-}

run2 ::
  FilePath
  -> IO ()
run2 wd =
  getCurrentTime >>= createMakeWaitProcessesExit . commands . (</>) wd . time

main2 ::
  IO ()
main2 =
  do a <- getArgs
     case a of
       [] -> putStrLn "Usage: osmgarmin <output-dir>" >> exitWith (IsNotZero (notZeroElse1 127))
       (o:_) ->  run2 o

{-


>>> /home/tmorris/Desktop/osmgarmin
wget -q -c http://localhost:8000/map.pbf -O Source-2015-06-20_12:46:08.400393_UTC.pbf
>>> /home/tmorris/Desktop/osmgarmin
java -Xmx1536M -jar /home/tmorris/opt/splitter/splitter.jar Source-2015-06-20_12:46:08.400393_UTC.pbf --mapid=82345912
>>> /home/tmorris/Desktop/osmgarmin
java -Xmx1536M -jar /home/tmorris/opt/mkgmap/mkgmap.jar --add-pois-to-areas --reduce-point-density-polygon=8 --remove-short-arcs --route --transparent --gmapsupp -c template.args --description="Australia and Oceania and Mt Barney Contour" --country-name="Australia and Oceania" --country-abbr=AU --drive-on-left
>>> /home/tmorris/Desktop/osmgarmin
java -Xmx1536M -jar /home/tmorris/opt/mkgmap/mkgmap.jar --add-pois-to-areas --reduce-point-density-polygon=8 --remove-short-arcs --route --transparent --gmapsupp --description="Australia and Oceania and Mt Barney Contour" --country-name="Australia and Oceania" --country-abbr=AU --drive-on-left Map-2015-06-20_12:46:10.180637_UTC.img /home/tmorris/Desktop/Mt_Barney_National_Park.img

----

>>> /home/tmorris/Desktop/osmgarmin
wget -q -c http://localhost:8000/map.pbf -O Source-2015-06-20_12:49:50.622964_UTC.pbf
>>> /home/tmorris/Desktop/osmgarmin
java -Xmx1536M -jar /home/tmorris/opt/mkgmap/mkgmap.jar --add-pois-to-areas --reduce-point-density-polygon=8 --remove-short-arcs --route --transparent --gmapsupp -c template.args --description="Australia and Oceania and Mt Barney Contour" --country-name="Australia and Oceania" --country-abbr=AU --drive-on-left
>>> /home/tmorris/Desktop/osmgarmin
java -Xmx1536M -jar /home/tmorris/opt/mkgmap/mkgmap.jar --add-pois-to-areas --reduce-point-density-polygon=8 --remove-short-arcs --route --transparent --gmapsupp --description="Australia and Oceania and Mt Barney Contour" --country-name="Australia and Oceania" --country-abbr=AU --drive-on-left Map-2015-06-20_13:20:14.998182_UTC.img /home/tmorris/Desktop/Mt_Barney_National_Park.img

-}