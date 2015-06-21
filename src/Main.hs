module Main where

import Control.Monad.Trans.Class
import System.Environment
import System.FilePath
import Data.Time
import Sys.Exit
import Data.NotZero
import Data.NotZeroOr

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

linkAustraliaOceania ::
  FilePath
  -> CreateProcess
linkAustraliaOceania wd =
  procIn (wd </> "dist" </> "australia-oceania") "ln"
    [
      "-s"
    , ".." </> ".." </> "build" </> "australia-oceania" </> "gmapsupp.img"
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

linkAustraliaOceaniaMountBarney ::
  FilePath
  -> CreateProcess
linkAustraliaOceaniaMountBarney wd =
  procIn (wd </> "dist" </> "australia-oceania_mt-barney") "ln"
    [
      "-s"
    , ".." </> ".." </> "build" </> "australia-oceania_mt-barney" </> "gmapsupp.img"
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
    , linkAustraliaOceania
    , getMountBarney
    , gmapsuppMountBarneyAustraliaOceania
    , linkAustraliaOceaniaMountBarney
    ]
  

{-

refactor non-IO
mkgmap, splitter

-}


linkLatest ::
  FilePath
  -> String
  -> CreateProcess
linkLatest d t =
  procIn d "ln"
    [
      "-f"
    , "-s"
    , "-n"
    , t
    , "latest"
    ]

run2 ::
  FilePath
  -> ExitCodeM IO
run2 d =
  do t <- lift getCurrentTime
     let u = time t
     createMakeWaitProcesses . commands . (</>) d $ u
     createMakeWaitProcessM (linkLatest d u)

main2 ::
  IO ()
main2 =
  do a <- getArgs
     case a of
       [] -> putStrLn "Usage: osmgarmin <output-dir>" >> exitWith (IsNotZero (notZeroElse1 127))
       (o:_) -> exit (run2 o)
       