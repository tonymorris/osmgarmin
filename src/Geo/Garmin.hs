module Geo.Garmin(
  time
, downloadDirectory
, buildDirectory
, distDirectory
, australiaOceaniaPbf
, mkgmap
, splitter
, getAustraliaOceania
, splitAustraliaOceania
, gmapsuppAustraliaOceania
, linkAustraliaOceania
, getMountBarney
, gmapsuppMountBarneyAustraliaOceania
, linkAustraliaOceaniaMountBarney
, commands
, linkLatest
) where

import Data.Time(UTCTime(utctDay, utctDayTime), TimeOfDay(TimeOfDay), toGregorian, timeToTimeOfDay)
import Sys.Exit(CreateProcess, procIn)
import System.FilePath((</>))

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

downloadDirectory ::
  FilePath
downloadDirectory =
  "download"

buildDirectory ::
  FilePath
buildDirectory =
  "build"

distDirectory ::
  FilePath
distDirectory =
  "dist"

australiaOceaniaPbf ::
  FilePath
australiaOceaniaPbf =
  "australia-oceania.osm.pbf"

mkgmap ::
  FilePath
mkgmap =
  "opt" </> "mkgmap" </> "mkgmap.jar"

splitter ::
  FilePath
splitter =
  "opt" </> "splitter" </> "splitter.jar"

getAustraliaOceania ::
  FilePath
  -> CreateProcess
getAustraliaOceania wd =
  procIn (wd </> downloadDirectory) "wget"
    [
      "-q"
    , "-c"
    -- http://download.geofabrik.de/australia-oceania-latest.osm.pbf
    , "http://localhost:8000/map.pbf"
    , "-O"
    , australiaOceaniaPbf
    ]
    
splitAustraliaOceania ::
  FilePath
  -> CreateProcess
splitAustraliaOceania wd =
  procIn (wd </> buildDirectory </> "australia-oceania") "java"
    [
      "-Xmx1536M"
    , "-jar"
    , splitter
    , wd </> downloadDirectory </> australiaOceaniaPbf
    , "--mapid=82345912"
    ]

gmapsuppAustraliaOceania ::
  FilePath
  -> CreateProcess
gmapsuppAustraliaOceania wd =
  procIn (wd </> buildDirectory </> "australia-oceania") "java"
    [
      "-Xmx1536M"
    , "-jar"
    , mkgmap
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
  procIn (wd </> distDirectory </> "australia-oceania") "ln"
    [
      "-s"
    , ".." </> ".." </> buildDirectory </> "australia-oceania" </> "gmapsupp.img"
    ]

getMountBarney ::
  FilePath
  -> CreateProcess    
getMountBarney wd =
  procIn (wd </> downloadDirectory) "wget"
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
  procIn (wd </> buildDirectory </> "australia-oceania_mt-barney") "java"
    [
      "-Xmx1536M"
     , "-jar"
     , mkgmap
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
     , wd </> buildDirectory </> "australia-oceania" </> "gmapsupp.img"
     , wd </> downloadDirectory </> "mt-barney-national-park.img"
     ]

linkAustraliaOceaniaMountBarney ::
  FilePath
  -> CreateProcess
linkAustraliaOceaniaMountBarney wd =
  procIn (wd </> distDirectory </> "australia-oceania_mt-barney") "ln"
    [
      "-s"
    , ".." </> ".." </> buildDirectory </> "australia-oceania_mt-barney" </> "gmapsupp.img"
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
