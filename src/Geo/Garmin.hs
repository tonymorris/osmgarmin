module Geo.Garmin(
  time
, downloadDirectory
, buildDirectory
, distDirectory
, mkgmap
, splitter
, australiaOceaniaPbf
, getAustraliaOceania
, getAustraliaOceania'
, splitAustraliaOceania
, splitAustraliaOceania'
, gmapsuppAustraliaOceania
, gmapsuppAustraliaOceania'
, linkAustraliaOceania
, linkAustraliaOceania'
, getMountBarney
, getMountBarney'
, gmapsuppMountBarneyAustraliaOceania
, gmapsuppMountBarneyAustraliaOceania'
, linkAustraliaOceaniaMountBarney
, linkAustraliaOceaniaMountBarney'
, commands
, linkLatest
, Parameters(..)
, ReadParameters(..)
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

mkgmap ::
  FilePath
mkgmap =
  "opt" </> "mkgmap" </> "mkgmap.jar"
  
splitter ::
  FilePath
splitter =
  "opt" </> "splitter" </> "splitter.jar"

australiaOceaniaPbf ::
  FilePath
australiaOceaniaPbf =
  "australia-oceania.osm.pbf"

getAustraliaOceania ::
  ReadParameters CreateProcess
getAustraliaOceania =
  ReadParameters (\(Parameters w _ _) -> getAustraliaOceania' w)

getAustraliaOceania' ::
  FilePath
  -> CreateProcess
getAustraliaOceania' wd =
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
  ReadParameters CreateProcess
splitAustraliaOceania =
  ReadParameters (\(Parameters w _ s) -> splitAustraliaOceania' w s)

splitAustraliaOceania' ::
  FilePath
  -> FilePath
  -> CreateProcess
splitAustraliaOceania' wd s =
  procIn (wd </> buildDirectory </> "australia-oceania") "java"
    [
      "-Xmx1536M"
    , "-jar"
    , s
    , wd </> downloadDirectory </> australiaOceaniaPbf
    , "--mapid=82345912"
    ]

gmapsuppAustraliaOceania ::
  ReadParameters CreateProcess
gmapsuppAustraliaOceania =
  ReadParameters (\(Parameters w m _) -> gmapsuppAustraliaOceania' w m)

gmapsuppAustraliaOceania' ::
  FilePath
  -> FilePath
  -> CreateProcess
gmapsuppAustraliaOceania' wd m =
  procIn (wd </> buildDirectory </> "australia-oceania") "java"
    [
      "-Xmx1536M"
    , "-jar"
    , m
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
  ReadParameters CreateProcess
linkAustraliaOceania =
  ReadParameters (\(Parameters w _ _) -> linkAustraliaOceania' w)

linkAustraliaOceania' ::
  FilePath
  -> CreateProcess
linkAustraliaOceania' wd =
  procIn (wd </> distDirectory </> "australia-oceania") "ln"
    [
      "-s"
    , ".." </> ".." </> buildDirectory </> "australia-oceania" </> "gmapsupp.img"
    ]

getMountBarney ::
  ReadParameters CreateProcess
getMountBarney =
  ReadParameters (\(Parameters w _ _) -> getMountBarney' w)

getMountBarney' ::
  FilePath
  -> CreateProcess    
getMountBarney' wd =
  procIn (wd </> downloadDirectory) "wget"
    [
      "-q"
    , "-c"
    , "https://dl.dropboxusercontent.com/u/7810909/img/Mt_Barney_National_Park.img"
    , "-O"
    , "mt-barney-national-park.img"
    ]

gmapsuppMountBarneyAustraliaOceania ::
  ReadParameters CreateProcess
gmapsuppMountBarneyAustraliaOceania =
  ReadParameters (\(Parameters w m _) -> gmapsuppMountBarneyAustraliaOceania' w m)

gmapsuppMountBarneyAustraliaOceania' ::
  FilePath
  -> FilePath
  -> CreateProcess
gmapsuppMountBarneyAustraliaOceania' wd m =
  procIn (wd </> buildDirectory </> "australia-oceania_mt-barney") "java"
    [
      "-Xmx1536M"
     , "-jar"
     , m
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
  ReadParameters CreateProcess
linkAustraliaOceaniaMountBarney =
  ReadParameters (\(Parameters w _ _) -> linkAustraliaOceaniaMountBarney' w)

linkAustraliaOceaniaMountBarney' ::
  FilePath
  -> CreateProcess
linkAustraliaOceaniaMountBarney' wd =
  procIn (wd </> distDirectory </> "australia-oceania_mt-barney") "ln"
    [
      "-s"
    , ".." </> ".." </> buildDirectory </> "australia-oceania_mt-barney" </> "gmapsupp.img"
    ]

commands ::
  ReadParameters [CreateProcess]
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

----

data Parameters =
  Parameters
    FilePath -- working directory
    FilePath -- mkgmap
    FilePath -- splitter
  deriving (Eq, Ord, Show)

newtype ReadParameters a =
  ReadParameters {
    (~>.) ::
      Parameters -> a
  }

instance Functor ReadParameters where
  fmap f (ReadParameters g) =
    ReadParameters (f . g)

instance Applicative ReadParameters where
  pure =
    ReadParameters . const
  ReadParameters f <*> ReadParameters a =
    ReadParameters (f <*> a)

instance Monad ReadParameters where
  return =
    pure
  ReadParameters r >>= f =
    ReadParameters (\x -> f (r x) ~>. x)
