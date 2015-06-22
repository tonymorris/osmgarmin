module Main(
  run
, main
) where

import Control.Monad.Trans.Class(MonadTrans(lift))
import Data.NotZero(notZeroElse1)
import Data.NotZeroOr(NotZeroOr(IsNotZero))
import Data.Time(getCurrentTime)
import Geo.Garmin(Parameters(Parameters), ReadParameters((~>.)), time, commands, linkLatest, mkgmap, splitter)
import System.Environment(getArgs)
import Sys.Exit(ExitCodeM, createMakeWaitProcesses, createMakeWaitProcessM, exitWith, exit)
import System.FilePath((</>))
import System.Directory
import System.IO(hPutStrLn, stderr)

run ::
  FilePath
  -> ExitCodeM IO
run d =
  do t <- lift getCurrentTime
     c <- lift getCurrentDirectory
     let u = time t
     let p = Parameters
               (d </> u)
               (c </> mkgmap)
               (c </> splitter)
     createMakeWaitProcesses (commands ~>. p)
     createMakeWaitProcessM (linkLatest d u)
     
main ::
  IO ()
main =
  do a <- getArgs
     case a of
       [] -> hPutStrLn stderr "Usage: osmgarmin <output-dir>" >> exitWith (IsNotZero (notZeroElse1 127))
       (o:_) -> exit (run o)
