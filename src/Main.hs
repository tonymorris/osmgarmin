module Main(
  run
, main
) where

import Control.Monad.Trans.Class(MonadTrans(lift))
import Data.NotZero(notZeroElse1)
import Data.NotZeroOr(NotZeroOr(IsNotZero))
import Data.Time(getCurrentTime)
import Geo.Garmin(time, commands, linkLatest)
import System.Environment(getArgs)
import Sys.Exit(ExitCodeM, createMakeWaitProcesses, createMakeWaitProcessM, exitWith, exit)
import System.FilePath((</>))
import System.IO(hPutStrLn, stderr)

run ::
  FilePath
  -> ExitCodeM IO
run d =
  do t <- lift getCurrentTime
     let u = time t
     createMakeWaitProcesses . commands . (</>) d $ u
     createMakeWaitProcessM (linkLatest d u)

main ::
  IO ()
main =
  do a <- getArgs
     case a of
       [] -> hPutStrLn stderr "Usage: osmgarmin <output-dir>" >> exitWith (IsNotZero (notZeroElse1 127))
       (o:_) -> exit (run o)
