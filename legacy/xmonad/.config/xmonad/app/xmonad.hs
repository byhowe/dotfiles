module Main (main) where

import Data.Version (showVersion)
import Graphics.X11.Xinerama (compiledWithXinerama)
import Paths_xmonad_config (version)
import System.Environment (getProgName, getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.Info (compilerName, compilerVersion, arch, os)
import Text.Printf (printf)
import XMonad ( launch, Directories'(..), getDirectories )

import Config (config)

main :: IO ()
main = getArgs >>= cli >> exitSuccess

cli :: [String] -> IO ()
cli [help] | help `elem` ["help", "-h", "--help"] = printUsage
cli [ver] | ver `elem` ["version", "-v", "--version"] = printVersion
cli [info] | info `elem` ["info", "-vv", "--info"] = printInfo
cli [dirs] | dirs `elem` ["dirs", "--dirs"] = printDirectories
cli run | run `elem` [["run"], []] = getDirectories >>= launch config
cli a = do
  putStrLn . printf "Unrecognized command(s): %s\n" $ unwords a
  printUsage
  exitFailure

printUsage :: IO ()
printUsage = do
  self <- getProgName
  putStr $ printf
      "Usage: %s [COMMAND]\n\
      \Commands:\n\
      \  help       Print this message\n\
      \  version    Print the version number\n\
      \  info       Print the verbose version information\n\
      \  dirs       Print the directory information\n\
      \  run        Run xmonad\n"
      self

printVersion :: IO ()
printVersion = putStrLn . printf "xmonad %s" $ showVersion version

printInfo :: IO ()
printInfo =
  putStr $
    printf
      "xmonad (byron howe) %s compiled by %s %s for %s-%s\n\
      \Xinerama: %s\n"
      (showVersion version)
      compilerName
      (showVersion compilerVersion)
      arch
      os
      (show compiledWithXinerama)

printDirectories :: IO ()
printDirectories = do
  dirs <- getDirectories
  putStr $
    printf
      "Config Dir: %s\n\
      \Data Dir:   %s\n\
      \Cache Dir:  %s\n"
      (cfgDir dirs)
      (dataDir dirs)
      (cacheDir dirs)
