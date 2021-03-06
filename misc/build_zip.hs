{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
import Control.Exception
import Control.Monad
import Data.String
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.Version
import qualified System.Info as SysInfo
import System.Process
import Turtle hiding (FilePath)

getGitHash :: IO (Maybe String)
getGitHash =
  liftM (Just . takeWhile (/='\n')) (readProcess "git" ["rev-parse", "--short", "HEAD"] "")
  `catch` \(_::SomeException) -> return Nothing

getVersion :: FilePath -> IO Version
getVersion cabalFile = do
  pkg <- readPackageDescription silent cabalFile
  return $ pkgVersion $ package $ packageDescription $ pkg

main :: IO ()
main = do
  gitHashMaybe <- getGitHash
  version <- getVersion "CPL.cabal"
  let suffix_githash =
        case gitHashMaybe of
          Nothing -> ""
          Just gitHash -> "_" ++ gitHash
      dir :: IsString a => a
      dir = fromString $ "CPL-" ++ showVersion version ++ suffix_githash ++ "-" ++ SysInfo.os ++ "-" ++ SysInfo.arch

  let binDir = dir </> "bin"
      samplesDir = dir </> "samples"
      zipFile :: IsString a => a
      zipFile = fromString (dir ++ ".zip")
  testfile zipFile >>= \b -> when b (rm zipFile)
  testfile dir >>= \b -> when b (rmtree dir)
  mktree dir
  mktree binDir
  mktree samplesDir
  cp ("cpl") (binDir </> "cpl")
  sh $ do
    fpath <- ls "samples"
    cp fpath (samplesDir </> filename fpath)
  cp "COPYING" (dir </> "COPYING")
  cp "README.markdown" (dir </> "README.markdown")
  cp "CHANGELOG.markdown" (dir </> "CHANGELOG.markdown")
  procs "zip" ["-r", zipFile, dir] empty
  return ()
