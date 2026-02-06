{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP, ScopedTypeVariables, OverloadedStrings #-}
import Control.Exception
import Control.Monad
import Data.String
import Data.Version (Version, makeVersion, showVersion)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
#if MIN_VERSION_Cabal(3,8,0)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
#endif
import qualified Distribution.Types.Version as Cabal
import Distribution.Verbosity
import qualified System.Info as SysInfo
import System.Process
import Turtle hiding (FilePath)

getGitHash :: IO (Maybe String)
getGitHash =
  liftM (Just . takeWhile (/='\n')) (readProcess "git" ["rev-parse", "--short", "HEAD"] "")
  `catch` \(_::SomeException) -> return Nothing

getVersion :: FilePath -> IO Version
getVersion cabalFile = do
  pkg <- readGenericPackageDescription silent cabalFile
  let cabalVersion = pkgVersion $ package $ packageDescription $ pkg
  return $ makeVersion $ Cabal.versionNumbers cabalVersion

main :: IO ()
main = do
  gitHashMaybe <- getGitHash
  version <- getVersion "../CPL.cabal"
  let suffix_githash =
        case gitHashMaybe of
          Nothing -> ""
          Just gitHash -> "_" ++ gitHash
      dir :: IsString a => a
      dir = fromString $ "CPL-" ++ showVersion version ++ suffix_githash ++ "-" ++ SysInfo.os ++ "-" ++ SysInfo.arch

  let binDir = dir </> "bin"
      zipFile :: IsString a => a
      zipFile = fromString (dir ++ ".zip")
  testfile zipFile >>= \b -> when b (rm zipFile)
  testfile dir >>= \b -> when b (rmtree dir)

  mktree dir
  mktree binDir
  cp ("../cpl.exe") (binDir </> "cpl.exe")
  cp "../COPYING" (dir </> "COPYING")
  cp "../README.md" (dir </> "README.md")
  cp "../CHANGELOG.md" (dir </> "CHANGELOG.md")
  cp "../TUTORIAL.md" (dir </> "TUTORIAL.md")
  cp "../TUTORIAL_ja.md" (dir </> "TUTORIAL_ja.md")
  cptree "../images" (dir </> "images")
  cptree "../samples" (dir </> "samples")
  mktree (dir </> "doc-images")
  sh $ do
    fname <- ls "../doc-images"
    when (extension fname == Just "png") $
      cp fname (dir </> "doc-images" </> filename fname)
  mktree (dir </> "web")
  cp "../web/README.md" (dir </> "web" </> "README.md")

  procs "7z" ["a", "-r", zipFile, dir] empty
  return ()
