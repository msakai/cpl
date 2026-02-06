{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP, ScopedTypeVariables, OverloadedStrings #-}
import Control.Exception
import Control.Monad
import Data.String
import Data.Version (Version, makeVersion, showVersion)
import Distribution.Package
import Distribution.PackageDescription
#if MIN_VERSION_Cabal(3,8,0)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
#else
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
#endif
import qualified Distribution.Types.Version as Cabal
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Utils.Path (makeSymbolicPath)
#endif
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
#if MIN_VERSION_Cabal(3,14,0)
  pkg <- readGenericPackageDescription silent Nothing (makeSymbolicPath cabalFile)
#else
  pkg <- readGenericPackageDescription silent cabalFile
#endif
  let cabalVersion = pkgVersion $ package $ packageDescription $ pkg
  return $ makeVersion $ Cabal.versionNumbers cabalVersion

isWindows :: Bool
isWindows = SysInfo.os == "mingw32"

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
      zipFile :: IsString a => a
      zipFile = fromString (dir ++ ".zip")
      exeName = if isWindows then "cpl.exe" else "cpl"
  testfile zipFile >>= \b -> when b (rm zipFile)
  testfile dir >>= \b -> when b (rmtree dir)

  mktree dir
  mktree binDir
  cp (fromString exeName) (binDir </> fromString exeName)
  cp "COPYING" (dir </> "COPYING")
  cp "README.md" (dir </> "README.md")
  cp "CHANGELOG.md" (dir </> "CHANGELOG.md")
  cp "TUTORIAL.md" (dir </> "TUTORIAL.md")
  cp "TUTORIAL_ja.md" (dir </> "TUTORIAL_ja.md")
  cptree "images" (dir </> "images")
  cptree "samples" (dir </> "samples")
  mktree (dir </> "doc-images")
  sh $ do
    fname <- ls "doc-images"
    when (extension fname == Just "png") $
      cp fname (dir </> fname)
  mktree (dir </> "web")
  cp "web/README.md" (dir </> "web" </> "README.md")

  if isWindows
    then procs "7z" ["a", "-r", zipFile, dir] empty
    else procs "zip" ["-r", zipFile, dir] empty
  return ()
