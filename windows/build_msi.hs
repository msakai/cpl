{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception
import Control.Monad
import Data.Version
import Distribution.Package
import Distribution.PackageDescription
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec
import Distribution.Pretty
#else
import Distribution.PackageDescription.Parse
#endif
import Distribution.Verbosity
import qualified System.Info as SysInfo
import System.Process

getGitHash :: IO (Maybe String)
getGitHash =
  liftM (Just . takeWhile (/='\n')) (readProcess "git" ["rev-parse", "--short", "HEAD"] "")
  `catch` \(_::SomeException) -> return Nothing

getVersion :: FilePath -> IO Version
getVersion cabalFile = do
  pkg <- readGenericPackageDescription silent cabalFile
  return $ pkgVersion $ package $ packageDescription $ pkg

main :: IO ()
main = do
  gitHashMaybe <- getGitHash
  version <- getVersion "../CPL.cabal"
  let suffix_githash =
        case gitHashMaybe of
          Nothing -> ""
          Just gitHash -> "_" ++ gitHash
#if MIN_VERSION_Cabal(2,2,0)
      msiFileName = "CPL-" ++ prettyShow version ++ suffix_githash ++ "-" ++ SysInfo.os ++ "-" ++ SysInfo.arch ++ ".msi"
#else
      msiFileName = "CPL-" ++ showVersion version ++ suffix_githash ++ "-" ++ SysInfo.os ++ "-" ++ SysInfo.arch ++ ".msi"
#endif
      arch =
        case SysInfo.arch of
          "x86_64" -> "x64"
          "i386" -> "x86"
          s -> s
  callProcess "candle" $ ["-arch", arch, "CPL.wxs"]
  callProcess "light" ["-ext", "WixUIExtension", "-out", msiFileName, "CPL.wixobj"]
  return ()
