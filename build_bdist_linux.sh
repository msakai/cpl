#!/bin/bash
export CABALVER=1.22
export GHCVER=7.10.2

sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update

sudo apt-get install cabal-install-$CABALVER ghc-$GHCVER
export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:~/.cabal/bin:$PATH
sudo apt-get install happy-1.19.4 alex-3.1.3
export PATH=/opt/alex/3.1.3/bin:/opt/happy/1.19.4/bin:$PATH

cabal sandbox init
cabal update
cabal install --only-dependencies -f-Haskeline -f-Readline
cabal configure -fLinuxStatic -f-Haskeline -f-Readline
cabal build

VER=`ghc -e ":m + Control.Monad Distribution.Package Distribution.PackageDescription Distribution.PackageDescription.Parsec Distribution.Verbosity Data.Version Distribution.Pretty" -e 'putStrLn =<< liftM (prettyShow . pkgVersion . package . packageDescription) (readGenericPackageDescription silent "CPL.cabal")'`
OS=`ghc -e ":m +System.Info" -e "putStrLn os"`
ARCH=`ghc -e ":m +System.Info" -e "putStrLn arch"`

PKG=CPL-${VER}-${OS}-${ARCH}

rm -r $PKG
mkdir -p $PKG/bin
cp dist/build/cpl/cpl $PKG/bin
cp CHANGELOG.markdown COPYING README.markdown $PKG/
cp -a samples $PKG
tar Jcf $PKG.tar.xz $PKG --owner=sakai --group=sakai
