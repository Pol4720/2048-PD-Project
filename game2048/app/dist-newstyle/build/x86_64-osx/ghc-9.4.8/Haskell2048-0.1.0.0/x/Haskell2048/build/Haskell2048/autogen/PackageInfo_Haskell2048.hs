{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_Haskell2048 (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "Haskell2048"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Clone of the popular game 2048 in Haskell."
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/tigrennatenn/2048haskell"
