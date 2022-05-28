{-# Language DerivingStrategies #-}
{-# Language InstanceSigs #-}
{-# Language TypeApplications #-}

{-# Options_GHC -Wno-orphans #-}

module Main (main) where

import Data.Parsable
import Data.Proxy
import Test.QuickCheck
import Test.QuickCheck.Classes

import qualified Data.Functor.Slim.Apply as Slim
import qualified Data.Functor.Apply as Semigroupoids

instance Semigroupoids.Apply PartialParse where
    (<.>) = (Slim.<.>)

instance Arbitrary a => Arbitrary (PartialParse a) where
    arbitrary :: Gen (PartialParse a)
    arbitrary = do
        s <- listOf chooseAny
        elements [CompleteParse, PartialParse s] <*> arbitrary

main :: IO ()
main = do
    lawsCheck $ applyLaws $ Proxy @PartialParse
