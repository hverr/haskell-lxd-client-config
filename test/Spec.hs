{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Text (Text, pack)
import qualified Data.Map.Strict as M
import qualified Data.Yaml as Yaml

import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck

import System.LXD.Client.Config

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    testProperty "encode/decode" prop_encode_decode
  , testCase "decode file" case_decode_file
  ]

prop_encode_decode :: Config -> Bool
prop_encode_decode c = Yaml.decode (Yaml.encode c) == Just c

case_decode_file :: Assertion
case_decode_file = do
    y <- parseFile fp
    assertEqual "should parse correctly" cfg y
  where
    fp = "data/example-config.yml"

    cfg = Config {
        configDefaultRemote = "local"
      , configRemotes = rems
      , configAliases = M.empty
      }

    rems = M.fromList [
        ("images", Remote "https://images.linuxcontainers.org" True (Just "simplestreams"))
      , ("local", Remote "unix://" False Nothing)
      , ("local-https", Remote "https://127.0.0.1:8443" False Nothing)
      , ("ubuntu", Remote "https://cloud-images.ubuntu.com/releases" True (Just "simplestreams"))
      , ("ubuntu-daily", Remote "https://cloud-images.ubuntu.com/daily" True (Just "simplestreams"))
      ]

instance Arbitrary Config where
    arbitrary = Config <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Remote where
    arbitrary = Remote <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Text where
    arbitrary = pack <$> arbitrary
