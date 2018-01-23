module StorageSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances
import Article
import Group
import Storage
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Data.Dates
    
main :: IO ()
main = hspec spec
    
spec :: Spec
spec = describe "Storage" $ do
    describe "updateGroups" $ do
        it "updates the groups according to a new article" $ do
            pending

