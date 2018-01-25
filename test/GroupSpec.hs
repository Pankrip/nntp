module GroupSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances
import Article
import Group
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Data.Dates
    
main :: IO ()
main = hspec spec
    
spec :: Spec
spec = do
    describe "Group" $ do
        describe "mkNewGroup" $ do
            it "creates a new group in the system" $ do
                mkNewGroup "net.all" dummy1 `shouldBe` dummy2
        describe "addArtToGrp" $ do
            it "adds a new article to a particular group" $ do
                pending

dummy1 :: DateTime
dummy1 = DateTime {year=2018, month=1, day=4, hour=12, minute=39, second=15}

dummy2 :: Group
dummy2 = Group 
         {
            name = "net.all",
            articles = [],
            creationDT = dummy1
         }