module Moonbase.UtilSpec where

import           Test.Hspec

import           Moonbase.Theme
import           Moonbase.Util

spec :: Spec
spec = do
  describe "Moonbase.Util.hex" $ do
    it "converts ff to 255" $
      hex "ff" `shouldBe` (255 :: Int)
    it "converts 00 to 0" $
      hex "00" `shouldBe` (0 :: Int)

  describe "Moonbase.Util.clamp" $ do
    it "clamps (255,255,255,255) to (1.0, 1.0, 1.0, 1.0)" $
      clamp (255,255,255,255) `shouldBe` (1.0, 1.0, 1.0, 1.0)
    it "clamps (0,0,0,0) to (0.0, 0.0, 0.0, 0.0)" $
      clamp (0,0,0,0) `shouldBe` (0.0, 0.0, 0.0, 0.0)

  describe "Moonbase.Util.parseColorTuple" $ do
    it "parses `#ffffffff` to (255,255,255,255)" $
      parseColorTuple "#ffffffff" `shouldBe` (255,255,255,255)
    it "parses `#ffffff` to (255,255,255,255)" $
      parseColorTuple "#ffffff" `shouldBe` (255,255,255,255)
    it "parses `#00f` to (0,0,0,255,255)" $
      parseColorTuple "#00f" `shouldBe` (0,0,255,255)
    it "parses `#000f` to (0,0,0,0,255)" $
      parseColorTuple "#000f" `shouldBe` (0,0,0,255)
    it "parses `#xxxxxx` to parseColorTuple defaultColor" $
      parseColorTuple "#xxxxxx" `shouldBe` parseColorTuple defaultColor
    it "parses `` to parseColorTuple defaultColor" $
      parseColorTuple "" `shouldBe` parseColorTuple defaultColor

  describe "Moonbase.Util."
