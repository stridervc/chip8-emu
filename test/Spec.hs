{-# LANGUAGE OverloadedStrings #-}

import Chip8

import Test.Hspec

main :: IO ()
main = do
  hspec $ do
    describe "offsetXY" $ do
      it "0 4" $ do
        offsetXY 0 4 defaultChip8 `shouldBe` 64*4

    describe "xorPutScreen" $ do
      it "0 0" $ do
        let c8 = xorPutScreen 0 0 True defaultChip8
        getScreen 0 0 c8 `shouldBe` True
      it "1 1" $ do
        let c8 = xorPutScreen 1 1 True defaultChip8
        getScreen 1 1 c8 `shouldBe` True

    describe "xorPutScreenByte" $ do
      it "0 0 11110000" $ do
        let c8 = xorPutScreenByte 0 0 0xf0 defaultChip8
        getScreen 0 0 c8 `shouldBe` True
        getScreen 1 0 c8 `shouldBe` True
        getScreen 2 0 c8 `shouldBe` True
        getScreen 3 0 c8 `shouldBe` True
        getScreen 4 0 c8 `shouldBe` False
        getScreen 5 0 c8 `shouldBe` False
        getScreen 6 0 c8 `shouldBe` False
        getScreen 7 0 c8 `shouldBe` False

      it "0 4 11110000" $ do
        let c8 = xorPutScreenByte 0 4 0xf0 defaultChip8
        getScreen 0 4 c8 `shouldBe` True
        getScreen 1 4 c8 `shouldBe` True
        getScreen 2 4 c8 `shouldBe` True
        getScreen 3 4 c8 `shouldBe` True
        getScreen 4 4 c8 `shouldBe` False
        getScreen 5 4 c8 `shouldBe` False
        getScreen 6 4 c8 `shouldBe` False
        getScreen 7 4 c8 `shouldBe` False

    describe "xorPutScreenBytes" $ do
      it "0 0" $ do
        let bs = [0xf0, 0x90, 0x90, 0x90, 0xf0]
        let (c8,collision) = xorPutScreenBytes 0 0 bs defaultChip8
        collision `shouldBe` False
        getScreen 0 0 c8 `shouldBe` True
        getScreen 1 0 c8 `shouldBe` True
        getScreen 2 0 c8 `shouldBe` True
        getScreen 3 0 c8 `shouldBe` True

