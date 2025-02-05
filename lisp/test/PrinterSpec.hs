{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- PrinterSpec
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module PrinterSpec (spec) where

import Data.Either (rights)
import Data.Text (Text)
import Printer (
    Color (..),
    ColorCode (..),
    Config (..),
    Paint (..),
    Style (..),
    confDefaultValues,
    confFilepath,
    getColorCode,
    parseColor',
    parseColors,
    parseConf,
    parseRGB,
    reset,
 )
import Test.Hspec (Spec, context, describe, it, shouldBe, shouldNotBe)
import Test.Hspec.Megaparsec (shouldParse)

newtype TestPaint = TestPaint Int

instance Paint TestPaint where
    paint :: Bool -> TestPaint -> String
    paint False (TestPaint i) = show i
    paint True (TestPaint i) = show Red ++ show i ++ reset

testConf :: Text
testConf = "warnings=\"255;0;255\"\nerrors=\"255;0;0\"\ninfos=\"0;0;255\""

spec :: Spec
spec = do
    describe "printing with color or style" $ do
        context "red color" $ do
            it "should be the red ansii code" $ do
                let red = Red
                show red `shouldBe` "\x1b[31m"

        context "green color" $ do
            it "should be the green ansii code" $ do
                let green = Green
                show green `shouldBe` "\x1b[32m"

        context "orange color" $ do
            it "should be the orange ansii code" $ do
                let orange = Orange
                show orange `shouldBe` "\x1b[38;2;255;128;0m"

        context "blue color" $ do
            it "should be the blue ansii code" $ do
                let blue = Blue
                show blue `shouldBe` "\x1b[34m"

        context "yellow color" $ do
            it "should be the yellow ansii code" $ do
                let yellow = Yellow
                show yellow `shouldBe` "\x1b[33m"

        context "magenta color" $ do
            it "should be the magenta ansii code" $ do
                let magenta = Magenta
                show magenta `shouldBe` "\x1b[35m"

        context "cyan color" $ do
            it "should be the cyan ansii code" $ do
                let cyan = Cyan
                show cyan `shouldBe` "\x1b[36m"

        context "white color" $ do
            it "should be the white ansii code" $ do
                let white = White
                show white `shouldBe` "\x1b[37m"

        context "underline style" $ do
            it "should be the underline ansii code" $ do
                let underline = Underlined
                show underline `shouldBe` "\x1b[4m"

        context "bold style" $ do
            it "should be the bold ansii code" $ do
                let bold = Bold
                show bold `shouldBe` "\x1b[1m"

        context "reset function should reset it" $ do
            it "should be the reset ansii code" $ do
                reset `shouldBe` "\x1b[0m"

        context "test if painter is not colored" $ do
            it "should return the number" $ do
                let testPaint = TestPaint 42
                paint False testPaint `shouldBe` "42"

        context "test if painter is colored" $ do
            it "should return the number with red color" $ do
                let testPaint = TestPaint 42
                paint True testPaint `shouldBe` "\x1b[31m42\x1b[0m"

        context "conf-file-path-const" $ do
            it "should be the conf file path" $ do
                confFilepath `shouldBe` "lisp-colors.conf"

        context "conf-default-values-const" $ do
            it "should be the default values" $ do
                confDefaultValues
                    `shouldBe` "warnings=\"255;0;255\"\nerrors=\"255;0;0\"\ninfos=\"0;0;255\""

        context "show color" $ do
            it "should show the color" $ do
                show (RGB (ColorCode (255, 128, 0))) `shouldBe` "\x1b[38;2;255;128;0m"

        context "get color code red" $ do
            it "should show the color code" $ do
                getColorCode Red `shouldBe` ColorCode (255, 0, 0)

        context "get color code green" $ do
            it "should show the color code" $ do
                getColorCode Green `shouldBe` ColorCode (0, 255, 0)

        context "get color code blue" $ do
            it "should show the color code" $ do
                getColorCode Blue `shouldBe` ColorCode (0, 0, 255)

        context "get color code yellow" $ do
            it "should show the color code" $ do
                getColorCode Yellow `shouldBe` ColorCode (255, 255, 0)

        context "get color code magenta" $ do
            it "should show the color code" $ do
                getColorCode Magenta `shouldBe` ColorCode (255, 0, 255)

        context "get color code cyan" $ do
            it "should show the color code" $ do
                getColorCode Cyan `shouldBe` ColorCode (0, 255, 255)

        context "get color code white" $ do
            it "should show the color code" $ do
                getColorCode White `shouldBe` ColorCode (255, 255, 255)

        context "get color code orange" $ do
            it "should show the color code" $ do
                getColorCode Orange `shouldBe` ColorCode (255, 128, 0)

        context "get RGB code" $ do
            it "should show the color code" $ do
                getColorCode (RGB (ColorCode (255, 128, 0))) `shouldBe` ColorCode (255, 128, 0)

        context "parse RGB" $ do
            it "parse RGB" $ do
                parseRGB "127" "255" "233" `shouldBe` RGB (ColorCode (127, 255, 233))

        context "parse conf" $ do
            it "parse good conf" $ do
                rights (parseConf testConf)
                    `shouldBe` [ Config "warnings" "255;0;255",
                                 Config "errors" "255;0;0",
                                 Config "infos" "0;0;255"
                               ]
            it "parse bad conf" $ do
                parseConf "caca" `shouldNotBe` []

        context "parse colors from config" $ do
            it "should work with basic colors" $ do
                parseColors
                    [ Config "warnings" "255;0;255",
                      Config "errors" "255;0;0",
                      Config "infos" "0;0;255"
                    ]
                    `shouldBe` Just
                        ( RGB (ColorCode (255, 0, 255)),
                          RGB (ColorCode (255, 0, 0)),
                          RGB (ColorCode (0, 0, 255))
                        )
            it "should fail when missing colors" $ do
                parseColors
                    [ Config "warnings" "255;0;255",
                      Config "errors" "255;0;0"
                    ]
                    `shouldBe` Nothing

            it "should fail on bad color" $ do
                parseColors
                    [ Config "warnings" "255;0;255",
                      Config "errors" "255;0;0",
                      Config "infos" "0;a;255;"
                    ]
                    `shouldBe` Nothing

            it "should parse color artinatively" $ do
                parseColor' "255;0;255" `shouldBe` Just (RGB (ColorCode (255, 0, 255)))

            it "should fail on bad color" $ do
                parseColor' "255;0;255;" `shouldBe` Nothing
