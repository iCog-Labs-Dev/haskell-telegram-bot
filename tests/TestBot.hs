{-# LANGUAGE OverloadedStrings #-}

module TestBot where

import Bot
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Test.HUnit

data ArgumentTestData = ArgumentTestData
  { atdMessage :: String,
    atdResult :: Either BotError Bool,
    atdData :: Map.Map String Aeson.Value
  }

sampleBot :: Bot
sampleBot =
  Bot
    { botHost = "example.com",
      botTokenPrefix = "Bearer",
      botToken = "1234567890"
    }

sampleBuilderBot :: BotBuilder
sampleBuilderBot = BotBuilder {builderToken = "1234567890"}

botTest :: Test
botTest =
  TestList
    [ TestCase $
        assertEqual
          "Test constructor correctness"
          sampleBot
          (Bot "example.com" "Bearer" "1234567890"),
      -- Test accessor function correctness
      TestCase $
        assertEqual
          "Test botHost accessor correctness"
          "example.com"
          (botHost sampleBot),
      TestCase $
        assertEqual
          "Test botTokenPrefix accessor correctness"
          "Bearer"
          (botTokenPrefix sampleBot),
      TestCase $
        assertEqual
          "Test botToken accessor correctness"
          "1234567890"
          (botToken sampleBot)
    ]

-- Test case for the BotBuilder datatype
botBuilderTest :: Test
botBuilderTest =
  TestList
    [ TestCase $
        assertEqual
          "BotBuilder constructor should create a BotBuilder value with the expected token"
          sampleBuilderBot
          (BotBuilder "1234567890"),
      TestCase $
        assertEqual
          "builderToken accessor should return the correct token"
          "1234567890"
          (builderToken sampleBuilderBot)
    ]

initBotTest :: Test
initBotTest =
  TestCase
    ( assertEqual
        "initBot should return a minimal BotBuilder"
        initBot
        (BotBuilder "")
    )

withTokenTest :: Test
withTokenTest =
  TestCase
    ( assertEqual
        "withToken should return a BotBuilder with an updated token"
        (withToken "987654321" sampleBuilderBot)
        (BotBuilder "987654321")
    )

argValidatorTest :: Test
argValidatorTest =
  let checks =
        Map.fromList
          [ ("required", (True, Aeson.Null)),
            ("required2", (True, Aeson.String "hello")),
            ("optional", (False, Aeson.Number 12)),
            ("optional2", (False, Aeson.Array Vector.empty)),
            ("optional3", (False, Aeson.Bool True))
          ]
      arg1 =
        Map.fromList
          [ ("required", Aeson.Null),
            ("required2", Aeson.String "what up"),
            ("optional", Aeson.Number 323),
            ("optional2", Aeson.Array (Vector.fromList [Aeson.Null])),
            ("optional3", Aeson.Bool False)
          ]
      arg2 =
        Map.fromList
          [ ("required2", Aeson.String ""),
            ("optional", Aeson.Number 10),
            ("optional2", Aeson.Array Vector.empty),
            ("optional3", Aeson.Bool False)
          ]
      arg3 =
        Map.fromList
          [ ("required", Aeson.String ""),
            ("required2", Aeson.Number 0),
            ("optional", Aeson.Null),
            ("optional2", Aeson.Bool True),
            ("optional3", Aeson.Array Vector.empty)
          ]
   in TestList
        [ TestCase
            ( assertBool
                "should return Right True"
                ( case argValidator arg1 checks of
                    Right True -> True
                    _ -> False
                )
            ),
          TestCase
            ( assertBool
                "should return Left ArgumentRequiredError"
                ( case argValidator arg2 checks of
                    Left (ArgumentRequiredError _) -> True
                    _ -> False
                )
            ),
          TestCase
            ( assertBool
                "should return Left ArgumentTypeError"
                ( case argValidator arg3 checks of
                    Left (ArgumentTypeError _) -> True
                    _ -> False
                )
            )
        ]

botModuleTest :: Test
botModuleTest =
  TestList
    [ TestLabel "botDataTypeTest" botTest,
      TestLabel "botBuilderDataTypeTest" botBuilderTest,
      TestLabel "initBotTest" initBotTest,
      TestLabel "withTokenTest" withTokenTest,
      TestLabel "argValidatorTest" argValidatorTest
    ]
