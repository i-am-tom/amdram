{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}

module Toolbox.GenericSpec where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Test.Hspec (Spec, it, shouldBe)
import Toolbox.Generic qualified as Generic

type User :: Type
data User = User {name :: String, age :: Int, likesDogs :: Bool}
  deriving stock (Eq, Generic, Show)

type Coord :: Type
data Coord = Coord {x :: Float, y :: Float, z :: Float}
  deriving stock (Eq, Generic, Show)

spec :: Spec
spec = do
  let user :: User
      user = User {name = "Tom", age = 30, likesDogs = True}

  it "itraverse" do
    let f :: String -> x -> ([String], x)
        f k v = (pure k, v)

    Generic.itraverse @Show f user
      `shouldBe` (["name", "age", "likesDogs"], user)

  it "traverse" do
    let f :: (Show x) => x -> ([String], x)
        f v = (pure (show v), v)

    Generic.traverse @Show f user
      `shouldBe` (["\"Tom\"", "30", "True"], user)

  it "ifoldMap" do
    let f :: (Show x) => String -> x -> [String]
        f "age" v = pure (show v)
        f _____ _ = mempty

    Generic.ifoldMap @Show f user `shouldBe` ["30"]

  it "foldMap" do
    let f :: (Show x) => x -> [String]
        f = pure . show

    Generic.foldMap @Show f user
      `shouldBe` ["\"Tom\"", "30", "True"]

  let coord :: Coord
      coord = Coord {x = 1, y = 2, z = 3}

  it "imap" do
    let f :: String -> Float -> Float
        f s x =
          x + fromIntegral do
            sum (map fromEnum s)

    Generic.imap @((~) Float) f coord
      `shouldBe` Coord 121 123 125

  it "map" do
    Generic.map @((~) Float) (+ 1) coord
      `shouldBe` Coord 2 3 4
