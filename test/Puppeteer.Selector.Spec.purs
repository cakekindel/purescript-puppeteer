module Puppeteer.Selector.Spec where

import Prelude

import Data.Foldable (fold)
import Data.Identity (Identity)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Puppeteer.Selector as S
import Test.Spec (SpecT, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (test)
import Web.HTML (HTMLButtonElement)

spec :: SpecT Aff Unit Identity Unit
spec = describe "Selector" do
  test "toCSS" do
    let isButton = identity :: forall s. S.Selector s HTMLButtonElement => s -> s
    let
      s = S.toCSS
        $ isButton
        $
          S.button
            `S.hasId` "foo"
            `S.hasClass` "bar"
            `S.hasAttr` "disabled"
            `S.hasAttrContaining` ("ident" /\ "abc")
            `S.hasAttrListContaining` ("feet" /\ "left_foot")
            `S.hasAttrStartsWith` ("name" /\ "frank")
            `S.hasAttrEndsWith` ("name" /\ "johnson")
            `S.isDescendantOf` S.body
            `S.isChildOf` S.html
            `S.not` (S.enabled S.none)
            `S.has` (S.div `S.isChildOf` S.none)
            # S.focus
            # S.disabled
            # S.active
    let
      expected = fold
        [ "html > body button"
        , "#foo.bar"
        , """[disabled]"""
        , """[ident *= "abc"]"""
        , """[feet ~= "left_foot"]"""
        , """[name ^= "frank"]"""
        , """[name $= "johnson"]"""
        , ":not(:enabled)"
        , ":has( > div)"
        , ":focus"
        , ":disabled"
        , ":active"
        ]
    s `shouldEqual` expected
