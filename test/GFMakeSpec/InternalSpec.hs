module GFMakeSpec.InternalSpec
  ( spec
  ) where

import Test.Hspec
  ( Spec
  , describe
  , it
  , shouldBe
  )

import GFMake.Internal
  ( Element (..)
  , constructOptionSets
  )

spec :: Spec
spec =
  describe "constructOptionSets" $ do
    it "doesn't do anything without OptionDelim" $
      constructOptionSets [NoOp, NoOp] `shouldBe` [NoOp, NoOp]
    it "handles single-level sets" $
      constructOptionSets
        [ OptionDelim
        , Option 1 "Yes"
        , Narration "Yay!"
        , Option 1 "No"
        , Narration "Aww..."
        , OptionDelim
        ]
      `shouldBe`
        [ OptionSet "Yes" [Narration "Yay!"]
        , OptionSet "No" [Narration "Aww..."]
        ]
    it "handles nested sets with sequential levels" $
      constructOptionSets
        [ OptionDelim
        , Option 1 "Yes"
        , Narration "Yay!"
        , Option 1 "No"
        , Narration "Please?"
        , Option 2 "Fine"
        , Narration "Yay!!"
        , Option 2 "Nope"
        , Narration "Boo!"
        , OptionDelim
        ]
      `shouldBe`
        [ OptionSet "Yes" [Narration "Yay!"]
        , OptionSet "No"
          [ Narration "Please?"
          , OptionSet "Fine" [Narration "Yay!!"]
          , OptionSet "Nope" [Narration "Boo!"]
          ]
        ]
    it "handles nested sets with non-sequential levels" $
      constructOptionSets
        [ OptionDelim
        , Option 1 "A"
        , Narration "A?"
        , Option 4 "B"
        , Narration "B?"
        , Option 12 "C"
        , Narration "C."
        , OptionDelim
        ]
      `shouldBe`
        [ OptionSet "A"
          [ Narration "A?"
          , OptionSet "B"
            [ Narration "B?"
            , OptionSet "C"
              [ Narration "C."
              ]
            ]
          ]
        ]
    it "handles nested sets beyond the normal max level 12" $
      constructOptionSets
        [ OptionDelim
        , Option 1 "A"
        , Narration "A?"
        , Option 20 "B"
        , Narration "B."
        , OptionDelim
        ]
      `shouldBe`
        [ OptionSet "A"
          [ Narration "A?"
          , OptionSet "B"
            [ Narration "B."
            ]
          ]
        ]
