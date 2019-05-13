module GFMakeSpec
  ( spec
  ) where

import Test.Hspec
  ( Spec
  , describe
  , it
  , shouldBe
  )

import GFMake
  ( convertScript
  )

shouldBeMarkup actual expected = actual `shouldBe` (";format:gf-markup\n\n" ++ expected)

h2 = unlines
  [ "#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#"
  , "#$#$#$#$#$#$#$#$#$#$                                       $#$#$#$#$#$#$#$#$#$#"
  , "#$#$#   [01'00]   ---              Header 2               ---   [01'00]   #$#$#"
  , "#$#$#$#$#$#$#$#$#$#$                                       $#$#$#$#$#$#$#$#$#$#"
  , "#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#"
  ]

h3 = unlines
  [ "#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#"
  , "#$#$#$#$#$#$#$                               $#$#$#$#$#$#$#"
  , "#$# [01'01] ---           Header 3          --- [01'01] #$#"
  , "#$#$#$#$#$#$#$                               $#$#$#$#$#$#$#"
  , "#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#"
  ]

h3Multi = unlines
  [ "#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#"
  , "#$#$#$#$#$#$#$                               $#$#$#$#$#$#$#"
  , "#$# [01'02] ---          Header 3:          --- [01'02] #$#"
  , "#$#$#                 Multiple Lines                  #$#$#"
  , "#$#$#$#$#$#$#$                               $#$#$#$#$#$#$#"
  , "#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#"
  ]

h3b = unlines
  [ "___________________________________________________________"
  , "-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-"
  , "==========================================================="
  , "                     _===============_"
  , "                    \\_  Header 3b  _/"
  , "                      ==============="
  ]

h4 = unlines
  [ "#$#$#$#$#$#$#$#$"
  , "$#  Header 4  #$"
  , "#$#$#$#$#$#$#$#$"
  ]

h4Anno = unlines
  [ "#$#$#$#$#$#  (high)"
  , "$#  H4A  #$  (mid)"
  , "#$#$#$#$#$#  (low)"
  ]

h4AnnoHigh = unlines
  [ "#$#$#$#$#$#  (high)"
  , "$#  H4A  #$"
  , "#$#$#$#$#$#"
  ]

h4AnnoMid = unlines
  [ "#$#$#$#$#$#"
  , "$#  H4A  #$  (mid)"
  , "#$#$#$#$#$#"
  ]

h4AnnoLow = unlines
  [ "#$#$#$#$#$#"
  , "$#  H4A  #$"
  , "#$#$#$#$#$#  (low)"
  ]

h5 = "\n   [ Header 5 ]"

spec :: Spec
spec =
  describe "convertScript" $ do
    it "converts narration with one line" $
      convertScript "* - Hello." `shouldBeMarkup` "Hello."
    it "converts narration with multiple lines" $
      convertScript "* - Hello\n    there." `shouldBeMarkup` "Hello there."
    it "converts narration with empty lines" $
      convertScript "* - Hello.\n\n    Bye." `shouldBeMarkup` "Hello. Bye."
    it "converts narration with colons" $
      convertScript "* - Hello: there." `shouldBeMarkup` "Hello: there."
    it "converts narration before speech" $
      convertScript "* - N.\nS         : W." `shouldBeMarkup` "N.\n\n| S | W. |"
    it "converts narration before conditional speech" $
      convertScript "* - N.\nS         :{1} W." `shouldBeMarkup` "N.\n\n|-1 S | 1 | W. |"

    it "converts speech with one line" $
      convertScript "Foo       : Bar." `shouldBeMarkup` "| Foo | Bar. |"
    it "converts speech with multiple lines" $
      convertScript "Foo       : Bar.\n            Baz." `shouldBeMarkup` "| Foo | Bar. Baz. |"
    it "converts speech with empty lines" $
      convertScript "Foo       : Bar.\n\n            Baz." `shouldBeMarkup` "| Foo | Bar. Baz. |"
    it "converts speech with colons" $
      convertScript "Foo       : ::A:B:::\n            ::C." `shouldBeMarkup` "| Foo | ::A:B::: ::C. |"
    it "converts speech with nested elements all on one line" $
      convertScript "Foo       : Bar.\n            * - Example.\n            Baz."
        `shouldBeMarkup` "| Foo | Bar. * - Example. Baz. |"
    it "converts speech before narration" $
      convertScript "S         : W.\n* - N." `shouldBeMarkup` "| S | W. |\n\nN."
    it "converts conditional speech before narration" $
      convertScript "S         :{1} W.\n* - N." `shouldBeMarkup` "|-1 S | 1 | W. |\n\nN."

    it "converts speech with one condition" $
      convertScript "S         :{A} Foo." `shouldBeMarkup` "|-1 S | A | Foo. |"
    it "converts speech with one condition containing braces" $
      convertScript "S         :{A} F{o}o." `shouldBeMarkup` "|-1 S | A | F{o}o. |"
    it "converts speech with one condition over multiple lines" $
      convertScript "S         :{A} Foo,\n               bar." `shouldBeMarkup` "|-1 S | A | Foo, bar. |"
    it "converts speech with one condition and a special marker" $
      convertScript "S         :*{A} Foo." `shouldBeMarkup` "|-1 S | *A | Foo. |"
    it "converts speech with many conditions" $
      convertScript "S         :{A} Foo.\n           {B} Bar." `shouldBeMarkup` "|-2 S | A | Foo. |\n|   B | Bar. |"
    it "converts speech with many conditions over multiple lines" $ do
      let
        input = unlines
          [ "Condition :{A} Case A."
          , "           {B} Case B,"
          , "               which contains multiple lines."
          , "          *{C} Case C."
          ]
        output = init $ unlines
          [ "|-3 Condition | A | Case A. |"
          , "|   B | Case B, which contains multiple lines. |"
          , "|   *C | Case C. |"
          ]
      convertScript input `shouldBeMarkup` output
    it "converts speech with 10 conditions using hexadecimal" $ do
      let
        input = unlines
          [ "Condition :{1} 1."
          , "           {2} 2."
          , "           {3} 3."
          , "           {4} 4."
          , "           {5} 5."
          , "           {6} 6."
          , "           {7} 7."
          , "           {8} 8."
          , "           {9} 9."
          , "           {10} 10."
          ]
        output = init $ unlines
          [ "|-a Condition | 1 | 1. |"
          , "|   2 | 2. |"
          , "|   3 | 3. |"
          , "|   4 | 4. |"
          , "|   5 | 5. |"
          , "|   6 | 6. |"
          , "|   7 | 7. |"
          , "|   8 | 8. |"
          , "|   9 | 9. |"
          , "|   10 | 10. |"
          ]
      convertScript input `shouldBeMarkup` output
    it "converts speech with 16 conditions by spliting into multiple elements" $ do
      let
        input = unlines
          [ "Condition :{1} 1."
          , "           {2} 2."
          , "           {3} 3."
          , "           {4} 4."
          , "           {5} 5."
          , "           {6} 6."
          , "           {7} 7."
          , "           {8} 8."
          , "           {9} 9."
          , "           {10} 10."
          , "           {11} 11."
          , "           {12} 12."
          , "           {13} 13."
          , "           {14} 14."
          , "           {15} 15."
          , "           {16} 16."
          ]
        output = init $ unlines
          [ "|-f Condition | 1 | 1. |"
          , "|   2 | 2. |"
          , "|   3 | 3. |"
          , "|   4 | 4. |"
          , "|   5 | 5. |"
          , "|   6 | 6. |"
          , "|   7 | 7. |"
          , "|   8 | 8. |"
          , "|   9 | 9. |"
          , "|   10 | 10. |"
          , "|   11 | 11. |"
          , "|   12 | 12. |"
          , "|   13 | 13. |"
          , "|   14 | 14. |"
          , "|   15 | 15. |"
          , "|-1 [cont] | 16 | 16. |"
          ]
      convertScript input `shouldBeMarkup` output

    it "converts speech of various kinds together" $ do
      let
        input = unlines
          [ "Simple    : Just one line."
          , ""
          , "Multiple  : This speech contains"
          , "            multiple lines."
          , ""
          , "            It also contains empty lines."
          , ""
          , "Condition :{A} Case A."
          , "           {B} Case B,"
          , "               which contains multiple lines."
          , "           {C} Case C."
          , ""
          , "Simple 2  : One more line."
          ]
        output = init $ unlines
          [ "| Simple | Just one line. |"
          , "| Multiple | This speech contains multiple lines. It also contains empty lines. |"
          , "|-3 Condition | A | Case A. |"
          , "|   B | Case B, which contains multiple lines. |"
          , "|   C | Case C. |"
          , "| Simple 2 | One more line. |"
          ]
      convertScript input `shouldBeMarkup` output

    it "converts h2" $
      convertScript h2 `shouldBeMarkup` "==Header 2=="
    it "converts h2 before a non-header element" $
      convertScript (h2 ++ "\n\n* - Example.") `shouldBeMarkup` "==Header 2==\nExample."
    it "converts h2 after a non-header element" $
      convertScript ("* - Example.\n\n" ++ h2) `shouldBeMarkup` "Example.\n\n==Header 2=="

    it "converts h3" $
      convertScript h3 `shouldBeMarkup` "===Header 3==="
    it "converts h3 before a non-header element" $
      convertScript (h3 ++ "\n\n* - Example.") `shouldBeMarkup` "===Header 3===\nExample."
    it "converts h3 after a non-header element" $
      convertScript ("* - Example.\n\n" ++ h3) `shouldBeMarkup` "Example.\n\n===Header 3==="
    it "converts h3 with multiple lines" $
      convertScript h3Multi `shouldBeMarkup` "===Header 3: Multiple Lines==="
    it "converts h3 with multiple lines before a non-header element" $
      convertScript (h3Multi ++ "\n\n* - Example.") `shouldBeMarkup` "===Header 3: Multiple Lines===\nExample."
    it "converts h3 with multiple lines after a non-header element" $
      convertScript ("* - Example.\n\n" ++ h3Multi) `shouldBeMarkup` "Example.\n\n===Header 3: Multiple Lines==="

    it "converts h3b" $
      convertScript h3b `shouldBeMarkup` "===++Header 3b++==="
    it "converts h3b before a non-header element" $
      convertScript (h3b ++ "\n\n* - Example.") `shouldBeMarkup` "===++Header 3b++===\nExample."
    it "converts h3b after a non-header element" $
      convertScript ("* - Example.\n\n" ++ h3b) `shouldBeMarkup` "Example.\n\n===++Header 3b++==="

    it "converts h4" $
      convertScript h4 `shouldBeMarkup` "====Header 4===="
    it "converts h4 before a non-header element" $
      convertScript (h4 ++ "\n\n* - Example.") `shouldBeMarkup` "====Header 4====\nExample."
    it "converts h4 after a non-header element" $
      convertScript ("* - Example.\n\n" ++ h4) `shouldBeMarkup` "Example.\n\n====Header 4===="
    it "converts h4 with annotations on all three lines" $
      convertScript h4Anno `shouldBeMarkup` "====H4A | (mid) / (high) / (low)===="
    it "converts h4 with an annotation on the top line" $
      convertScript h4AnnoHigh `shouldBeMarkup` "====H4A | (high)===="
    it "converts h4 with an annotation on the middle line" $
      convertScript h4AnnoMid `shouldBeMarkup` "====H4A | (mid)===="
    it "converts h4 with an annotation on the bottom line" $
      convertScript h4AnnoLow `shouldBeMarkup` "====H4A | (low)===="

    it "converts h5" $
      convertScript h5 `shouldBeMarkup` "=====Header 5====="
    it "converts h5 before a non-header element" $
      convertScript (h5 ++ "\n\n* - Example.") `shouldBeMarkup` "=====Header 5=====\nExample."
    it "converts h5 after a non-header element" $
      convertScript ("* - Example.\n\n" ++ h5) `shouldBeMarkup` "Example.\n\n=====Header 5====="

    it "converts option blocks" $ do
      let
        input = unlines
          [ "-----------------------------------------------------------"
          , "     -(1a)"
          , "A         : Foo."
          , ""
          , "======(12a)"
          , "B         : Bar."
          , ""
          , "     -(1b)"
          , "C         : Baz."
          , "-----------------------------------------------------------"
          ]
        output = init $ unlines
          [ "%"
          , "*1. 1a"
          , "| A | Foo. |"
          , ""
          , "*12. 12a"
          , "| B | Bar. |"
          , ""
          , "*1. 1b"
          , "| C | Baz. |"
          , "%"
          ]
      convertScript input `shouldBeMarkup` output
    it "converts option delimiters" $
      convertScript (replicate 59 '-') `shouldBeMarkup` "%"
    it "converts options at level 1" $
      convertScript "     -(A)" `shouldBeMarkup` "*1. A"
    it "converts options at level 2" $
      convertScript "    --(A)" `shouldBeMarkup` "*2. A"
    it "converts options at level 3" $
      convertScript "   ---(A)" `shouldBeMarkup` "*3. A"
    it "converts options at level 4" $
      convertScript "  ----(A)" `shouldBeMarkup` "*4. A"
    it "converts options at level 5" $
      convertScript " -----(A)" `shouldBeMarkup` "*5. A"
    it "converts options at level 6" $
      convertScript "------(A)" `shouldBeMarkup` "*6. A"
    it "converts options at level 7" $
      convertScript "-----=(A)" `shouldBeMarkup` "*7. A"
    it "converts options at level 8" $
      convertScript "----==(A)" `shouldBeMarkup` "*8. A"
    it "converts options at level 9" $
      convertScript "---===(A)" `shouldBeMarkup` "*9. A"
    it "converts options at level 10" $
      convertScript "--====(A)" `shouldBeMarkup` "*10. A"
    it "converts options at level 11" $
      convertScript "-=====(A)" `shouldBeMarkup` "*11. A"
    it "converts options at level 12" $
      convertScript "======(A)" `shouldBeMarkup` "*12. A"
    it "converts parenthetical option phrases with inner braces" $
      convertScript "     -({Con{tains} {{ braces)" `shouldBeMarkup` "*1. {Con{tains} {{ braces"
    it "converts brace option phrases with inner parentheses" $
      convertScript "     -{(Con(tains) (( parentheses}" `shouldBeMarkup` "*1. (Con(tains) (( parentheses"

    it "converts comments" $
      convertScript "\nOne.\nTwo.\n\nThree.\n" `shouldBeMarkup` "; One.\n; Two.\n; Three."
    it "converts comments between other elements" $
      convertScript "* - One.\nTwo.\n* - Three." `shouldBeMarkup` "One.\n; Two.\nThree."
    it "trims would-be comments through the last special character" $
      convertScript "* - One.\nWould-be\n* - Two." `shouldBeMarkup` "One.\n; be\nTwo."

    it "handles empty files" $
      convertScript "" `shouldBe` ";format:gf-markup\n"
