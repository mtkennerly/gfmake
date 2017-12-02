# gfmake

gfmake is a tool written in Haskell to convert the game scripts that I
transcribed from their original format into GameFAQS Markup.

## Usage

From the console:

```
stack build
stack exec -- gfmake --help
stack exec -- gfmake sample1.txt

stack install
gfmake --help
```

From code:

```haskell
module Main where

import GFMake (convertScript)

main :: IO ()
main = readFile "sample1.txt" >>= (return . convertScript) >>= putStrLn
```

## Versioning

This project uses Semantic Versioning, *not* PVP. The public API comprises the
command line interface and all exports from non-`Internal` modules under src.
Exporting new names is not considered a breaking change, so use unqualified
imports at your discretion.

## Sample

This is the output for sample1.txt:

```
;format:gf-markup

; This is a freeform comment without any special characters.

==Header 2==
===Header 3===
===Header 3: Multiple Lines===
===++Header 3b++===
====Header 4====
=====Header 5=====
This is original narration for events. If it is long enough, then it is indented after the hyphen. There may also be multiple paragraphs of narration.

| Speaker | This is text or speech directly from the game. The speaker ID is always padded with spaces to at least 10 characters. This may also have multiple lines. |
| Long Name Here | If the speaker ID is longer than 10 characters, then the indentation is simply shifted over. |
| Narrated | * - Narration can occur inside of a speaker block. Speech occurs at normal indentation. * - Narration is indented an extra 2 spaces. |

====Header 4 Annotated | (an annotation will be present) / (in some rare cases) / (middle line is default, but all three are possible)====
| Condition | {A} This is said under condition A. {B} This is said under condition B. * - It may include narration as well. *{C} Idiosyncratic marker. |
%
*1. Option 1
| Speaker | This is the response to choosing one option. |

*2. Option 1.1
| Speaker | This option is available if you choose Option 1. |

*6. Six
| Speaker | The hyphens continue increasing... |

*7. Seven
| Speaker | ...until they wrap around to an equal sign. |

*12. Twelve
| Speaker | The maximum depth is 12 nested options. |

*1. Option 2
| Speaker | This is the alternative to Option 1. |
%
```

## To-do

* The plus-formatting for h3b headers doesn't have any significance in GameFAQs
  Markup and is just to highlight them for manual cleanup, since there aren't
  enough header levels for them to work very well. It would be better to
  perform that cleanup automatically.

* Nested narration, conditions, and options inside of a speech block should
  be formatted specially, whereas now they end up all on one line in a jumble.

For nesting inside non-conditional speech, elements could simply be
interspersed around said speech.

```
# Current:
| Narrated | * - Narration can occur inside of a speaker block. Speech occurs at normal indentation. * - Narration is indented an extra 2 spaces. |

# Future:
Narration can occur inside of a speaker block.
| Narrated | Speech occurs at normal indentation. |
Narration is indented an extra 2 spaces.
```

Conditional speech could be split into an additional table cell, but there is
a complication when a condition contains its own nesting, since the GameFAQs
Markup format does not support multiple lines within a cell.

```
# Current:
| Condition | {A} This is said under condition A. {B} This is said under condition B. * - It may include narration as well. *{C} Idiosyncratic marker. |

# Future?
| Condition | {A} | This is said under condition A. |
|| {B} | This is said under condition B. |
It may include narration as well.
|| *{C} | Idiosyncratic marker.
```

Care would be necessary to avoid introducing ambiguity in the conversion.
