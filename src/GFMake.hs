module GFMake
  ( convertScript
  ) where

import GFMake.Internal
  ( Element (MarkupFlag)
  , padElements
  , parseLines
  , serializeElements
  , squeezeElements
  )

-- | Convert a game script from the old format to GameFAQs Markup.
convertScript :: String  -- ^ Old script content
              -> String  -- ^ Converted script
convertScript = serializeElements . (MarkupFlag :) . padElements . squeezeElements . parseLines . lines
