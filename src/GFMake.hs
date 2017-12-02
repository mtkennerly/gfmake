module GFMake
  ( convertScript
  ) where

import GFMake.Internal
  ( Element (MarkupFlag)
  , padElements
  , parseElements
  , serializeElements
  )

-- | Convert a game script from the old format to GameFAQs Markup.
convertScript :: String  -- ^ Old script content
              -> String  -- ^ Converted script
convertScript = serializeElements . (MarkupFlag :) . padElements . parseElements
