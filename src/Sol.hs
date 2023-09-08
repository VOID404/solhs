module Sol (
    SolPack,
    SolPack1,
    SolType (..),
    encode,
) where

import Data.Text (Text)
import Sol.Encode (enc)
import Sol.ToAST (SolPack (pack), SolPack1)
import Sol.Types (SolType (..))

encode :: (SolPack a) => SolType -> a -> Maybe Text
encode t v = enc <$> pack t v