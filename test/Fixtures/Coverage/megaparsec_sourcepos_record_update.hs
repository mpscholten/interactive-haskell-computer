-- Record update of megaparsec SourcePos. parseHsx's setPosition is
--   state { statePosState = (statePosState state) { pstateSourcePos } }
-- and used to hang in exportedFieldRegistryForNames walking every
-- `module Text.Megaparsec.*` re-export before the local State fields.
import Text.Megaparsec (initialPos, sourceName)

main :: IO ()
main = do
  let p = initialPos "x"
      p' = p { sourceName = "y" }
  putStrLn (sourceName p')
