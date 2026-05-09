-- Regression: a pattern synonym must remain visible across a chain of
-- module re-exports that use the @T(..)@ wildcard form.
--
-- Surfaced by warp_hello: warp's @Network.Wai.Handler.Warp.Imports@
-- module does
--
--   import Data.ByteString.Internal (ByteString (..))
--   module Imports (ByteString (..)) where
--
-- and bytestring's @Data.ByteString.Internal@ exports
-- @ByteString (BS, PS)@.  Before this fix, 'resolveImport'\\'s cheap
-- @specAllows@ check silently dropped any import whose spec carried
-- only a @\"$dotdot:T\"@ sentinel — meaning a bare @PS@ lookup from
-- warp's @parseRequestLine@ never reached @Data.ByteString.Internal@
-- (let alone @Data.ByteString.Internal.Type@), failing with
-- @unbound variable PS@ at runtime.
import Modules.PatsynXmodReexport.Imports

main :: IO ()
main = case Pair 100 5 8 of
    BS a b -> putStrLn ("BS " ++ show a ++ " " ++ show b)
