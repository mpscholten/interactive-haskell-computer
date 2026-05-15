-- Data.ByteString.append source-loaded path.
-- Source body: append = mappend.
-- Two fixes had to land for this to dispatch correctly:
--   (1) typeTagOf for source-loaded VCons returns the constructor name
--       directly (not the bare type name via lookupCtorType), so
--       `VCon "BS" _` dispatches under "BS" — distinct from the lazy
--       ByteString's "Empty"/"Chunk" ctors that share the bare type
--       name "ByteString".
--   (2) instanceRuntimeCtors prefers lmTypeCtorReg of the instance's
--       owning module before the cross-module union, so the strict
--       Monoid ByteString's per-ctor registrations land on "BS" and
--       not on the lazy ctors (or vice versa) when the union has
--       collapsed both module's "ByteString" -> [...] entries.
import qualified Data.ByteString as BS

main :: IO ()
main = do
    -- Round-trip: append two then unpack.
    let a = BS.pack [104, 105]            -- "hi"
    let b = BS.pack [33]                  -- "!"
    let r = BS.append a b
    print (BS.length r)
    print (BS.unpack r)
    -- Empty as left arg.
    print (BS.length (BS.append BS.empty (BS.pack [88])))
    -- Empty as right arg.
    print (BS.length (BS.append (BS.pack [88]) BS.empty))
    -- Three-way via mappend chain.
    let triple = mappend (BS.pack [97]) (mappend (BS.pack [98]) (BS.pack [99]))
    print (BS.unpack triple)
