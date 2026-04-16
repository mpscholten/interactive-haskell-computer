{-# LANGUAGE CPP #-}
-- Template-include pattern (mirrors System.FilePath.Posix):
-- The wrapper defines macros, then #includes a template body
-- that uses those macros in regular Haskell source lines.
-- The CPP preprocessor must expand macros in non-directive lines.
#define IS_POSIX True
#define MODULE_LABEL "Posix"
#define SEPARATOR '/'
#include "body.hs"
