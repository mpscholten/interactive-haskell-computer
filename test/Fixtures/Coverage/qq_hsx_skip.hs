-- QuasiQuote body is opaque at parse time: an unused [hsx|…|] is
-- captured as EQuasiQuote and never evaluated, so main still runs.
-- (Expansion of a used QQ is covered by qq_toy_string.hs; full HSX
-- remains expected-fail — see examples/hsx_hello.)
main = print (length label)
  where
    label = "form-control" :: [Char]
    _unused = [hsx|<div class="form-control">{label}</div>|]
