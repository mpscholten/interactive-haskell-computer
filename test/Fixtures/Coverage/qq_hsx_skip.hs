-- QuasiQuote skipping: an unexpanded [hsx|…|] body is treated as an
-- opaque block so the surrounding module parses.  The placeholder is
-- only ever forced if the QQ is actually called.
main = print (length label)
  where
    label = "form-control" :: [Char]
    _unused = [hsx|<div class="form-control">{label}</div>|]
