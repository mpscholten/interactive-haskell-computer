-- The binding `unused` appears *after* main — the scanner must never
-- reach it, so the gibberish on that line must never be parsed.
main = 7
unused = )))garbage***syntax not even tokens
