-- Gap: `Strict` language pragma causes silent exit in IHC evaluator. Seen in: warp-3.4.12 (all modules use `default-extensions: Strict StrictData`). Ref: warp-dryrun-findings.md (blocker #3).
{-# LANGUAGE Strict #-}

main = do
    let x = 1 + 2
    let y = x * 4
    print y
