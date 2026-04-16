# record_dot

Showcases `OverloadedRecordDot` syntax: records accessed as `p.pName`,
`p.pAddr.addrCity` (chained). Two record types, `Address` and `Person`,
with nested dot access.

Demonstrates: record declarations, record construction, chained `.` access,
string equality via `==`.

Note: record field declarations must be on a single line (multi-line record
syntax is not yet scanned correctly). Field names should not shadow builtins.

## Run

```
nix develop -c cabal run exe:ihc -- run examples/record_dot/Main.hs
```

## Verify

```bash
nix develop -c cabal run exe:ihc -- run examples/record_dot/Main.hs | diff - examples/record_dot/record_dot.out
```
