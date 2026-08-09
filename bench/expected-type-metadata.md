# Expected-type metadata benchmark

`expected-type-metadata.sh` measures the owner-scoped metadata lookup used to
elaborate a constrained value from its argument's expected type. It also runs a
neutral multi-parameter type-class fixture before timing, so a fast but
incorrect implementation cannot pass.

Run it inside the development environment:

```sh
nix develop -c bench/expected-type-metadata.sh
```

The script builds `ihc` once, performs one untimed warm-up, then validates and
times five runs of `expected_arg_cs_text.hs`. It reports their median and exits
non-zero when that median is above 23 seconds.

For repeatable parallel worktree runs, give each worktree its own build
directory. An existing executable skips the build entirely:

```sh
nix develop -c bench/expected-type-metadata.sh \
  --builddir dist-newstyle-benchmark \
  --threshold 20.5

nix develop -c bench/expected-type-metadata.sh \
  --binary /absolute/path/to/ihc \
  --threshold 20.5
```

The equivalent environment variables are `IHC_BIN`, `IHC_BUILDDIR`, and
`IHC_THRESHOLD_SECONDS`. Command-line options take precedence.
