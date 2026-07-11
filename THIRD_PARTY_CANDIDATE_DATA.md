# Candidate table third-party data notice

`candidate.txt` is generated data and is **not covered by this repository's MIT license**. It remains licensed under **LGPL-2.1-or-later**.

## Source evidence

- Current upstream: <https://codeberg.org/chewing/libchewing-data>
- Pinned commit: `bfba418ea8a4bd2fe10b9bf80b4bf81ec01af5f4` (2026-07-04, `build: lower cmake minimum version to 3.27.0`)
- Source file: `dict/chewing/tsi.csv`
- Source SHA-256: `DFDD65583115E1D43DEAE5554053C79AA9DE950232A42C310A9E5DDDE5D13F03`
- File header copyright: `Copyright (c) 2025 libchewing Core Team`
- File header license: `LGPL-2.1-or-later`
- The header metadata was added by upstream commit `1e7def281ce5f866c51bea0c6c437b2af3ea7232` (`Move metadata to file header`).

The archived GitHub mirror at <https://github.com/chewing/libchewing-data> was also checked at commit `c44e81aef24b06f1509f19e1be54c99812d0c43f`; the same source file carries the same copyright and license header. Neither checked repository root contains a separate `LICENSE`, `COPYING`, or REUSE manifest, so the explicit per-file `dc:license` metadata is the controlling evidence used here.

## Transformation and redistribution

On 2026-07-11, `tools/convert-chewing-candidates.ps1` read the pinned upstream CSV, removed metadata, pronunciation and one-scalar entries, split phrases into one-to-three-scalar prefix/suffix mappings, kept the maximum frequency for duplicates, then emitted deterministic frequency-ranked UTF-8 TSV. The generated `candidate.txt` SHA-256 is `6EE4364A78409DF376AAAD42206D3746EEE6F60F39026566CE220F654E54E4CA`.

Redistribution is permitted under LGPL-2.1-or-later provided that recipients keep this copyright, source, license and modification notice and receive the generated source-form data plus a copy of the LGPL. The converter and pinned upstream location remain available in this repository, and the license text is included at `LICENSES/LGPL-2.1-or-later.txt`. This separate data license does not change the MIT license of the UCL_LIU_CSharp program code.
