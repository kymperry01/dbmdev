# dbmdev — rewrite review

**Purpose.** During the recent revival of `dbmdev`, Stevie (`smped`) rewrote the
package on a branch, which was merged into `master` (PR #6, merge commit
`c6da0cf`). Before merging, Sam added a suite of regression tests + snapshots
built against the *pre-rewrite* code, so we could see how much behaviour changed.

This document records what the harness found. It has three parts:

1. **Changes made** in this branch — objective bug fixes, the restored friendly
   error messages, and the now-implemented interface decisions.
2. **Resolved** — the interface differences, each now decided and implemented.
3. **Documentation inconsistencies** — reconciled with the decisions.

## Version map

| Label | What it is | Where it lives |
|---|---|---|
| **V1** | Kym's original code (pre-rewrite) | git commit `0a2020e` |
| **Tests + snapshots** | written/generated against **V1** | `tests/testthat/` (working tree) |
| **V2** | Stevie's rewrite, merged | the current `R/` code on `master` |

The regression tests encode **V1** behaviour and are run against **V2**, so every
failure marks a place where the rewrite changed observable behaviour.

**Reassuring headline:** the core development-rate maths is unchanged. In the
`briere2` snapshot, every `dev` and `total_dev` value is identical between V1 and
V2 — only presentation (column order) and the interface differ.

---

## Part 1 — Changes made in this branch

### 1.1 Bug fix — `hourly()` silently dropped rows (data loss)

- **File:** [R/hourly.R](R/hourly.R) (de-duplication step)
- **Caught by:** `test-hourly.R` — *"each location has a complete hourly time series"*
- **Symptom:** For `loc21` (Australia/Perth) in the 50-location fixture, the hourly
  series collapsed from **72 rows to 2**. Data-dependent and silent.
- **Cause & fix:** de-duplication applied `duplicated()` to each column
  *independently* rather than to the `(location_key, datetime)` combination:

  ```r
  # V2 (buggy)
  dups <- do.call("cbind", lapply(out[c("location_key", "datetime")], duplicated))
  dups <- rowSums(dups) == ncol(dups)
  # Fixed
  dups <- duplicated(out[c("location_key", "datetime")])
  ```
- **Verified:** all 50 locations now return complete series (3,583 rows total).
  V1 history even contains a commit *"Fixed bug in hourly() that occasionally
  caused duplicate datetimes"* — the rewrite reintroduced this class of bug.

### 1.2 Bug fix — `predict_dev()` `total_days` grew quadratically

- **File:** [R/predict_dev.R](R/predict_dev.R) (the `increments` table)
- **Caught by:** not directly asserted; found by code inspection.
- **Symptom:** `total_days` on the hourly `increments` table should be elapsed
  days = row / 24 (`0.042, 0.083, 0.125, …`) but was accelerating
  (`0.042, 0.125, 0.25, …`).
- **Cause & fix:** the rewrite kept `cumsum()` but changed its argument to an
  already-cumulative sequence, so it accumulated twice:

  ```r
  # V2 (buggy)
  cumsum(seq_along(out_all$increments$datetime) / 24)
  # Fixed
  seq_along(out_all$increments$datetime) / 24
  ```
- **Verified:** `total_days` is now linear with a constant `1/24` step (matching
  the sibling helpers `.summarise_stages()` / `.summarise_gens()`).

### 1.3 Restored V1's friendly validation messages + argument aliases

**Decision (Kym):** revert to the more helpful, per-item error messages from V1.
Applied across `predict_dev()`, `hourly()`, and `briere2()`. This resolves review
items **2.3** and **2.4** below.

- **Per-variable class errors** (`predict_dev`, `hourly`) now read
  *"Variable named `datetime` must be class POSIXct"* etc., instead of one combined
  block. (The message still also contains the *"The following columns…"* header, so
  both existing test expectations are satisfied.)
- **`start_stage`** → *"start_stage must be one of: …"* (was a bare `match.arg` error).
- **`start_date`** → *"start_date must be a character string in YYYY-MM-DD format"*
  (was *"Failed to parse start_date"*).
- **`direction`** (both `predict_dev` and `briere2`) → accepts aliases/partial
  matches (`"f"`, `"b"`, `"F"`, `"B"`) again, and rejects others with
  *"Direction must be 'forward' or 'back'"* (`predict_dev`) /
  *"Direction must be forward or back"* (`briere2`).

**Not touched:** `keep` handling in `predict_dev()` (default value, aliases,
mis-spec fallback) — resolved subsequently in §1.4.

### 1.4 Resolved the remaining open items + fixed two critical bugs

**Decisions (Kym/Sam):** all Part 2 items are now closed (see the table). Implemented
in this branch:

- **2.1** — `keep` now defaults to `"stages"` (returns just the life-stage
  `data.frame`); `"all"` still returns the 3-element list. One-line default change
  (`keep` choices reordered so `"stages"` is first).
- **2.2** — kept V2 behaviour: `match.arg` (exact or unambiguous partial matches like
  `"s"`); any other value is a hard error (no alias table, no mis-spec fallback).
- **2.5** — `briere2()` is now **forward-only** and no longer has a `direction`
  argument; backward-in-time prediction is handled entirely inside `predict_dev()`
  (it reverses the series, then flips the sign and cumulative total). Verified
  equivalent to the old direction-based output for both forward and back.
- **2.6** — kept the current column order (`location_crds` first); snapshots regenerated.

**Two critical bugs found in the review were fixed first** (required before snapshots
could be regenerated against correct output — see [`package-review.md`](package-review.md)
BUG-1 and BUG-2):

- **BUG-1** — `fitted <- lubridate::Date()` coerced datetimes to midnight, so every
  prediction's stages overlapped. Seeded an empty POSIXct instead. V1 used `c()`.
- **BUG-2** — `start_stage` was ignored (dead `stages` variable); generation 1 now
  iterates from `start_stage`, later generations run all stages.

After the fixes, the default forward `egg` output matches the V1 snapshot dates
exactly, confirming correctness.

---

## Part 2 — Resolved

Every row is now closed. (Originally "open questions for Kym".)

| # | Area | V1 (tests expect) | V2 (current code) | Caught by | Status |
|---|---|---|---|---|---|
| 2.1 | `predict_dev` default `keep` | `"stages"` → returns a **tibble** | `"all"` → returns a **3-element list** | `test-predict_dev.R` snapshot + `:114` | ✅ Done (1.4) — default `"stages"`; `"all"` returns the list |
| 2.2 | `keep` aliases & bad input | `"s"`/`"g"`/`"gens"`/`"increments"` accepted; invalid → *message* + fall back to stages | only exact names; invalid → hard `match.arg` error | `test-predict_dev.R:113`, `:138` | ✅ Done (1.4) — kept V2: `match.arg`, invalid errors |
| 2.3 | Validation messages | friendly, per-item | terse `match.arg` / combined block | `test-predict_dev.R:46`,`:70`; `test-hourly.R:51` | ✅ Done (1.3) |
| 2.4 | `direction` aliases + error | aliases accepted; friendly error | `match.arg` (case-sensitive); generic error | `test-briere2.R:22`; `test-predict_dev.R:70` | ✅ Done (1.3) |
| 2.5 | `briere2` `direction` | (no default; required arg) | defaults to **"back"** while `predict_dev` defaults **forward** | `test-briere2.R` | ✅ Done (1.4) — `briere2` forward-only; back handled by `predict_dev` |
| 2.6 | Output column order | `location_key` first | `location_crds` first | `test-briere2.R` snapshot | ✅ Done (1.4) — kept V2 order; snapshots regenerated |

---

## Part 3 — Documentation inconsistencies (fixed)

Reconciled with the decisions above:

- **`predict_dev` `@param keep`** — now documents the default `"stages"`, the value
  `"generations"` (not `"gens"`), and that any other value errors. Examples updated:
  the default call shows the `stages` data.frame directly, and list-style access uses
  `keep = "all"`. ✅ Fixed
- **`start_stage` doc** — `row.names(devparams())` → `row.names(dev_params())`. ✅ Fixed
- **`briere2` docs** — removed the `direction` `@param`, fixed the empty `\code{}`
  cross-reference (now `\code{\link{predict_dev}}`), and corrected `@return`
  (`data.frame`, not `tibble`). ✅ Fixed
- **`plot_dev` example** — updated to `keep = "all"` so it still runs under the new
  default (see the note under *Related consequence* below). ✅ Fixed

**Related consequence (flagged, not yet fully fixed):** changing the `keep` default to
`"stages"` means `plot_dev(predict_dev(...))` now receives a data.frame rather than the
list it expects — this is `package-review.md` **BUG-5**, and it is now *active* rather
than latent. The example was adjusted as a stopgap; `plot_dev()` itself should be made
to accept either shape.

---

## Current test status

- **72 passing, 0 failing** (run with `NOT_CRAN=true` so the snapshot tests execute).
- Snapshots (`briere2.md`, `predict_dev.md`) regenerated against the corrected,
  bug-fixed output.
- `man/*.Rd` regenerated via `roxygen2::roxygenise()`.

## Next steps

1. Address `package-review.md` **BUG-5** properly (make `plot_dev()` accept the default
   `stages` data.frame), plus the remaining review items (BUG-3/4/6, packaging, docs).
2. Consider committing `.Rbuildignore` (currently gitignored — `package-review.md`
   PKG-1) so the new build-ignore entries are shared.
