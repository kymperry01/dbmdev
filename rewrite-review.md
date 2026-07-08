# dbmdev — rewrite review

**Purpose.** During the recent revival of `dbmdev`, Stevie (`smped`) rewrote the
package on a branch, which was merged into `master` (PR #6, merge commit
`c6da0cf`). Before merging, Sam added a suite of regression tests + snapshots
built against the *pre-rewrite* code, so we could see how much behaviour changed.

This document records what the harness found. It has three parts:

1. **Changes already made** in this branch — objective bug fixes, plus the
   friendly error messages Kym asked to restore.
2. **Open questions for Kym** — deliberate-looking interface changes still to
   decide, one per row.
3. **Documentation inconsistencies** — smaller cleanups.

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
mis-spec fallback) — that is a larger change to the function's return *type* and is
left open as items 2.1/2.2 for Kym.

---

## Part 2 — Open questions for Kym

Each row is a place where V2 still differs from V1. Decide per item: **keep V2,
restore V1, or design something new?**

| # | Area | V1 (tests expect) | V2 (current code) | Caught by | Status |
|---|---|---|---|---|---|
| 2.1 | `predict_dev` default `keep` | `"stages"` → returns a **tibble** | `"all"` → returns a **3-element list** | `test-predict_dev.R` snapshot + `:114` | **Open — Kym** |
| 2.2 | `keep` aliases & bad input | `"s"`/`"g"`/`"gens"`/`"increments"` accepted; invalid → *message* + fall back to stages | only exact names; invalid → hard `match.arg` error | `test-predict_dev.R:113`, `:138` | **Open — Kym** |
| 2.3 | Validation messages | friendly, per-item | terse `match.arg` / combined block | `test-predict_dev.R:46`,`:70`; `test-hourly.R:51` | ✅ Done (1.3) |
| 2.4 | `direction` aliases + error | aliases accepted; friendly error | `match.arg` (case-sensitive); generic error | `test-briere2.R:22`; `test-predict_dev.R:70` | ✅ Done (1.3) |
| 2.5 | `briere2` `direction` default | (no default; required arg) | defaults to **"back"** while `predict_dev` defaults **forward** | — (latent inconsistency) | **Open — Kym** |
| 2.6 | Output column order | `location_key` first | `location_crds` first | `test-briere2.R` snapshot | **Open — Kym** |

Notes:

- **2.1 is the highest-impact open item:** it changes the default return *type* of
  the main user-facing function (data frame vs list), and contradicts V2's own docs
  (see Part 3). Suggest deciding this one first.
- **2.5 / 2.6** are small, but the two functions should at least be made
  *consistent* with each other before submission, whichever way we go.

---

## Part 3 — Documentation inconsistencies

Independent of the decisions above, to tidy once `keep` (2.1/2.2) is resolved:

- **`predict_dev` `@param keep`** ([R/predict_dev.R](R/predict_dev.R)) documents the
  default as `"stages"` and lists `"gens"` as a value, but the code default is
  `"all"` and the accepted value is `"generations"`. Examples use `keep = "gens"`
  and list-style access (`pred$stages`, `pred$generations`). Docs, code default,
  and examples must be made to agree.
- **`start_stage` doc** references `row.names(devparams())` — typo for `dev_params()`.

---

## Current test status

- **69 passing.**
- **Remaining failures (3 blocks), all in `test-predict_dev.R` and all tied to the
  open `keep` question (2.1 / 2.2):**
  - *"predict_dev example outputs are stable"* — default output shape + `keep = "gens"` (2.1)
  - *"predict_dev handles keep aliases, defaults, and return types"* (2.1 / 2.2)
  - *"predict_dev warns through a message and falls back to stages for invalid keep"* (2.2)

No failures remain in `test-briere2.R`, `test-hourly.R`, or `test-daily.R`.

## Next steps

1. **Decide the open Part 2 items with Kym** (start with 2.1).
2. For each, either adjust the code (restore V1) or relax the test (accept V2),
   then regenerate snapshots with `testthat::snapshot_accept()`.
3. **Tidy Part 3 docs** to match the resolved `keep` behaviour.
