# dbmdev — package review

A thorough, critical review of the **current** `dbmdev` package from user (UX) and
developer (maintainability) perspectives, covering documentation, correctness bugs,
usability, and CRAN-readiness. The package predicts / back-calculates
temperature-dependent development of the diamondback moth for entomologists (often
non-expert R users) and is being prepared for CRAN.

**Scope note.** Items that overlap the recent V1→V2 rewrite decisions already tracked in
[`rewrite-review.md`](rewrite-review.md) (the `keep` return-type/aliases, output column
order, `briere2` direction default) are cross-referenced here rather than duplicated.

**Method.** Re-verified on **2026-07-14** against the working tree of branch
`sam-updates` by loading the package with `pkgload::load_all()` under R 4.5.3 and
exercising each finding. The full `testthat` suite passes (**72 tests, 0 failures**,
`NOT_CRAN=true`). Every "reproduced" note below was run live in this pass. Two candidate
findings were checked and rejected as false (listed at the end).

**Severity:** **[H]** high (correctness / blocks release) · **[M]** medium (significant) ·
**[L]** low (polish).

---

## 0. Resolved since the previous review (verified)

These items from the earlier review are now fixed in the working tree and have been
dropped from the lists below:

- **BUG-1** — `fitted` is now seeded as an empty POSIXct (`R/predict_dev.R:234`), and the
  empty-stage placeholder uses POSIXct too (`R/predict_dev.R:259-262`). Stage timings are
  correct.
- **BUG-2** — `start_stage` is now honoured: generation 1 iterates `stages`
  (`R/predict_dev.R:236,241`), later generations run all stages.
- **DOC-1** — `predict_dev` `@param keep` now matches the code (default `"stages"`, value
  `"generations"`, partial matches, hard error otherwise). *(But see DOC-14 for a wording nit.)*
- **DOC-5 / DOC-6 / DOC-8** — `briere2` `@return` now says `data.frame`; the empty
  `\code{}` cross-reference is fixed; the `row.names(devparams())` typo is now
  `row.names(dev_params())`.
- **DOC-7** — `briere2` is now coherently documented as an exported, user-facing helper
  (no longer described as "internal").
- **MNT-2** — direction-normalisation duplication is gone: `briere2` is forward-only and
  the logic lives only in `predict_dev` (`R/predict_dev.R:143-147`).
- **MNT-4** — empty-stage placeholder is consistent with `briere2` output (POSIXct, no
  stray `stage` column).
- **PKG-1** — `.Rbuildignore` is now tracked in git (commit `f2538ca`) and removed from
  `.gitignore`.

Partially addressed (see the updated items): **DOC-9**, **MNT-3**, **MNT-5**, **PKG-3**.

## 1. Bugs (correctness)

- **BUG-3 [M] — `hourly()` next-day minimum (`Tp`) shift crosses location boundaries.**
  `R/hourly.R:118` `out$Tp <- c(out$Tn[-1], NA)` shifts the whole (multi-location,
  timezone-concatenated) frame with no grouping, so each location's **last day** borrows
  the *next location's* first-day minimum, and the final row of the frame gets `NA`.
  **Reproduced:** with a 2-location fixture, loc1's last-day `Tp` = **11.159**, which is
  exactly loc2's first-day `Tn`; loc2's last row is `NA`. This contaminates the sunset
  temperature (`T0`) and the night-time interpolation for the last day of every location.
  **Fix:** compute `Tp` per location (within the existing `split()`, or via `ave`/
  `dplyr::lead` keyed on `location_crds`).

- **BUG-4 [M] — `hourly()` errors on its own documented minimal input.**
  `R/hourly.R:149` `ret_cols` always includes `"location_key"`, but with the documented
  required columns (`lat`, `lon`, `date`, `min`, `max` — no key) and the default
  `add_location_key = FALSE`, that column never exists → `out[ret_cols]`
  (`R/hourly.R:155`) fails with *"undefined columns selected"*.
  **Reproduced** (error text verified). The examples only pass because `daily()` happens
  to supply a `location_key`.
  **Fix:** include `location_key` in `ret_cols` only when it is present.

- **BUG-5 [M] — `plot_dev()` is broken for the default `predict_dev()` output and for two
  of its three documented modes.**
  `R/plot_dev.R:37` `match.arg(what)` resolves against the single formal default
  `"stages"`, so `what = "increments"`/`"gens"` error with *"'arg' should be \"stages\""*.
  `R/plot_dev.R:38` `match.arg(what, names(x))` assumes `x` is the `keep = "all"` **list**;
  since `keep` now defaults to `"stages"` (a data.frame), `plot_dev(predict_dev(...))`
  errors with *"'arg' should be one of \"gen\", \"stage\", …"*.
  **Reproduced:** both failures verified. The example only works because it was changed to
  `keep = "all"` as a stopgap; the plain `plot_dev(predict_dev(h1, start_date = …))` a new
  user would write is broken.
  **Fix:** correct the `what` choice set, accept a list *or* a bare `stages` data.frame,
  and implement or clearly gate the unimplemented `increments`/`gens` modes. Note the mode
  name mismatch too: `plot_dev` advertises `"gens"` while the list element is
  `"generations"` (ties DOC-2).

- **BUG-6 [M] — `daily()` gives every row an identical diurnal range.**
  `R/daily.R:104` `df$max <- df$min + (10 * runif(1, min = 1.2, max = 2))` draws a
  **single** scalar reused for all rows. **Reproduced:** `max - min` is a constant
  **15.6619** across all 8 sample days. Contrast the row-wise `min` on `R/daily.R:103`.
  **Fix:** draw per row, e.g. `runif(nrow(df), 1.2, 2)`.

- **BUG-7 [L] — `daily()` next-day adjustment leaks across locations.**
  `R/daily.R:109` `lead(min, 1, default = dplyr::last(max) - adj)` is not grouped by
  `location_key`, so the last day of one location is corrected against the *next*
  location's first `min` (same class of bug as BUG-3). Group by `location_key`.

- **BUG-8 [L] — `hourly()` timestamps are local clock hours labelled UTC.**
  `.hourly_obs()` `R/hourly.R:185` builds `datetime` with `lubridate::ymd_h()` (UTC) from
  locally-derived hours, so the values are not true instants. Benign for single-location
  ordered stepping, but incorrect if timestamps are compared or merged across zones.
  Document the convention or attach the timezone.

- **BUG-9 [L] — Over-broad column rename.**
  `R/hourly.R:117` `gsub("^m[ia]", "T", names(out))` renames *any* column beginning
  `mi`/`ma` (e.g. a stray `mid`), not just `min`/`max`. Rename explicitly.

- **BUG-10 [M] — `briere2()` validates `datetime` but not `obs`, so a missing `obs`
  column throws a cryptic error.**
  `R/briere2.R:63-64` checks `is(df, "data.frame")` and `"datetime" %in% colnames(df)`
  but never checks for `obs`, which is required at `R/briere2.R:74` (`obs <- df$obs`).
  **Reproduced:** `briere2(data.frame(datetime = …), a = …, …)` fails with
  *"replacement has 0 rows, data has 5"* — opaque for an exported, documented function.
  **Fix:** add `stopifnot("obs" %in% colnames(df))` (mirroring the `datetime` check) with
  a friendly message.

## 2. Documentation

- **DOC-2 [M] — `plot_dev` `@param what`** lists `increments`/`stages`/`gens`, but only
  `stages` is implemented and reachable (BUG-5), and `gens` does not even match the
  output element name `generations`.
- **DOC-3 [M] — `dev_params()`** has no citation for the source of the fitted Briere
  parameters and only documents `@return a Matrix`. Add the parameter source/reference,
  describe the columns (`a`, `Tmin`, `Tmax`, `m`) and their units, and note that `"dbm"`
  is currently the only supported `species`.
- **DOC-4 [M] — No package-level help.** There is no `?dbmdev` topic (`R/dbmdev-package.R`
  with `"_PACKAGE"` is absent — though an unused `old/dbmdev-package.R` exists to adapt).
  See PKG-6.
- **DOC-10 [L] — `predict_dev` `@param FUN`** describes an "unquoted function name", but
  the code requires the string `"briere2"` (`match.arg`, `R/predict_dev.R:150`); passing
  the bare symbol `briere2` errors. Drop the "unquoted function name" wording.
- **DOC-11 [L] — `predict_dev` `@param start_hour`** documents range 1–23, but the code
  only checks `start_hour < 24` (`R/predict_dev.R:193`); `0` and negative values pass and
  produce `NA`/empty results silently. Reconcile the doc and the validation.
- **DOC-12 [L] — Undocumented details.** `hourly()`/`daily()` boolean/logical parameters
  lack stated type and default, and the output columns of `hourly()` (`location_crds`,
  `datetime`, `obs`, `location_key`) and `daily()` are not described in `@return`.
- **DOC-13 [L] — `.hourly_obs`** produces a stray `man/dot-hourly_obs.Rd` with an
  unhelpful title, an undocumented `df` argument, and no `\value`. See PKG-11.
- **DOC-14 [L] — `predict_dev` `@param keep` wording is self-contradictory:** *"The value
  must be given in full (partial matches such as \"s\" are also accepted; …)"*
  (`R/predict_dev.R:60-61`). Either drop "must be given in full" or the partial-match
  clause.

## 3. Usability (UX for entomologists)

- **UX-1 [L] — Incomplete output is signalled by `message()`, and the docs mislabel it a
  "warning".** `R/predict_dev.R:312` uses `message()`, which is easy to miss in scripts,
  yet the example comment (`R/predict_dev.R:110-111`) says *"A warning is thrown …"*.
  **Reproduced:** a forward `gens = 10` run emits a *message* (`emitted warning: FALSE`).
  Use `warning()` (and consider a structured attribute on the result), and fix the doc.
- **UX-2 [L] — Cryptic / colliding names.** `location_crds`, `mean_temp_oC`; and the
  output column `start_dev` (in both the `stages` and `generations` frames) collides with
  the `start_dev` *argument* — the column holds a datetime, not a proportion. Rename for
  clarity (e.g. `start_time`/`complete_time`).
- **UX-3 [L] — `start_date` exact-match is brittle** and its "not found" error blames the
  format rather than reporting the available date range (`R/predict_dev.R:195-202`). Accept
  any date within range and report the range on failure.
- **UX-4 [L] — Multi-location dead-end.** `daily()`/`hourly()` support many locations, but
  `predict_dev()` rejects >1 (`R/predict_dev.R:207-210`) — and `hourly()`'s own example
  produces multi-location data that `predict_dev()` then refuses. Either loop over
  locations internally or steer users clearly.
- **UX-5 [L] — `hourly()` prints a location-key message on every call**
  (`R/hourly.R:138,144`). Make it opt-in (`verbose`) or drop it. *(Note: `test-hourly.R`
  currently asserts these messages, so the tests must be updated alongside.)*
- **UX-6 [L] — No Fahrenheit input path;** Celsius is assumed silently (also in
  `TODO.md`). Consider a `units`/conversion argument.
- **UX-7 [L] — No way to query events.** Users cannot ask "when did eclosion occur?" (in
  `TODO.md`). A small helper to query the output for a given stage/event would materially
  help the target audience.

## 4. Maintainability (developer)

- **MNT-1 [M] — `daily()` `set.seed()` mutates the caller's global RNG**
  (`R/daily.R:81`). Save/restore the seed or use `withr::with_seed()` so user state is not
  clobbered (also in `TODO.md`).
- **MNT-3 [M] — Remaining test gaps.** Back-direction and multi-generation are now
  covered by snapshots, but there are still **no tests** for `plot_dev()`, the
  `dev_params()` invalid-species branch, or `briere2()` stage-parameter defaulting (the
  `stage=`/defaulting path), and the incomplete-series `message()` is not asserted.
- **MNT-5 [L] — Dead code / open questions.** The dead `stages` variable is resolved
  (now used), but commented-out blocks and "is this needed?" notes remain across `R/`
  (e.g. the dead `FUN` mis-spec check `R/predict_dev.R:184-186`; `R/predict_dev.R:193,205-206,300,329`;
  `R/hourly.R:152-154,158`). Clean these up before release.
- **MNT-6 [L] — `total_days` is computed three ways** (`R/predict_dev.R:317`,
  `.summarise_stages` `:341`, `.summarise_gens` `:367`) with divergent semantics (running
  elapsed vs per-group duration) — centralise it.
- **MNT-7 [L] — Unexplained magic numbers** (`0.39` `R/hourly.R:119`; `H0 - 4`
  `R/hourly.R:123`; `/4` `R/hourly.R:175`; `adj = 4` `R/daily.R:88`) — add comments or
  named constants.
- **MNT-8 [L] — Brittle `do.call("rbind", split(...))` + rowname munging** in `hourly()`
  (`R/hourly.R:116,125-127`).
- **MNT-9 [L] — Mixed base-R and dplyr styles** within functions; pick one idiom per
  function for readability.

## 5. CRAN-readiness & packaging

- **PKG-2 [M] — `old/` legacy folder is tracked and would be bundled** (11 files:
  `fwdBriere.R`, `revDev.R`, `hourlyTemperatures.R`, …). It is git-tracked and **not** in
  `.Rbuildignore`. Remove it or add it to `.Rbuildignore`.
- **PKG-3 [M] — Extraneous files still reach the build.** `devcode2023.R` and `jobs.txt`
  are now in `.Rbuildignore` ✅, but `TODO.md` (git-tracked) and the root `.RData`
  (~130 KB) / `.Rhistory` (gitignored, but still in the working tree) are not — R CMD
  check flags top-level non-standard/hidden files. Add `TODO.md`, `^\.RData$`,
  `^\.Rhistory$` (and `old/`, PKG-2) to `.Rbuildignore`.
- **PKG-4 [M] — `LazyData: TRUE` with no `data/` directory** (`DESCRIPTION:37`) triggers an
  R CMD check WARNING. Remove it (or add a `data/` directory).
- **PKG-5 [M] — `Title` is not in title case** (`DESCRIPTION:2`) — an R CMD check NOTE.
- **PKG-6 [M] — No README, NEWS, or vignette** (and no package-level doc, DOC-4) — the
  single biggest onboarding gap for the non-expert audience, and expected by CRAN. Add a
  `README.md` and a workflow vignette (`daily → hourly → predict_dev → plot_dev`, including
  bring-your-own-data), plus `R/dbmdev-package.R` (`"_PACKAGE"`).
- **PKG-7 [L] — `DESCRIPTION` `Description`** is a single sentence duplicating the Title,
  with no method references (Briere 1999, Cesaraccio 2001). Expand it.
- **PKG-8 [L] — Full-namespace imports.** `import(ggplot2)`, `import(lutz)`,
  `import(suncalc)` (`NAMESPACE:9-11`) — convert to `importFrom` for only what is used.
- **PKG-9 [L] — Missing `URL` and `BugReports`** fields in `DESCRIPTION`.
- **PKG-10 [L] — `plot_dev` example loads `RColorBrewer`/`dplyr`** that the example never
  uses (ties DOC-9).
- **PKG-11 [L] — `.hourly_obs` Rd** has an undocumented argument and no `\value`
  (ties DOC-13).
- **PKG-12 [L] — `DESCRIPTION` has no trailing newline**, producing an *"incomplete final
  line found on '…/DESCRIPTION'"* warning on every read/`load_all()`. Add a newline.

## 6. Suggested priority order

1. **BUG-4, BUG-5** — the documented "minimal input" `hourly()` call and the natural
   `plot_dev(predict_dev(...))` call both fail out of the box for a new user.
2. **BUG-3, BUG-6, BUG-10** — silent correctness (cross-location `Tp`, constant diurnal
   range) and a cryptic `briere2` error.
3. Packaging blockers **PKG-2 – PKG-6** and the README / vignette / package-level doc.
4. Coupled docs: **DOC-2** with the `plot_dev` fix; **DOC-3/DOC-4**.
5. Remaining UX, maintainability, and packaging polish.

---

## Checked and excluded (verified false)

- *"Bringing your own data hits an unexplained `location_key` requirement"* — the flow as
  described did not reproduce; the genuine issue in that area is captured precisely as
  **BUG-4**.
- *"`Config/roxygen2/version: 8.0.0` is an invalid version string"* — the value is
  legitimate.
