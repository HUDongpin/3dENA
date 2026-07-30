# 3D ENA systematic bug-detection and remediation audit

## Decision

All ten registered findings, ENA-BUG-001 through ENA-BUG-010, are **resolved in
the current remediation worktree**. The strict deterministic harness reports
zero reproduced seeded findings and zero S0/S1 blockers; the locked R suite,
128-case property audit, subsystem coverage gate, strict accessibility audit,
and three-repeat five-project browser matrix are clean. No public analytical
API, exchange schema version, download format, or UI contract was changed.

This document preserves the sanitized pre-remediation fixtures, expected versus
actual behavior, and root causes below, then records the source remediation and
named regression for each finding. AI remained disabled throughout verification
and provider tests used mocked transports/jobs only.

The registered bug-remediation goal is complete, but the broader release audit
is **not yet a pass**. Four legacy functions remain above the conservative
complexity gate and need refactoring or an explicitly approved baseline.
Docker is also unavailable in the local environment, so
the hardened image, read-only runtime, nginx redirect/WebSocket behavior, and
container vulnerability gates have not been reproduced locally. The CI jobs
described below must run on the exact audited revision before a release verdict
can be issued.

The container workflow exercises proxied health, redirects, security headers,
the nginx WebSocket-upgrade directives, and bounded live HTTP 101 handshakes
against both the direct Shiny endpoint and the TLS proxy. Those checks remain
CI-only until the workflow runs on the exact audited revision.

The original detector implementation was based on `main` revision
`6282670021a0b240b4086debe2e2ee7dbb2810cd`. The harness and CI baseline record
the revision, dirty-state flag, runtime versions, lock hashes, configuration
names, seeds, and selected source hashes in their JSON evidence. A dirty flag
is expected while the detector changes themselves are uncommitted; a release
audit must run from a clean checkout.

## Safety and scope

- AI execution is disabled. The harness does not load a provider transport;
  existing AI tests use fake jobs or injected transports only.
- No credentials, live provider, production service, or user-supplied data are
  accessed. `All API Keys.docx` is expressly outside the audit boundary and is
  also excluded from the CI secret-scan tree.
- Finding fixtures are synthetic. Property tests use synthetic or bundled
  reviewed datasets. The finding and property JSON reports retain
  descriptions, counts, booleans, condition classes, hashes, and seeds, never
  raw rows or participant values.
- Browser runs force `ENA3D_AI_ENABLED=false`, stub the analytics script, and
  target an isolated local Shiny process.
- Native-code sanitizers are out of scope because the repository contains no
  first-party C or C++.
- Detection artifacts under `output/` are generated evidence, not source. Only
  sanitized summaries are uploaded by CI; rendered browser/research content is
  kept ephemeral.

## Severity and status policy

| Level | Default meaning |
| --- | --- |
| S0 | Code execution, cross-user disclosure, or widespread corruption. |
| S1 | Privacy-boundary breach, scientifically wrong output, valid-input crash, or resource-cap escape. |
| S2 | Contained incorrect behavior with a practical workaround or a material audit/accessibility defect. |
| S3 | Cosmetic or diagnostic defect. |

`resolved` means the original sanitized fixture no longer reproduces and a
named regression test passes in the locked environment. `CI-only` and `not
executed locally` describe evidence state, not a pass.

The R harness has two modes:

- `report-only` writes a complete report and exits zero even when findings
  block release.
- `strict` exits `0` only for a complete pass, `1` for reproduced S0/S1 or
  failed property gates, and `2` for incomplete evidence or detector errors.

## Reproduction commands

Run commands from the repository root unless testing path independence.

### Locked baseline and deterministic detectors

```sh
Rscript renv/bootstrap.R
Rscript tests/check.R

Rscript tools/run_bug_audit.R \
  --mode report-only --output output/audit --seed 20260719
Rscript tools/run_bug_audit.R \
  --mode strict --output output/audit --seed 20260719

Rscript tools/run_property_fuzz.R \
  --mode report-only --output output/audit/property-fuzz \
  --seed 20260719 --iterations 128

Rscript tools/run_static_audit.R \
  --mode report-only --output output/audit/static
Rscript tools/run_coverage_audit.R \
  --mode report-only --output output/audit/coverage
```

Replay one seeded finding without storing its fixture values:

```sh
Rscript tools/run_bug_audit.R \
  --mode report-only --output output/audit \
  --seed 20260719 --only ENA-BUG-001
```

Replay one indexed mutation case:

```sh
Rscript tools/run_property_fuzz.R \
  --mode report-only --output output/audit/property-fuzz \
  --seed 20260719 --iterations 128 --replay-case 1
```

The full CI property matrix uses seeds `104729`, `20260719`, and `8675309`;
the mutation/boundary audit uses seed `20260719` with 128 cases.

### Bundled scientific oracle

```sh
Rscript tools/run_bundled_real_e2e.R
```

The script now discovers the project root from its invocation path, working
directory, or `ENA3D_PROJECT_ROOT`. It runs the four bundled datasets through
real centroid paths, seeded 500-replicate bootstraps, paired or independent
comparisons, 999-permutation inference where applicable, Plotly numerical
inspection, CSV re-import, and manifest generation. Its evidence is written to
`output/bundled-real-e2e/`.

### JavaScript and browser audit

```sh
npm ci
npx playwright install chromium firefox webkit
npm run lint

E2E_PORT=43838 npm run test:e2e
E2E_PORT=43838 npm run test:e2e:repeat
E2E_PORT=43838 npm run test:e2e:a11y:report
```

`npm run test:e2e:a11y` is the strict accessibility form. The remediation run
passed all three projects, and each sanitized Axe artifact contains zero
violation and zero incomplete groups.

The fresh isolated three-repeat smoke execution passed all 45 assertions in
5.7 minutes with no retry, browser console error, page error, or default
Network render failure.

The reliability test is opt-in and uses a separate port. The release workflow
runs it for 30 minutes; a short run only validates the test plumbing.

```sh
ENA3D_AUDIT_SOAK_MINUTES=2 npm run test:e2e:soak
```

## Evidence artifacts

| Artifact | Purpose |
| --- | --- |
| `output/audit/audit-report.json` | Sanitized baseline, findings, properties, and gate decision. |
| `output/audit/audit-findings.jsonl` | One selected seeded R-harness finding per line. |
| `output/audit/audit-report.sha256` | Integrity checksum for the main report. |
| `output/audit/property-fuzz/property-fuzz-report.json` | Seeded round-trip, mutation, boundary, trajectory, and CSV properties. |
| `output/audit/property-fuzz/property-fuzz-report.sha256` | Integrity checksum for the property report. |
| `output/audit/static/` | `lintr`, `codetools`, complexity, and JavaScript-lint summaries. |
| `output/audit/coverage/` | Sanitized subsystem coverage, named risk-test mapping, and no-regression comparison. |
| `output/bundled-real-e2e/` | Per-dataset scientific manifests and numerical round-trip evidence. |
| `output/playwright/audit/accessibility-*.json` | Sanitized Axe rules, impacts, selectors, and contrast ratios. |
| `output/browser-audit/` | CI browser-project status and sanitized failure titles. |
| `output/soak/` | CI duration, iteration, bounded-memory, health, and cleanup evidence. |
| `output/supply-chain/` | npm audit, redacted secret scan, filesystem scan, and repository SBOM. |
| `output/container/` | Runtime health/proxy checks, image SBOM, and vulnerability report. |

## Local remediation snapshot

The following evidence was produced locally on 2026-07-20. It is deliberately
separate from checks that only exist in the pre-release workflow.

| Check | Local result |
| --- | --- |
| Locked R baseline | R 4.4.1 loaded all 110 lockfile packages at matching versions. The local Node runtime was v24.18.0; CI pins Node 22, so CI remains the authoritative Node baseline. |
| Standard R suite | `Rscript tests/check.R` passed all source parsing and testthat checks with zero unapproved warning after expected scientific diagnostics were converted to explicit assertions. The cold-start worker regression uses a phase-separated 10-second deadline and passed in both focused and full-suite runs. |
| Deterministic finding/property harness | All 12 checks completed in strict mode: zero of eight seeded findings reproduced, all four numerical properties passed, and no blocker, detector error, or incomplete condition occurred. Strict exit is `0`; final report SHA-256 is `5e6229f992023757b5354a0990d859ad16327e24387df0a5e447a4c452582786`. |
| Mutation/property audit | Strict 128-case runs at seeds `104729`, `20260719`, and `8675309` passed all six checks without warning or detector error. Adjacent-double/fractional-duration exchange identity and CSV pre-publication rejection are now passing properties. |
| R static audit | Exact pinned detectors inspect 35 runtime files and 454 functions. The `seq_linter` defect was fixed. Four legacy functions remain above the conservative complexity review gate and 11 `codetools` dispatch/usage diagnostics remain explicitly triaged technical debt rather than registered correctness findings. |
| JavaScript lint/dependencies | After the lockfile install, `npm run lint` completed with no ESLint finding and `npm audit --audit-level=high` reported zero vulnerabilities. |
| Subsystem coverage | Five subsystems and all 50 named high-risk test contracts passed the no-regression gate: scientific correctness 89.83%, data boundaries 74.62%, AI/privacy 72.24%, reactive/async 73.59%, plotting/rendering 78.51%. |
| Bundled oracle | The 2026-07-20 invocation from `/tmp` completed the then-current three bundled datasets. The runner now declares the fourth Class 1 fixture; a new four-fixture receipt must not be inferred from this older snapshot. |
| Browser smoke | The isolated three-repeat five-project matrix reported 45/45 passing assertions in 5.7 minutes with no retry or browser error. The default `No Network` Plotly canvas rendered in every full-flow execution. |
| Accessibility | Strict Axe runs passed 3/3 on desktop, tablet, and mobile Chromium. All three sanitized artifacts contain zero violation and zero incomplete groups. |
| Reliability soak | The corrected isolated workload passed: 30.2 minutes in the test body and 30.3 minutes total. It repeatedly completed supported longitudinal trajectories, asserted the cross-sectional guard where appropriate, switched through Stats and all bundled datasets, probed health with bounded connection-reset retry, and observed no browser/page error. |
| Supply chain and container | `npm audit --audit-level=high` reports zero vulnerabilities. Docker, Trivy, Gitleaks, and Syft are unavailable locally, so redacted-secret, SBOM, filesystem/image vulnerability, hardened runtime, and proxy checks remain CI-only release gates. |

The Class 1 extension was replayed locally on 2026-07-30. The four-fixture
bundled oracle completed with zero failed dataset: Class 1 contributed 72
student-period points in 15 SVD dimensions, 26 pseudonymous entities, 15
group-period path rows, three independent condition-comparison rows, and
passing point/path/bootstrap/comparison CSV round trips. This supplements; it
does not retroactively alter the dated 2026-07-20 browser and coverage receipts.

Generated local percentages establish the committed coverage baseline; future
strict runs reject any subsystem decrease greater than 0.01 percentage points.
The 50 risk-to-test contracts are declared in
`tests/audit/coverage_manifest.json`, and the accepted values are in
`tests/audit/coverage_baseline.json`. The validated strict coverage replay
exited zero.

## Detector and matrix traceability

### Numerical and typed-value properties

| Required property | Named detector or test | Present evidence |
| --- | --- | --- |
| Row-order and selected-axis reordering invariance | `ENA-PROP-001`; trajectory and comparison unit suites | Deterministic local/CI detector. |
| Selected versus full-rotation distance | `ENA-PROP-003`; trajectory analysis tests | Deterministic local/CI detector. |
| Missing, duplicate, cohort, extreme, and near-zero policies | `ENA-FUZZ-005`; public trajectory numerical tests | Deterministic property runner and unit suite. |
| RNG restoration and seeded bootstrap replay | `ENA-PROP-002`; trajectory bootstrap tests | Three-seed CI matrix. |
| Multiplicity correction | `ENA-PROP-004`; Stats and independent-comparison tests | Deterministic local/CI detector. |
| Date, POSIXct, timezone fold, factor, numeric, and `difftime` identity | `ENA-FUZZ-002`, `ENA-BUG-004`, `ENA-BUG-006` | Passing exact typed-key, Stats pairing, and exchange round-trip regressions. |
| Bundled scientific oracle | `tools/run_bundled_real_e2e.R` | Path-independent runner; CI evidence still required. |

### Input and transactional properties

| Required property | Named detector or test | Present evidence |
| --- | --- | --- |
| All bundled exchange round trips | `ENA-FUZZ-001`; exchange unit suite | Deterministic canonical JSON SHA and byte checks; native in-memory payload-object identity is informational. |
| Distinct identifier tuples never share a key | `ENA-BUG-002` | Passing typed, length-safe, delimiter-free identity-key regression. |
| Limit minus one, exact limit, and plus one | `ENA-FUZZ-004`; exchange limit tests | Deterministic boundary checks. |
| Corrupt/duplicate/mismatched/non-finite exchange data | `ENA-FUZZ-003`; exchange unit suite; duplicate-field browser fixture | Rejection and rollback paths covered. |
| Unicode, delimiters, controls, and spreadsheet formulas | `ENA-FUZZ-006`, `ENA-BUG-007`; raw-import/security tests | Passing; CR/LF/CRLF cells are rejected before publication and reversible values round-trip exactly. |
| Failed load cannot replace active state | load-dataset and exchange module tests; browser invalid-upload path | Unit and browser transactional checks. |
| Raw CSV and Excel construction | raw-import unit suite | Browser construction and broad Excel mutation matrix remain open. |

### Browser, reactive, async, AI, and deployment properties

| Stream | Implemented coverage | Evidence state / remaining gap |
| --- | --- | --- |
| Browser engines and viewports | Desktop Chromium, Firefox, WebKit; tablet and 390px mobile Chromium | Three clean repetitions completed: 45/45. |
| Browser flows | Bundled sample, valid/invalid exchange upload, every Model view, Stats, real trajectory, state switch, numerical Plotly checks, CSV/ZIP verification | Passing across all 45 executions with no browser error in the 2026-07-20 three-fixture snapshot. The current soak declares all four fixtures, including Class 1; raw browser construction and fake-provider browser success/failure remain future matrix expansion, with provider tests still mocked at unit level. |
| Accessibility | Keyboard activation/focus, accessible states, responsive overflow, Axe WCAG A/AA on Home/Data/Stats | Strict desktop/tablet/mobile Chromium audit passed with zero findings. |
| Async resources | Hard timeout, cancel, session cleanup hooks, stale AI promise rejection, dataset invalidation; opt-in browser cancel/soak | Unit/soak coverage present. Worker death, cap/cap-plus-one multi-session pressure, rapid cancel/restart, and slot-release races still need dedicated stress fixtures. |
| AI boundary | Aggregate evidence, small-cell suppression, consent hash, malformed responses, safe errors, stale/cancel behavior, fake success/failure | Participant-like `Name` fields are rejected before evidence construction and provider call; all provider tests remain mocked. Prompt-injection and cross-session/global quota stress remain future expansion. |
| Reliability | 30-minute switch/calculate/cancel soak, health probes, process-group RSS and cleanup checks | Local browser soak and CI process/RSS wrapper provide complementary evidence. Maximum supported datasets and above-cap concurrent sessions remain future stress expansion. |
| Supply chain | npm high-severity audit, checksum-verified redacted Gitleaks, Trivy filesystem scan, repository SBOM | CI-only until the pre-release workflow completes. |
| Production container | Non-root read-only run, tmpfs, dropped capabilities, CPU/memory/PID limits, direct/proxied health metadata, redirect/security headers, direct/proxied WebSocket HTTP 101 handshakes, upgrade-directive inspection, image SBOM/Trivy | CI-only and not executed locally because Docker is unavailable. |

Visual snapshots are intentionally limited to stable shell work. Plotly paths,
arrows, colors, and download content are asserted from data and archive bytes,
not treated as correct because a screenshot looks plausible.

## Finding register

### ENA-BUG-001 — Participant-like `Name` values cross the AI Change boundary

- **Severity / confidence / status:** S1 / high / resolved.
- **Fixture:** Ten synthetic rows split between two participant-like labels,
  with each cell meeting the small-cell threshold.
- **Expected:** An identifier-like change variable is rejected, or every such
  label is absent from the public provider envelope.
- **Pre-remediation actual:** Change evidence succeeds and its public payload includes the
  synthetic labels while declaring that raw rows are excluded.
- **Root cause:** Identifier recognition and the Change evidence path are not a
  complete boundary for commonly used participant columns. Relevant code is
  `.ena3d_ai_is_identifier_name()`, `.ena3d_ai_require_aggregate_variable()`,
  and `.ena3d_ai_build_change()` in `R/ai_evidence.R`.
- **Remediation / evidence:** Identifier normalization now rejects `Name` and
  common participant/person/student/subject variants before evidence exists.
  `Change rejects common participant-name columns before evidence exists` and
  ENA-BUG-001 strict replay pass with no provider transport.
- **Impact:** Participant identifiers can be disclosed to an AI provider.
- **Regression specification:** Build Change evidence from a participant-like
  `Name` field at the threshold; require rejection and recursively assert that
  no marker appears in previews, public payloads, prompts, errors, or logs.
  Provider call count must remain zero in the test.

AI remains disabled in audit execution. Enabling it is no longer blocked by
this finding once the remediation revision passes the clean CI release gates.

### ENA-BUG-002 — Separator collision merges distinct raw unit tuples

- **Severity / confidence / status:** S1 / high / resolved.
- **Fixture:** Two distinct synthetic two-column unit tuples whose cell values
  contain the current carriage-return separator and produce one joined text.
- **Expected:** Distinct accepted tuples receive distinct internal keys, or the
  separator-bearing values are rejected before commit.
- **Pre-remediation actual:** Two distinct tuples produce one key.
- **Root cause:** `ena3d_unit_key()` in `R/raw_data_import.R` converts values to
  text and joins them with an unescaped `"\r"` delimiter.
- **Remediation / evidence:** `ena3d_unit_key()` now composes typed, exact,
  ASCII hex components whose separator cannot occur in a component. The
  adversarial CR, missing-marker, pipe, and control tuple regression is
  injective and ENA-BUG-002 strict replay passes.
- **Impact:** Separate units can merge, corrupting ENA grouping and downstream
  numerical results.
- **Regression specification:** Exercise adversarial multi-column tuples with
  missing markers and every control separator; assert injective typed keys and
  unchanged unit counts through raw construction and rollback.

### ENA-BUG-003 — Accepted dimension names fail or misresolve in formulas

- **Severity / confidence / status:** S1 / high / resolved.
- **Fixture:** Synthetic accepted dimension identifiers containing a backtick
  or a backslash.
- **Expected:** Every accepted name creates a formula whose `all.vars()` value
  exactly matches the original column, or ingestion rejects it.
- **Pre-remediation actual:** At least one accepted name causes a parse failure or resolves to
  a different name.
- **Root cause:** `tilde_var_or_null()` in `R/app_utils.R` interpolates an
  incompletely escaped identifier into formula source text.
- **Remediation / evidence:** `tilde_var_or_null()` constructs the formula
  language object directly with `as.name()` instead of parsing interpolated
  source. Backtick, backslash, and whitespace names retain exact `all.vars()`
  identity and ENA-BUG-003 strict replay passes.
- **Impact:** Valid imported data can crash a plot or silently bind the wrong
  analytical dimension.
- **Regression specification:** Generate quoting/control variants, pass them
  through exchange validation and every Plotly formula consumer, and require
  exact name identity or rejection before dataset commit.

### ENA-BUG-004 — POSIXct pairing IDs collide in a daylight-saving fold

- **Severity / confidence / status:** S1 / high / resolved.
- **Fixture:** Two exact POSIXct instants that share one displayed local time,
  repeated across two paired conditions.
- **Expected:** Both exact instants remain distinct and form two matched pairs.
- **Pre-remediation actual:** The matcher rejects them as duplicate or fails to retain both
  pairs after character conversion.
- **Root cause:** `ena3d_match_pairs()` in `R/app_module_stats.R` constructs
  `.pair_key` with `as.character()`, which loses the UTC-offset distinction.
- **Remediation / evidence:** Pair construction now uses exact typed identity
  keys while keeping readable labels only for diagnostics. The DST-fold test
  retains two pairs under row reorder and ENA-BUG-004 strict replay passes.
- **Impact:** Valid repeated-measures data can be rejected or paired
  incorrectly, producing missing or wrong statistics.
- **Regression specification:** Use typed keys for POSIXct values and assert
  two exact pairs across both sides of a DST fold, including timezone and row
  reorder variants.

### ENA-BUG-005 — Adjacent numeric trajectory conditions collapse in the UI

- **Severity / confidence / status:** S1 / high / resolved.
- **Fixture:** Two adjacent representable IEEE-754 doubles used as condition
  levels.
- **Expected:** Each exact value has a distinct stable UI token and filters
  only its rows.
- **Pre-remediation actual:** Character conversion exposes one choice and its equality mask
  selects both exact conditions.
- **Root cause:** `.trajectory_condition_values()` and comparison filtering in
  `R/app_module_trajectory.R` use display strings as identity tokens.
- **Remediation / evidence:** Condition choices carry exact numeric hex/typed
  tokens, deduplicate by typed identity, and filter through exact masks in
  comparison and network-overlay paths. Adjacent doubles produce two disjoint
  selections and ENA-BUG-005 strict replay passes.
- **Impact:** Distinct cohorts can merge, yielding scientifically incorrect
  paths, comparisons, or statistics.
- **Regression specification:** Assert two choices, exact typed round trips,
  disjoint masks, and stable behavior after dataset/axis changes for adjacent
  doubles and other collision-prone typed values.

### ENA-BUG-006 — Exchange JSON loses adjacent-double precision

- **Severity / confidence / status:** S1 / high / resolved.
- **Fixture:** Two adjacent representable doubles encoded and decoded with the
  production exchange JSON settings.
- **Expected:** Both finite values round-trip bit-exactly, or unsupported
  precision is rejected before a file is published.
- **Pre-remediation actual:** The second value serializes as the first, reducing two exact
  identities to one.
- **Root cause:** The numeric path through
  `ena3d_write_exchange_file()`/`jsonlite::toJSON()` in
  `R/ena3d_exchange.R` does not preserve all accepted IEEE-754 identities.
- **Remediation / evidence:** Exchange serialization is pinned to 17 decimal
  digits without changing the schema version. Complete files now retain
  adjacent doubles and fractional `difftime` values exactly; ENA-FUZZ-002 and
  ENA-BUG-006 strict replay pass.
- **Impact:** Numeric IDs, conditions, coordinates, or fractional `difftime`
  values can merge or change across a valid exchange round trip.
- **Regression specification:** Round-trip adjacent doubles and fractional
  `difftime` values through a complete exchange file and require bit identity,
  or explicit pre-publication rejection with no partial file.

### ENA-BUG-007 — CSV re-import collapses carriage return and newline values

- **Severity / confidence / status:** S2 / high / resolved by explicit rejection.
- **Fixture:** Two accepted synthetic text cells differing only by carriage
  return versus newline.
- **Expected:** The values remain distinct through safe CSV export/re-import,
  or the unsupported control value is rejected before writing.
- **Pre-remediation actual:** Common R CSV re-import normalizes the carriage return to newline,
  collapsing the two values.
- **Root cause:** `ena3d_write_safe_csv()` in `R/security_utils.R` does not
  establish a reversible control-character contract for exported cells.
- **Remediation / evidence:** The safe CSV writer rejects CR, LF, and CRLF in
  character/factor cells before opening the destination. No partial file is
  published; tabs, delimiters, quotes, Unicode, and neutralized formulas retain
  their supported contract. ENA-FUZZ-006 and ENA-BUG-007 strict replay pass.
- **Impact:** Downloaded CSV can merge distinct text identities; avoiding those
  controls or using the typed exchange format is a contained workaround.
- **Regression specification:** Export CR, LF, CRLF, tab, quote, delimiter,
  Unicode, and formula-prefix variants; require exact distinct round trips or
  deterministic rejection before writing.

### ENA-BUG-008 — Serious text contrast failures across responsive views

- **Severity / confidence / status:** S2 / high / resolved on desktop,
  tablet, and mobile Chromium.
- **Fixture:** Home, Data workspace, and Stats surfaces in the three Chromium
  viewport projects with light color scheme.
- **Expected:** WCAG AA normal text contrast is at least 4.5:1 and Axe reports
  no serious or critical violation.
- **Pre-remediation actual:** Automated evidence reports ratios from 3.01:1 to 4.27:1 for home
  kicker/step text, workspace and Stats tabs, upload controls, sidebar toggle,
  and fullscreen control.
- **Root cause:** The cyan/coral design tokens and related Bootstrap states are
  too light on the paper/light-gray backgrounds. Relevant definitions are in
  `R/www/app_shell.css` and the theme/control styling in `R/app.R`.
- **Remediation / evidence:** The cyan token is now `#07747a` and coral is
  `#a7442e` throughout the Bootstrap and fallback styles. Unit checks measure
  every affected foreground/background pair at or above 4.5:1; strict Axe
  reports zero violation and zero incomplete groups in all three viewports.
- **Impact:** Important labels and controls are materially harder to perceive;
  the strict accessibility audit fails.
- **Regression specification:** Run Axe WCAG A/AA in all three responsive
  Chromium projects and assert zero serious/critical findings; directly verify
  the affected foreground/background pairs meet 4.5:1 in default, active,
  hover, focus, and disabled states.

### ENA-BUG-009 — Cold-start async deadline test is timing-flaky

- **Severity / confidence / status:** S2 / medium / resolved.
- **Fixture:** The locked R environment running the original isolated
  0.3-second slow
  bootstrap-worker deadline test from the full suite.
- **Expected:** The test proves event-loop liveness, observes the worker during
  its intended running phase, then proves timeout rejection and process death.
- **Pre-remediation actual:** One full-suite run reached the `is_alive()` assertion after the
  0.3-second deadline and failed because the worker had already exited. A
  focused rerun passed.
- **Root cause:** The test in `tests/testthat/test-trajectory-module.R` couples
  its mid-flight assertion to a wall-clock interval shorter than cold process
  startup variability; the observation is not evidence of a runtime worker
  leak.
- **Remediation / evidence:** The fixture writes an explicit worker-start
  handshake, sleeps for 30 seconds, and applies a phase-separated 10-second
  executable deadline with independent heartbeat, timeout-class, process-death,
  and empty-registry assertions. Focused and locked full-suite runs pass.
- **Impact:** Locked-environment audit results can be flaky and therefore
  cannot be treated as reproducible evidence without repetition.
- **Regression specification:** Add an explicit worker-start handshake or a
  longer phase-separated deadline, then repeat from a cold locked library and
  under CPU contention. Assert heartbeat, timeout class, process-tree cleanup,
  and zero orphaned children independently.

### ENA-BUG-010 — Default `No Network` state raises a Plotly render error

- **Severity / confidence / status:** S1 / high / resolved by the
  deterministic harness and browser matrix.
- **Fixture:** A valid bundled dataset with the default `No Network` selection
  and an enabled axis overlay.
- **Expected:** The Network surface renders its valid geometry-only `No
  Network` state, or a usable transient empty fallback, with no Shiny output
  error or server warning.
- **Pre-remediation actual:** Axis post-processing converts the absent plot to a plain list;
  subsequent Plotly typography/layout dispatch fails with no applicable
  `layout` method.
- **Root cause:** `ena_network_plot_output()` in
  `R/app_module_network.R` returns `NULL` for the valid empty selection, but
  `output$ena_network_plot` unconditionally applies axis, typography, and event
  post-processing as though it received a Plotly object.
- **Remediation / evidence:** Plotly post-processors are NULL-safe and the
  renderer replaces a transient absent plot with a real annotated Plotly
  object before axes, typography, and event registration. The direct fallback
  unit test passes, ENA-BUG-010 strict replay no longer reproduces, and all 15
  repeated full-flow browser executions render the default canvas with an empty
  console/page-error collector.
- **Impact:** A valid default state emits a server-side render failure and can
  leave the Network output broken. The browser runner did not classify Shiny
  server warnings as page `console.error`, which is why its test assertions
  still reported pass.
- **Regression specification:** Load a trusted dataset, leave `No Network`
  selected, exercise every axis toggle, and assert a defined Plotly surface,
  no `shiny-output-error`, no layout-dispatch warning, no browser
  error, and continued usability after selecting and clearing a network.

## Pre-release CI split

The existing pull-request workflow remains the fast deterministic check. The
new `.github/workflows/pre-release-audit.yml` is manually dispatchable in
report-only or strict mode and runs strict automatically for prerelease and
published-release events. It pins R 4.4.1, Node 22, actions by commit, and
AI-off configuration.

The full workflow contains:

1. locked R restore, baseline metadata, standard tests, static analysis,
   subsystem coverage/no-regression gate, JavaScript lint, three-seed detector
   matrix, sanitized artifact upload, and strict harness enforcement;
2. five browser/viewport projects plus three accessibility projects, repeated
   three times for releases with no retries masking flakes;
3. the gated 30-minute Chromium load/switch/calculate/cancel soak with memory,
   health, and cleanup checks;
4. npm, redacted secret, filesystem vulnerability, and repository SBOM scans;
5. a hardened production-container build and read-only runtime, HTTP proxy,
   bounded live WebSocket handshake, directive, and image audit.

## Completion gates

An audited release requires all of the following on one clean revision:

- every required matrix cell executed with no unapproved warning, skip,
  package fallback, detector error, or omitted container check;
- all findings triaged; no open S0/S1 in an enabled feature and explicit
  disposition for every S2;
- locked R 4.4.1 and `npm ci` baselines clean, with no subsystem coverage
  regression;
- the full browser matrix clean for three repetitions and the 30-minute soak
  complete within memory/process/health limits;
- dependency, redacted-secret, SBOM, filesystem, and container vulnerability
  evidence reviewed;
- hardened container, health metadata, redirect, security-header, and live
  WebSocket upgrade checks clean; and
- a sanitized report whose checksum matches its uploaded evidence.

ENA-BUG-001 through ENA-BUG-010 have been remediated and regression-tested in
the locked local environment. Release authorization still requires the exact
clean revision to dispose of the four legacy complexity-gate items and pass the
CI supply-chain, hardened-container, proxy, and WebSocket jobs. If Docker/CI
evidence cannot be reproduced, the release verdict
is `incomplete`, never `pass`, even though the registered bug-remediation goal
is complete.
