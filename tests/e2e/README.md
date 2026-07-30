# 3D ENA browser smoke tests

These tests start the local Shiny app and exercise the reviewed, bundled
`newfrat_enaset.Rdata` sample in Chromium, Firefox, and WebKit at desktop width,
plus Chromium at a 768-pixel tablet width and an exact 390-pixel mobile width.
The desktop and tablet projects also import the 12 KB synthetic
`fixtures/small-valid.ena3d.json` exchange fixture. It never uploads a native R
serialization file or calls the retired `www.ena3d.org` deployment.

The smoke path verifies:

- the internal `/ena3d-health/healthz.json` endpoint before browser interaction
  (nginx exposes this as `/healthz` on 3dena.com);
- the `.ena3d.json`-only browser-import boundary and explicit native-R warning;
- transactional rollback after a duplicate-field exchange file is rejected;
- every Model sub-tab, including `No Network` as the initial Networks state;
- trajectory selectors, repeated-ID coverage, Plot Tools guidance, the
  server-enforced bootstrap input range of 200–500, and a completed real
  centroid-path run with 14 on-segment direction arrows, its Plotly result,
  CSV/ZIP export bytes and manifest contents;
- preservation of the completed trajectory while visiting real Stats output,
  and invalidation after a different valid exchange dataset becomes active;
- key tab, camera, sidebar and fullscreen accessibility contracts;
- keyboard activation and serious/critical automated WCAG checks on Home, Data,
  and Stats at all three Chromium viewport sizes;
- uncaught page errors and unexpected `console.error` messages.

Any browser console error fails the test. Test timeouts are finite so a stalled
Shiny computation cannot occupy a CI runner indefinitely.

## Run locally

Use R 4.4.1 with the locked R dependencies restored first:

```sh
Rscript renv/bootstrap.R
npm ci
npx playwright install chromium firefox webkit
npm run lint
npm run test:e2e
```

Playwright starts `Rscript tests/e2e/start-app.R` automatically and waits for
the health endpoint. If port 3838 is already in use, select another unprivileged
port with `E2E_PORT=43838 npm run test:e2e`.

Use `npm run test:e2e:chromium` for the three responsive Chromium projects,
`npm run test:e2e:desktop` for the three desktop browser engines, or
`npm run test:e2e:repeat` for the full three-pass flake-detection matrix.

Accessibility checks are release-audit detectors rather than ordinary PR
smoke tests. `npm run test:e2e:a11y` is release-blocking; the current command
exits non-zero when serious/critical findings exist. For triage without a
non-zero exit, run `npm run test:e2e:a11y:report`. Both modes write sanitized
rule, selector, impact, and contrast evidence to
`output/playwright/audit/accessibility-<project>.json`; screenshots, traces,
and video are disabled for these audit projects.

The pre-release soak is excluded from ordinary test projects. It defaults to
30 minutes and an isolated port, and can be shortened for local verification:

```sh
ENA3D_AUDIT_SOAK_MINUTES=2 npm run test:e2e:soak
```

The soak repeatedly switches all four bundled datasets and top-level views,
completes real trajectories where repeated IDs exist, verifies the scientific
cross-sectional guard otherwise, probes isolated-bootstrap cancellation,
checks health, and fails on browser console or page errors.

Screenshots, traces, video and the HTML report are written only under
`output/playwright/`. CI does not upload that directory because browser output
can contain rendered research data. It remains ephemeral on the runner.

The committed exchange fixture is generated from the smallest reviewed sample
and replaces all person-like labels with synthetic unit IDs. Regenerate it
after an intentional exchange-schema change with:

```sh
Rscript tests/e2e/fixtures/generate-small-exchange.R
```
