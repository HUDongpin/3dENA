# 3D ENA browser smoke tests

These tests start the local Shiny app and exercise the reviewed, bundled
`newfrat_enaset.Rdata` sample in Chromium at desktop width, a 768-pixel tablet
width, and an exact 390-pixel mobile width. The desktop and tablet projects
also import the 12 KB synthetic
`fixtures/small-valid.ena3d.json` exchange fixture. It never uploads a native R
serialization file or calls the retired `www.ena3d.org` deployment.

The smoke path verifies:

- the internal `/ena3d-health/healthz.json` endpoint before browser interaction
  (nginx exposes this as `/healthz` on 3dena.com);
- the `.ena3d.json`-only browser-import boundary and explicit native-R warning;
- every Model sub-tab, including `No Network` as the initial Networks state;
- trajectory selectors, repeated-ID coverage, Plot Tools guidance, the
  server-enforced bootstrap input range of 200–500, and a completed real
  centroid-path run with 14 on-segment direction arrows, its Plotly result,
  and export controls;
- key tab, camera, sidebar and fullscreen accessibility contracts;
- uncaught page errors and unexpected `console.error` messages.

Any browser console error fails the test. Test timeouts are finite so a stalled
Shiny computation cannot occupy a CI runner indefinitely.

## Run locally

Use R 4.4.1 with the locked R dependencies restored first:

```sh
Rscript renv/bootstrap.R
npm ci
npx playwright install chromium
npm run test:e2e
```

Playwright starts `Rscript tests/e2e/start-app.R` automatically and waits for
the health endpoint. If port 3838 is already in use, select another unprivileged
port with `E2E_PORT=43838 npm run test:e2e`.

Screenshots, traces, video and the HTML report are written only under
`output/playwright/`. CI does not upload that directory because browser output
can contain rendered research data. It remains ephemeral on the runner.

The committed exchange fixture is generated from the smallest reviewed sample
and replaces all person-like labels with synthetic unit IDs. Regenerate it
after an intentional exchange-schema change with:

```sh
Rscript tests/e2e/fixtures/generate-small-exchange.R
```
