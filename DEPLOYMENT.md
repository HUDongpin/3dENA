# ENA 3D production deployment

The production target is **https://3dena.com**. The project is not deployed to
`www.ena3d.org`; that address appears only in historical audit material.

## Security boundary

The public application accepts only version-1 `.ena3d.json` exchange uploads.
Native R serialization can contain executable objects, so `.RData`, `.rds` and
workspaces must never be passed from a browser to `load()` or `readRDS()` in
the Shiny worker.

Only the three reviewed fixtures packaged under `sample_data/` are available.
They are resolved as direct children of that directory, validated against ENA
schema and size limits, and mounted read-only in the production image. Adding a
sample is a source-code and supply-chain change: review it, run the full test
suite, commit it, and build a new immutable image. Do not mount a writable data
directory over `sample_data/`.

The exchange contract is documented in `docs/ENA3D_EXCHANGE_V1.md`. The worker
reads bounded UTF-8 bytes with `jsonlite`, accepts only JSON scalars under a
strict columnar schema, assigns a small fixed set of rENA compatibility classes
server-side, and runs the normal ENA validator before a transactional state
change. It does not call native deserializers or evaluate file content.

Re-enabling the old `.RData` file input is not an acceptable shortcut. The
trusted converter in `tools/` is an offline operator tool, not a web endpoint.
Run it only for local trusted input, preferably in a disposable non-privileged
sandbox with no network access, a read-only host file system, and hard CPU,
memory and time limits.

## Reproducible build

The lockfile records R 4.4.1 and all runtime package versions. Restore it with:

```sh
Rscript renv/bootstrap.R
Rscript tests/check.R
```

Build from a clean, committed tree and use the immutable commit or release tag
as the build identifier:

```sh
export ENA3D_BUILD_ID="$(git rev-parse --verify HEAD)"
export ENA3D_APP_VERSION="$(tr -d '\r\n' < VERSION)"
docker compose -f compose.production.yaml build --pull
docker compose -f compose.production.yaml up -d
curl --fail http://127.0.0.1:3838/ena3d-health/healthz.json
```

The application runs as UID/GID 10001, with no Linux capabilities, a read-only
root filesystem, bounded temporary storage, a process limit, and container CPU
and memory limits. Its port binds only to loopback and must be reached through
the TLS reverse proxy.

## TLS and reverse proxy

`deploy/nginx/3dena.com.conf.example` is a reviewed starting point. Before use:

1. provision and automatically renew one certificate whose Subject Alternative
   Names include both `3dena.com` and `www.3dena.com`;
2. install the configuration in nginx's `http` context;
3. run `nginx -t` and verify HTTP and `https://www.3dena.com/*` canonically
   redirect to the same path at `https://3dena.com/*`;
4. verify Shiny WebSocket upgrades through nginx;
5. verify `/healthz`, application load, sample switching, trajectory analysis,
   downloads, sidebar toggling and fullscreen in a supported browser;
6. confirm nginx and the app both enforce the 2 MB request-body limit.

Do not add a `server_name` for `www.ena3d.org`. DNS, certificates and redirects
for unrelated historical domains are outside this deployment.

The application currently contains inline Shiny JavaScript and CSS. Introduce a
Content-Security-Policy only after testing it in report-only mode; a strict
policy applied without that work can break Shiny and Plotly. Do not silently
fall back to a broad wildcard policy.

## Configuration and resource budgets

Production defaults are recorded in `compose.production.yaml`. Every limit is
also checked in R before an object becomes active:

- trusted file and post-load object bytes;
- public `.ena3d.json` bytes before parsing (2 MiB by default);
- raw `.csv`, `.xlsx`, and `.xls` bytes before parsing (5 MiB by default),
  uncompressed Excel archive bytes, plus raw row, column, and cell limits;
- number of saved objects;
- point rows, nodes, dimensions and metadata columns;
- total cells across core ENA tables;
- grouping levels and unique ENA units.

Increasing a limit is an operational change requiring a load test and a review
of worker memory, Plotly trace count and response time. The request-size limit
does not make native R serialization safe.

## Logs, monitoring and privacy

The app emits one-line `ena3d_event` records with UTC time, severity, event and
build ID. Logs intentionally contain aggregate sizes and trusted sample names,
not point tables or participant identifiers. Forward stdout/stderr to the
chosen log platform, restrict operator access, and define a retention period.

At minimum alert on:

- container restarts and failed health checks;
- `trusted_sample_load_failed`, `public_native_upload_blocked`,
  `public_exchange_rejected`, `public_raw_table_rejected`, and
  `public_raw_ena_rejected` events;
- sustained CPU/memory saturation;
- nginx 4xx/5xx and rate-limit rejections;
- TLS renewal failures.

The public UI states that native R uploads are disabled; raw spreadsheets are
parsed as plain tables; the versioned JSON exchange is accepted; identifiable
research data must not be sent to the site; and operational logs follow the
deployment retention policy. Publish the final operator contact, privacy notice
and concrete log retention period before launch.

## Release and rollback checklist

1. Work from a clean Git tree; run `Rscript tests/check.R`.
2. Build with the full commit SHA in `ENA3D_BUILD_ID`.
3. Record the image digest and scan the image/SBOM for vulnerabilities.
4. Deploy to staging and complete the proxy/browser smoke tests.
5. Deploy the exact tested digest to production.
6. Keep the preceding digest available and document the rollback command.
7. Confirm the visible build ID and startup log match the deployed digest.
