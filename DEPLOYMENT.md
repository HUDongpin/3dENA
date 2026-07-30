# 3D ENA production deployment

The production target is **https://3dena.com**. The project is not deployed to
`www.ena3d.org`; that address appears only in historical audit material.

## Required production architecture

3D ENA is a stateful Shiny application. Production must run on a persistent
Linux host as this repository's `linux/amd64` container, with Posit Shiny
Server inside the container and nginx terminating TLS in front of it:

```text
browser -> nginx/TLS -> Shiny Server robust transport -> long-lived R worker
```

Do not deploy the production domain to a request-scoped or serverless function
runtime. A function can be recycled while the page is still open, which ends
the in-memory R session and produces Shiny's gray disconnected overlay.
`Dockerfile.vercel` and `vercel.json` are retained only for bounded,
non-authoritative previews until DNS cutover is complete; they explicitly use
the `ephemeral-preview` runtime profile and cannot pass the persistent-runtime
gate.

The preview image listens on Vercel's default container HTTP port `80`. Keep
that setting separate from the persistent Shiny Server image, which exposes
`3838` behind nginx. A Vercel build marked `Ready` is not a health check: open
the immutable preview URL and confirm an HTTP response before treating the
preview as usable.

Shiny Server's robust transport has a bounded 15-second opportunity to
reattach the browser to the **existing** R session. The app explicitly disables
Shiny's separate new-session reconnect fallback because replaying browser
inputs cannot restore uploaded temporary files, `reactiveValues`, or running
calculations. If the original session cannot be proven, the page stays blocked
and requires a reload.

## Security boundary

The public application accepts only version-1 `.ena3d.json` exchange uploads.
Native R serialization can contain executable objects, so `.RData`, `.rds` and
workspaces must never be passed from a browser to `load()` or `readRDS()` in
the Shiny worker.

Only the four reviewed fixtures packaged under `sample_data/` are available.
They are resolved as direct children of that directory, validated against ENA
schema and size limits, and mounted read-only in the production image. Adding a
sample is a source-code and supply-chain change: review it, run the full test
suite, commit it, and build a new immutable image. Do not mount a writable data
directory over `sample_data/`.

The Class 1 fixture remains inside that reviewed trust boundary and is packaged
only in a de-identified form: it contains one shared ENA rotation and
pseudonymous learner-period records, with original learner names, original ENA
unit labels, and message text excluded. This packaging decision does not
authorize identifiable classroom data on the public site; uploaded datasets
remain the user's responsibility to de-identify under the applicable
research-data policy.

The exchange contract is documented in `docs/ENA3D_EXCHANGE_V1.md`. The worker
reads bounded UTF-8 bytes with `jsonlite`, accepts only JSON scalars under a
strict columnar schema, assigns a small fixed set of rENA compatibility classes
server-side, and runs the normal ENA validator before a transactional state
change. It does not call native deserializers or evaluate file content.

Qwen-assisted interpretation is a separate, optional outbound boundary for the
3D ENA page. It is off in `compose.production.yaml`. When explicitly enabled,
the server can send only a freshly previewed, bounded aggregate evidence ledger
and optional user-entered research context after consent bound to that exact
envelope. Raw rows, ENA unit and participant identifiers, unit-level networks,
participant trajectories, and local dataset/request fingerprints are excluded.
See `docs/AI_INTERPRETATION.md` before enabling the feature.

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

The health response must contain the immutable build ID plus:

```json
{
  "runtime_profile": "persistent",
  "connection_policy": "host-existing-session-only",
  "shiny_server_version": "1.5.23.1030"
}
```

The image pins the Linux amd64 base image by digest and pins plus
checksum-verifies the exact Shiny Server package named by Posit's current
administrator guide; moving aliases or unverified downloads are not accepted.
Its reviewed entrypoint copies only an
allowlist of non-secret runtime settings into the non-root R worker's startup
profile; it never copies a raw provider key. The current Shiny Server binary
is available only for Linux amd64, so the Compose service fixes that platform
explicitly.

That command keeps AI disabled and requires no provider credential. To enable
Qwen after the data-governance review, use the optional overlay and a secret
file outside the repository:

```sh
export ENA3D_DASHSCOPE_SECRET_FILE=/etc/ena3d/secrets/dashscope_api_key
docker compose -f compose.production.yaml -f compose.qwen.yaml config --quiet
docker compose -f compose.production.yaml -f compose.qwen.yaml up -d
```

Do not place the key in Compose YAML or `.env`. The overlay mounts it read-only
and exposes only `/run/secrets/dashscope_api_key` to the application. Region,
endpoint, model, permissions, limits, and verification steps are documented in
`docs/AI_INTERPRETATION.md`.

The application runs as UID/GID 10001, with no Linux capabilities, a read-only
root filesystem, bounded temporary storage, a process limit, and container CPU
and memory limits. Its port binds only to loopback and must be reached through
the TLS reverse proxy.

## Preview analytics

Analytics is off by default in the persistent container. The Vercel preview
image explicitly selects Vercel's framework-independent Web Analytics
bootstrap and loads `/_vercel/insights/script.js`; the persistent host must not
emit that platform-relative script because it is not served there.

If a production analytics provider is selected later, review it as a separate
privacy and deployment change. Analytics must remain limited to traffic
metadata and page views; do not add research data, uploaded content,
participant identifiers, or ENA results as custom event properties.

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
6. confirm nginx permits the 6 MiB multipart envelope while the app still
   enforces the reviewed 2 MiB exchange-file and 5 MiB raw-file limits;
7. keep the WebSocket read/send timeout at 24 hours and verify that the
   upstream host or load balancer does not impose a shorter undocumented
   lifetime.

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

The optional Qwen overlay adds separate provider-request, response, context,
evidence, concurrency, per-session request, completion-token, thinking-token,
and timeout limits. Those limits bound exposure and workload; they do not
replace provider quota, billing alerts, institutional approval, or review of
Alibaba region/data residency. Invalid AI configuration—including invalid
optional AI resource settings—fails closed while the ENA application continues
without AI.

Increasing a limit is an operational change requiring a load test and a review
of worker memory, Plotly trace count and response time. The request-size limit
does not make native R serialization safe.

## Logs, monitoring and privacy

The app emits one-line `ena3d_event` records with UTC time, severity, event and
build ID. Logs intentionally contain aggregate sizes and trusted sample names,
not point tables or participant identifiers. The nginx template disables
access logging for `__sockjs__` transport paths so short-lived session
identifiers are not retained. Forward stdout/stderr and the remaining site
access logs to the chosen log platform, restrict operator access, and define a
retention period.

At minimum alert on:

- container restarts and failed health checks;
- `trusted_sample_load_failed`, `public_native_upload_blocked`,
  `public_exchange_rejected`, `public_raw_table_rejected`, and
  `public_raw_ena_rejected` events;
- sustained CPU/memory saturation;
- nginx 4xx/5xx and rate-limit rejections;
- TLS renewal failures.

When Qwen is enabled, also alert on sustained `ai_interpretation_failed`
events, request timeouts, provider authentication/quota errors, and unexpected
token or cost growth. AI logs must remain metadata-only: never add the API key,
evidence JSON, research context, or generated narrative to logs.

The public UI states that native R uploads are disabled; raw spreadsheets are
parsed as plain tables; the versioned JSON exchange is accepted; identifiable
research data must not be sent to the site; and operational logs follow the
deployment retention policy. Publish the final operator contact, privacy notice
and concrete log retention period before launch.

## Release and rollback checklist

1. Work from a clean Git tree; run `Rscript tests/check.R`.
2. Build with the full commit SHA in `ENA3D_BUILD_ID`.
3. Record the image digest and scan the image/SBOM for vulnerabilities.
4. Deploy to an always-on staging host and complete the proxy/browser smoke
   tests. Keep one page connected through nginx for at least six minutes,
   perform periodic server round trips, and confirm that its session proof
   remains unchanged.
5. Briefly interrupt the staging browser's network for less than 15 seconds.
   Accept recovery only when the same session proof returns and a new
   server-side action succeeds. Also test an interruption beyond the recovery
   window and confirm that the UI stays blocked and requires a reload.
   If Qwen is enabled, complete the aggregate-preview, consent, staleness,
   unit-selection refusal, failure-injection, log-redaction and billing checks
   in `docs/AI_INTERPRETATION.md`.
6. Deploy the exact tested digest to production.
7. Keep the preceding digest available and document the rollback command.
8. Confirm the visible build ID, health metadata, and startup log match the
   deployed digest.
9. Change DNS only after the new host passes TLS, health, six-minute
   connection, and short-interruption checks. Keep the previous deployment
   available until the same checks pass through the public production domain.

The `production-container` job in
`.github/workflows/pre-release-audit.yml` automates the six-minute hold,
periodic server round trips, a two-second nginx/TCP outage, same-session proof,
a post-recovery round trip, and a 22-second terminal nginx/TCP outage. Stopping
the proxy forces the established transport to close; a browser offline flag is
not accepted as evidence unless it actually closes that transport. The terminal
check requires both the custom blocking message and Shiny Server's native
Reload state to remain present. It writes only a bounded, identifier-free
`output/container/browser-session-audit.json`; browser request details,
console output, traces, video, screenshots, and nginx access logs are not
captured by this gate.

Repository changes alone do not perform the public cutover. It additionally
requires an always-on Linux amd64 host, TLS certificate automation, access to
the DNS zone, and an operator-selected immutable image digest.
