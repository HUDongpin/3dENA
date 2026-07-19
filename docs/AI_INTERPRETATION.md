# Qwen-assisted ENA interpretation

3D ENA can ask Alibaba Cloud Model Studio's Qwen API to interpret the
currently selected result on the **3D ENA page only**. The feature is optional
and fail-closed: it is unavailable unless an operator enables it and configures
a server-owned credential. All ENA analysis continues to work when AI is off
or the provider is unavailable.

The interpretation is an analytical aid, not a statistical result. Qwen can be
wrong. Each generated claim must cite evidence IDs from the locally constructed
ledger; users should verify those claims, caveats, and proposed checks against
the displayed ENA result before reporting them.

Quick mode favors a concise reading. Deep mode asks for a more developed
interpretation, and Challenge mode emphasizes rival explanations, caveats, and
next checks. The same evidence contract and output validation apply to all
three modes.

## Data boundary and consent

The server builds a bounded evidence ledger before any provider request. The
ledger can contain displayed axis names and anchors, sanitized code and
group/condition labels, aggregate sample sizes that meet the configured minimum
cell size, aggregate coordinate summaries, top aggregate edge weights,
aggregate statistical results, and aggregate trajectory metrics relevant to
the selected view.

It does **not** include raw upload rows, full ENA point or line-weight tables,
`ENA_UNIT` values, participant/pair identifiers, unit-level networks, or
participant-level trajectories. A Network view must select an aggregate group;
unit selections are rejected. Cells smaller than `ENA3D_AI_MIN_CELL_N` are
suppressed. Local dataset and request fingerprints are retained only for
staleness checks and metadata-only logging; neither fingerprint is included in
the preview or sent to the provider.

Before every request, the user must open and inspect the exact JSON data
envelope that will be placed in the provider prompt. The server binds consent
to a hash of that envelope. Changing the dataset, result, mode, language, or
research context invalidates the preview and clears consent; starting a request
consumes both so they cannot silently authorize another request.

This is a structural allowlist, not an automatic de-identification service.
Sanitization makes labels safe to transport and display; it does not anonymize
their meaning. Because group values, code labels, axis names, and optional
research context are sent, do not use names or IDs as group values and do not
request interpretation until the preview has been reviewed for indirect or
free-text identifiers.

Every request requires a fresh preview, an explicit user action, and a newly
checked consent control.
The provider receives only:

- the previewed aggregate evidence packet;
- the selected Quick, Deep, or Challenge mode and English or Chinese output
  language; and
- optional research context typed by the user.

Optional research context is user-controlled text. Users must not paste names,
IDs, raw data extracts, confidential records, credentials, or other information
that is not approved for transfer to Alibaba Cloud. Code names and group labels
may themselves disclose sensitive study details, so operators must complete
their own ethics, data-residency, institutional, and provider-contract review
before enabling the feature.

Model output is treated as untrusted data. The server rejects unexpected JSON,
unknown evidence references, oversized fields, unsupported confidence labels,
numeric literals not found in the cited evidence, and causal assertions when
the evidence does not explicitly declare a causal design. Accepted output is
displayed as plain text, never evaluated as HTML. Provider-side web search is
explicitly disabled for interpretation requests. A malformed, truncated, or
semantically unsupported completion fails closed; the application does not
make an automatic repair/retry call that could add unreviewed cost or egress.
Any displayed strong/moderate/tentative label is explicitly identified as
model confidence, not as a statistical evidence grade; the cited metrics and
the underlying ENA result remain authoritative.

## Configure Alibaba Cloud Model Studio

Create a pay-as-you-go Model Studio API key in the same region as the selected
endpoint. Alibaba states that API keys and endpoints are region-specific; see
[Select region and access domain](https://www.alibabacloud.com/help/en/model-studio/regions/)
and [Obtain an API key](https://www.alibabacloud.com/help/en/model-studio/get-api-key).
Workspace-dedicated domains are recommended by Alibaba for production, while
the integration also accepts the listed shared DashScope domains.

The client intentionally allows only these region/URL combinations:

| `ENA3D_QWEN_REGION` | Default shared `ENA3D_QWEN_BASE_URL` | Default model | Optional workspace URL |
| --- | --- | --- | --- |
| `cn-beijing` | `https://dashscope.aliyuncs.com/compatible-mode/v1` | `qwen3.7-max-2026-06-08` | `https://{workspace-id}.cn-beijing.maas.aliyuncs.com/compatible-mode/v1` |
| `ap-southeast-1` | `https://dashscope-intl.aliyuncs.com/compatible-mode/v1` | `qwen3.7-max-2026-06-08` | `https://{workspace-id}.ap-southeast-1.maas.aliyuncs.com/compatible-mode/v1` |
| `us-east-1` | `https://dashscope-us.aliyuncs.com/compatible-mode/v1` | `qwen3.7-max-us` | Not accepted |

The region, URL, and API key must match. HTTP, query strings, fragments,
alternate paths, IP literals, redirects, and other hosts are rejected. The
Beijing and Singapore default to the pinned
`qwen3.7-max-2026-06-08` snapshot for reproducibility; the moving
`qwen3.7-max` alias and the prior `qwen3.7-max-2026-05-20` snapshot are accepted
only when explicitly configured. US inference is restricted to
`qwen3.7-max-us`; a global or non-US model ID is rejected even on the Virginia
access domain. Alibaba documents the regional deployment-scope rule in
[Select region and access domain](https://www.alibabacloud.com/help/en/model-studio/regions/)
and the current Max IDs in its
[supported models](https://www.alibabacloud.com/help/en/model-studio/models).
Model availability, snapshot retirement, and billing can change; confirm the
exact approved model in the chosen region before deployment. A configured but
unavailable model fails the request without changing the ENA analysis.

Exactly one credential source is allowed:

- `DASHSCOPE_API_KEY_FILE` is recommended for production secret mounts; or
- `DASHSCOPE_API_KEY` may be used for a local, server-owned environment.

Never put the API key in `.env`, `.Renviron`, Compose YAML, source control,
logs, screenshots, browser code, or Shiny inputs. The application loads the key
only in the isolated request process and redacts it from configuration,
provider errors, result metadata, and logs.

## Docker Compose enablement

The base deployment is explicitly AI-off and needs no secret:

```sh
export ENA3D_BUILD_ID="$(git rev-parse --verify HEAD)"
export ENA3D_APP_VERSION="$(tr -d '\r\n' < VERSION)"
docker compose -f compose.production.yaml up -d --build
```

For Qwen, place a file containing only the API key outside the repository. On
a Linux Compose host, prepare it for the container's UID/GID 10001 and prevent
access by other users. Docker implements file-backed Compose secrets as bind
mounts, so its `uid`, `gid`, and `mode` service attributes cannot remap the host
file; prepare the host ownership and mode explicitly and validate readability
on the actual host. See Docker's
[Compose secrets reference](https://docs.docker.com/reference/compose-file/services/#secrets).

```sh
sudo install -o 10001 -g 10001 -m 0400 \
  /secure/source/dashscope_api_key \
  /etc/ena3d/secrets/dashscope_api_key
export ENA3D_DASHSCOPE_SECRET_FILE=/etc/ena3d/secrets/dashscope_api_key
```

Copy `.env.example` to an ignored operator configuration file if useful, but
do not copy the key into it. If changing regions, change both
`ENA3D_QWEN_REGION` and `ENA3D_QWEN_BASE_URL` to a matching row above. Validate
the merged configuration, then apply the optional overlay:

```sh
docker compose -f compose.production.yaml -f compose.qwen.yaml config --quiet
docker compose -f compose.production.yaml -f compose.qwen.yaml up -d --build
```

The overlay mounts the host file at `/run/secrets/dashscope_api_key` and sets
only that container path in `DASHSCOPE_API_KEY_FILE`. It never interpolates the
key into the Compose model. Reverting to the base file disables AI without
affecting any other view:

```sh
docker compose -f compose.production.yaml -f compose.qwen.yaml down
docker compose -f compose.production.yaml up -d
```

## Configuration reference

The following variables are validated during startup or immediately before a
request. Invalid provider, credential-source, or local resource-budget
configuration fails closed and disables only the integration; the core ENA
application still starts. No invalid case weakens a boundary or falls back to
sending more data.

| Variable | Default | Accepted range or meaning |
| --- | ---: | --- |
| `ENA3D_AI_ENABLED` | `false` | Boolean; must be true as well as having one credential source |
| `ENA3D_QWEN_REGION` | `cn-beijing` | `cn-beijing`, `ap-southeast-1`, or `us-east-1` |
| `ENA3D_QWEN_BASE_URL` | Region default | Matching allowlisted HTTPS compatible-mode base URL above |
| `ENA3D_QWEN_MODEL` | Region-specific above | Approved Qwen 3.7 Max ID: Beijing/Singapore accept `qwen3.7-max-2026-06-08`, `qwen3.7-max-2026-05-20`, or explicit moving alias `qwen3.7-max`; US accepts only `qwen3.7-max-us` |
| `DASHSCOPE_API_KEY_FILE` | Unset | Server path to a 1–4096-byte secret file; its trimmed key must be 8–2048 bytes; mutually exclusive with `DASHSCOPE_API_KEY` |
| `DASHSCOPE_API_KEY` | Unset | Direct 8–2048-byte server environment secret for local use; mutually exclusive with `DASHSCOPE_API_KEY_FILE` |
| `ENA3D_QWEN_TIMEOUT_SECONDS` | `60` | 5–120 seconds |
| `ENA3D_QWEN_CONNECT_TIMEOUT_SECONDS` | `10` | 1–30 seconds |
| `ENA3D_QWEN_MAX_REQUEST_BYTES` | `262144` | 4 KiB–1 MiB |
| `ENA3D_QWEN_MAX_RESPONSE_BYTES` | `262144` | 1 KiB–1 MiB |
| `ENA3D_QWEN_MAX_CONTEXT_BYTES` | `8192` | 0–32768 UTF-8 bytes in the provider request |
| `ENA3D_QWEN_MAX_COMPLETION_TOKENS` | `4096` | 1024–16384 total output tokens, including thinking and JSON answer |
| `ENA3D_QWEN_THINKING_BUDGET` | `1536` | 128–8192 thinking tokens; must leave at least 512 within the completion cap for the JSON answer |
| `ENA3D_QWEN_TEMPERATURE` | `0.1` | 0–0.5 |
| `ENA3D_AI_MIN_CELL_N` | `5` | 2–100; aggregates below this size are suppressed |
| `ENA3D_AI_TOP_N` | `10` | 1–25 ranked aggregate edges/items |
| `ENA3D_AI_CONTEXT_MAX_CHARS` | `1500` | 100–5000 user-entered characters retained locally before request construction |
| `ENA3D_AI_MAX_CONCURRENT_JOBS` | `4` | 1–16 isolated Qwen processes per app process |
| `ENA3D_AI_MAX_REQUESTS_PER_HOUR` | `10` | 1–100 requests per Shiny session per rolling hour |
| `ENA3D_AI_MAX_EVIDENCE_BYTES` | `65536` | 4 KiB–256 KiB aggregate public payload |

`ENA3D_QWEN_MAX_CONTEXT_BYTES` bounds the complete research-context field at
the API boundary, while `ENA3D_AI_CONTEXT_MAX_CHARS` bounds user input earlier
in the Shiny module. The client uses Qwen's `max_completion_tokens` plus an
explicit thinking budget and does not use the deprecated `max_tokens` field
with structured JSON output. Provider quotas, token charges, and rate limits
still apply independently. Set billing alerts and review usage before enabling
a public deployment.

`ENA3D_DASHSCOPE_SECRET_FILE` is not read by the R application. It is a
Compose-host interpolation variable that identifies the source file mounted at
the container path assigned to `DASHSCOPE_API_KEY_FILE`.

## Failure and lifecycle behavior

- A missing credential leaves the panel disabled. Invalid Qwen configuration
  records only a sanitized configuration event and disables the panel. The
  rest of 3D ENA remains available.
- A timeout, network/authentication error, provider rejection, malformed model
  response, or local limit violation produces no change to the ENA dataset or
  analytical result.
- Leaving the 3D ENA page, changing the selected analysis while a request is
  running, or cancelling explicitly terminates that request. A completed
  interpretation is marked stale when its underlying analysis changes.
- Operational logs contain bounded metadata such as view, model, latency,
  token counts, a truncated request hash, and error class. They do not contain
  the credential, evidence payload, research context, or generated narrative.
- The health document's `ai_enabled` field means the feature flag and a
  credential source are configured. It is not a provider-reachability check.

## Verification checklist

1. Run `Rscript tests/check.R` and build the immutable production image.
2. Start `compose.production.yaml` alone. Confirm
   `/ena3d-health/healthz.json` reports `"ai_enabled":false` and every ENA view
   remains usable.
3. Run `docker compose -f compose.production.yaml -f compose.qwen.yaml config`
   and inspect the merged configuration. It may show the host secret path and
   container secret name, but must not contain the API key.
4. Start with the overlay. Confirm the health document reports
   `"ai_enabled":true`; this verifies configuration presence, not a successful
   billable request.
5. On the 3D ENA page, load reviewed non-identifiable test data. Open the
   interpreter, inspect the JSON preview for aggregate-only content, confirm
   consent, and make one controlled request.
6. Confirm every claim cites visible evidence IDs, output is plain text, and
   switching the result marks the interpretation stale. Confirm unit-network
   selections are refused and the interpreter is not exposed on other pages.
7. Review logs for timing/token metadata and confirm no API key, research
   context, evidence JSON, raw row, or identifier was recorded.
8. Exercise failure cases in staging: missing secret, mismatched regional key
   and URL, unreachable provider, timeout, malformed response, and per-session
   rate limit. In every case, the active ENA result must remain unchanged.
