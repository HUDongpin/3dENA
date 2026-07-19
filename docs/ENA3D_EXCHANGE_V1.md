# 3D ENA exchange format, version 1

`*.ena3d.json` is the only user-supplied dataset format accepted by the public
3D ENA application. It is a plain-data interchange format, not a native R
serialization format. The web worker reads it with `jsonlite`, applies the
schema and resource checks below, constructs a new list-based `ena.set`, and
then runs the application's normal `ena3d_validate_ena_object()` validator.

Native `.RData`, `.rds`, workspaces, expressions, functions, environments and
user-declared R classes are not part of this format.

The companion [JSON Schema](ena3d-exchange-v1.schema.json) describes the
machine-checkable structure. Cross-column semantics such as equal row counts,
metadata alignment, complete adjacency and edge order are additionally
enforced by the authoritative R validator described below.

## Top-level object

Every field is required. Unknown fields and duplicate JSON object keys are
errors.

```json
{
  "format": "ena3d-exchange",
  "version": 1,
  "dimensions": ["SVD1", "SVD2", "SVD3"],
  "group_variables": ["Week", "Name"],
  "tables": {
    "meta_data": {"columns": []},
    "points": {"columns": []},
    "line_weights": {"columns": []},
    "nodes": {"columns": []},
    "adjacency_key": {"columns": []}
  }
}
```

- `format` must be exactly `ena3d-exchange`.
- `version` must be the JSON number `1`.
- `dimensions` is an ordered array of at least three unique column names.
- `group_variables` is a non-empty ordered array of unique metadata column
  names. The first value is the primary grouping variable.
- `tables` must contain exactly the five named tables above.

A future incompatible representation must use a new integer version. Readers
must reject versions they do not implement; they must not guess or silently
migrate them.

## Columnar tables

Each table contains exactly one field, `columns`, whose value is a non-empty
array. Each column contains exactly these fields:

```json
{
  "name": "Week",
  "type": "integer",
  "values": [1, 2, null]
}
```

`type` is exactly one of `logical`, `integer`, `double`, `character`, `date`,
`datetime`, `difftime`, `factor`, or `ordered`. Values must match the declared
type; `null` is the only missing-value marker. Integers must fit a signed
32-bit R integer. Every non-null number must be finite. Nested objects or arrays
are never valid cell values. Column names must be unique, valid UTF-8,
non-empty, free of control characters, and no longer than 256 UTF-8 bytes. All
columns within a table must have the same non-zero row count.

The special scalar types have fixed, non-executable metadata:

- `date` values are ISO 8601 `YYYY-MM-DD` strings.
- `datetime` values are finite seconds since the Unix epoch and the column has
  one additional `timezone` field. It must be `UTC` or an installed IANA
  timezone name.
- `difftime` values are finite numeric amounts and the column has one additional
  `units` field: `secs`, `mins`, `hours`, `days`, or `weeks`.
- `factor` and `ordered` values are strings and the column has one additional
  `levels` array. Every non-null value must occur in that unique ordered array;
  unused levels are retained. `ordered` restores ordered-factor semantics.

Other column types have exactly `name`, `type`, and `values`; special types
have only their one documented extra field. Arbitrary attributes or R class
names are rejected.

The following order and alignment rules are mandatory:

1. `meta_data` contains `ENA_UNIT` and every declared group variable.
2. `points` contains the metadata columns first, in the exact `meta_data`
   order, followed by the declared dimensions in exact order.
3. `nodes` contains `code` first, followed by the same dimensions. `code` is a
   character column of unique, non-missing node names.
4. `adjacency_key` has exactly two rows. Each column is a character endpoint
   pair, and its column name is exactly `<from> & <to>`.
5. `line_weights` contains metadata columns first, followed by numeric edge
   columns in exactly the `adjacency_key` order and with the same names.
6. `meta_data`, `points`, and `line_weights` have equal row counts. Every
   metadata column has identical declared type and value-for-value alignment
   across those rows.
7. The adjacency key contains every unordered pair of node codes exactly once,
   has no self-pairs, and has no unknown nodes. Its edge count is therefore
   `choose(number_of_nodes, 2)`.

Point dimensions may use `null` under the application's documented
missing-point policy. Node dimensions and line-weight edges must be complete
and finite; `null`, `NaN` and infinite values are rejected.

The server restores only fixed compatibility markers: metadata columns receive
`ena.metadata`, dimensions receive `ena.dimension`, and edge columns receive
`ena.co.occurrence`. No class name or executable object is read from JSON.

## Limits and parser behavior

The public server reads at most 2 MiB before JSON parsing. It also rejects
excessive nesting, invalid UTF-8, a byte-order mark, invalid syntax, duplicate
or unknown fields, extra columns, excessive rows/nodes/dimensions/cells/group
levels/units, and objects that fail the regular ENA validator. The configured
exchange limit has a hard operational ceiling of 10 MiB, but increasing it
requires coordinated application, proxy and load-testing changes.

The canonical compact conversion of the three bundled fixtures is below 2 MiB
(the largest, `newfrat_enaset.Rdata`, is currently about 0.90 MB), so files
produced by the supported converter can be uploaded with the default limit.

Validation completes before active application state changes. A rejected file
therefore cannot replace the previously active dataset.

## Trusted offline conversion

Only convert an `.RData` file that is local and trusted:

```sh
Rscript tools/convert_trusted_rdata_to_ena3d_json.R \
  --trusted-native-input input.RData output.ena3d.json
```

Loading native R serialization can execute code. Run the converter outside the
public web worker, preferably in a disposable network-disabled environment for
legacy material. The explicit flag is an acknowledgement of that trust
decision. The converter validates the ENA object, writes canonical compact
JSON atomically, prints SHA-256 for both input and output, and creates
`output.ena3d.json.sha256`.
