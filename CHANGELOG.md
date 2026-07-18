# Changelog

All notable ENA 3D changes are recorded here. The application also displays
`VERSION` and the immutable deployment build ID supplied by `ENA3D_BUILD_ID`.

## 0.2.0-dev — unreleased

### Added

- Ordered 2D/3D centroid trajectory analysis with explicit time, entity,
  cohort, missing-value, distance-space and bootstrap policies.
- Simple two-wing direction arrowheads visibly ending at each destination node
  edge through a marker-over-arrow layer for every finite non-zero trajectory
  step, now using a compact head length, enabled by default in both 3D and 2D
  with a display toggle.
- Distinct ordered-period colors for centroid nodes, with group-colored
  outlines and a responsive named node legend beside the plot on desktop and
  below it on mobile.
- ID-matched paired trajectory comparison, selected-time network context,
  diagnostics, provenance, CSV exports and an analysis-bundle ZIP.
- Direct independent-group centroid-path comparison with side-specific
  participant-cluster bootstrap intervals, whole-trajectory label-permutation
  p-values, finite-sample correction, and Holm multiplicity adjustment.
- A versioned, non-executable `.ena3d.json` exchange boundary and an offline
  trusted-native conversion workflow. Anonymous `.RData`/`.rds` upload remains
  prohibited.
- Reproducible R dependencies, hardened container/reverse-proxy examples for
  `3dena.com`, health/build metadata, GitHub Actions and desktop/mobile browser
  smoke tests.

### Fixed

- Centroids now use the selected axes, including non-default ENA dimensions.
- Change uses the selected variable, clears dataset-dependent state and builds
  only the requested value with a bounded cache.
- Paired statistics match by explicit ID; effect directions, finite-sample
  counts, study-design constraints and multiple-testing adjustment are stated.
- Overall grouping/hover lookup, dynamic colors, fullscreen/sidebar behavior,
  Plotly autoranges, camera titles and Network selector lifecycle defects.
- Dense networks batch edges into bounded Plotly traces while retaining
  per-edge hover values.
- Dataset loading is transactional, schema-validated and resource-bounded.

### Security and privacy

- Browser-controlled native R serialization is rejected before
  deserialization.
- Spreadsheet-formula injection is neutralized in analytical CSV exports.
- Production defaults use a non-root, read-only, resource-limited container
  and aggregate-only structured logs.
