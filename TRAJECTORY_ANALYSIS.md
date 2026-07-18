# Centroid Trajectory Analysis

This guide documents the centroid trajectory feature implemented in
[trajectory_analysis.R](R/trajectory_analysis.R),
[trajectory_plot.R](R/trajectory_plot.R), and
[app_module_trajectory.R](R/app_module_trajectory.R). It covers the analytical
definitions, application workflow, assumptions, exports, and current
limitations.

## What the feature estimates

A trajectory is an ordered sequence of mean ENA point locations. For each
group and requested time, the analysis first reduces duplicate rows to one
participant-period estimate and then calculates a centroid across participants.
Adjacent centroids are connected in their explicit time order.

The analysis returns data. The Plotly line is only a view of that data; it does
not smooth, interpolate, rescale, or recalculate the trajectory.

The feature answers descriptive questions such as:

- Where is the cohort's mean ENA position at each requested time?
- How far does that mean position move between adjacent times?
- How much path distance accumulates over the observed sequence?
- How does the result change under an available or complete cohort?
- How uncertain are the centroid and movement estimates under participant
  resampling?
- For the same matched entities, how do two condition paths differ?

It does not by itself establish that movement is statistically meaningful,
caused by an intervention, or representative of every participant.

## Required inputs

### ENA point table

The application reads raw coordinates from `ena_obj$points`. Display scale,
camera position, 2D/3D mode, marker sizes, and network-overlay settings never
enter the analytical calculation.

All points used in one analysis must be expressed in the same ENA rotation.
Coordinates from independently fitted or independently rotated ENA sets are not
directly comparable without an explicitly justified alignment procedure.

### Time/order variable

The time variable identifies the trajectory slices. The application displays
an explicit comma- or line-separated order and requires every observed
non-missing time value to appear exactly once.

The generated application order is:

- ascending for numeric, logical, `Date`, and date-time values;
- factor-level order for factors; and
- lexical order for character values.

POSIX date-time values are displayed with an explicit numeric UTC offset and
microsecond precision plus an exact epoch token. Non-integer numeric values
carry an exact hexadecimal token. These generated labels round-trip through the
application parser without changing the underlying instant or double value.

Always review character and unordered-factor values. Labels such as `week 1`,
`week 10`, and `week 2` do not have a reliable substantive order under ordinary
lexical sorting.

The direct R API behaves slightly differently when `order = NULL`: numeric and
date-like values are sorted, factors use their levels, and character values use
stable first appearance with a diagnostic warning. Supplying `order` is the
reproducible choice. Both the UI and direct API may include expected but
globally unobserved periods in `order`; those rows are returned as missing
periods. Generated factor orders retain unused levels for this purpose. The
parser still requires every actually observed value to appear exactly once.
For a paired comparison, the two time columns must have the same semantic
family; for example, factor versus character is rejected rather than silently
coerced into a combined order.

### Entity ID

The entity ID is the repeated observational unit carried across time. It must
be stable and substantively meaningful. For example, in the bundled `newfrat`
data, `Name` is the repeated person identifier; `ENA_UNIT` combines week and
name and is therefore not the appropriate longitudinal ID.

The participant-cluster bootstrap and paired comparison both depend on this
field. A changing, duplicated, or time-embedded identifier changes the
estimand and may destroy pairing. Raw IDs also need a declared namespace: the
same text may mean one entity shared across groups, or a group-local identifier
that is reused for unrelated entities. That distinction determines the
bootstrap design.

### Optional group/condition

Selecting a group variable creates one independent descriptive path for each
observed level. The application also lets the user select two levels, A and B,
for an exact ID-matched paired comparison.

The descriptive paths do not require the same IDs in every level. The paired
comparison does: entities absent from either condition at a given time are not
used in that paired slice. The application reports raw-ID overlap and exact
ID-time overlap, and requires an explicit confirmation that an equal raw ID in
A and B denotes the same physical entity before running the comparison.

### ENA dimensions

The selected dimensions are the X, Y, and Z axes active when **Run / recompute
trajectory** is selected. They determine the exported centroid and delta
coordinate columns. A 2D view selects two of these already-computed axes; it
does not change the analytical result.

The full rotation is the complete set of ENA dimension columns identified from
the loaded set. It is used only when full-space distance is selected.

Time, group, and dimension names must not collide with the generated output
schema, such as `n_total`, `time_order`, `centroid_*`, or bootstrap
`*_lower` fields. The APIs reject such collisions before constructing a
result instead of silently overwriting a source column. Export also reserves
the `.analysis_*` namespace for provenance and stops on a source/output
collision rather than overwriting analytical data.

## Analytical construction

### 1. Participant-period reduction

For a group `g`, time `t`, and entity `i`, every valid raw coordinate row is
first reduced to one participant-period point. With equal weights this is the
ordinary mean of duplicate rows:

```text
X[g,t,i] = mean of valid raw rows for entity i in group g at time t
```

Consequently, an entity with three duplicate rows does not receive three times
the influence of an entity with one row. The output reports duplicates in
`n_duplicate_rows` and emits a `duplicate_id_time` diagnostic.

The direct R API accepts a weight column or numeric weight vector. It uses a
weighted participant-period mean, and the participant's centroid weight is the
mean of that participant's positive row weights. The application UI currently
uses equal participant weights and does not expose weighting controls.

### 2. Centroid

For the entities admitted by the cohort and missing-value policies, the
centroid is:

```text
C[g,t] = sum_i(w[g,t,i] * X[g,t,i]) / sum_i(w[g,t,i])
```

With the application defaults, every used entity has weight one. Coordinate
columns are named `centroid_<dimension>`.

A centroid describes the mean location only. Different point clouds can have
the same centroid, so inspect sample counts, uncertainty, and—when relevant—the
underlying distribution before interpreting the path.

### 3. Coordinate movement

For every selected dimension `d` after the first time:

```text
delta_d[g,t] = centroid_d[g,t] - centroid_d[g,t-1]
```

The first delta is `NA`. `dx`, `dy`, and `dz` are aliases for the first three
selected dimension deltas.

### 4. Step and cumulative distance

For the configured distance dimensions `D`:

```text
step_distance[g,t] = sqrt(sum_d in D((C_d[g,t] - C_d[g,t-1])^2))
```

At the first requested time, the step is zero when its centroid is valid.
`cumulative_distance` starts at zero and adds valid adjacent step distances.
The method does not bridge an invalid or missing centroid: the affected step is
`NA` and no interpolated segment is added. After the first such discontinuity,
`cumulative_distance` remains `NA` for all later rows in that group because a
single continuous total from the requested origin is no longer defined. A
later `step_distance` is still reported when its own adjacent centroid pair is
valid; it does not restart the cumulative path.

These are Euclidean distances in the chosen ENA coordinate space. They are not
geodesic distances, network-edit distances, or participant-level travel
distances. In particular, a centroid path is not the average of individual path
lengths.

### 5. Elapsed interval and speed

`elapsed_interval` is the difference between adjacent ordered time values:

- days for `Date`;
- seconds for POSIX date-time;
- the existing units for `difftime`;
- generic time units for numeric values; and
- `NA` for factor or character sequences.

Speed is defined only for a finite, positive elapsed interval:

```text
speed[g,t] = step_distance[g,t] / elapsed_interval[g,t]
```

The first speed is `NA`. A non-positive interval produces `NA` and a diagnostic.
For labeled phases without quantitative spacing, interpret the step distance,
not speed.

## Cohort policies

### Available at each time

`cohort_policy = "available"` uses every valid entity present in each
group-time slice. The sample can therefore change over time. This maximizes use
of available observations but can make centroid movement partly reflect
composition change rather than within-entity change.

The result emits `changing_cohort` when the used ID set differs across periods.
Review `n_used` and the diagnostic before attributing a path segment to temporal
change.

### Complete cohort across time

`cohort_policy = "complete"` uses, within each trajectory group, only entities
with a valid participant-period point at every requested time. This keeps cohort
composition constant, but may reduce sample size and change the target
population to complete cases.

`n_cohort_excluded` reports valid slice entities removed by this rule. In a
paired comparison, completeness is evaluated after exact A/B matching, so the
same matched entities are used at every requested time.

Neither policy repairs missing observations. The choice changes the estimand
and should be justified before viewing results.

## Missing-value policy and counts

### Complete analytical rows

`na_policy = "complete"` excludes invalid analytical rows. Invalid values
include missing or non-finite keys and selected centroid dimensions, missing or
non-finite weights, and negative weights. If duplicate rows exist, an entity can
still contribute when at least one positive-weight row has complete selected
coordinates. A zero-weight row is valid but contributes no weight.

When full-space distance is requested, extra full-rotation coordinates do not
silently remove an otherwise valid entity from the selected-axis centroid.
Instead, the selected centroid and its `n_used` cohort are retained, the affected
full-space distance is `NA`, and `full_distance_incomplete` is emitted. This
keeps the displayed centroid estimand identical when switching distance spaces.

### Stop on missing values

`na_policy = "error"` stops the run when any invalid key, coordinate, or weight
is found. Use this policy when silent analytical exclusion is unacceptable.

### Count columns

The path CSV includes transparent slice-level counts:

| Column | Meaning |
|---|---|
| `n_rows_total` | Raw rows in the group-time slice after excluding invalid keys. |
| `n_total` | Unique non-missing entity IDs observed before coordinate, weight, or cohort exclusions. |
| `n_used` | Participant-period estimates used in the centroid. |
| `n_missing` | Observed entities excluded because all candidate rows have invalid selected coordinates or weights; zero-only entities are counted separately. |
| `n_excluded` | `n_total - n_used`, including missing, zero-weight, and cohort exclusions. |
| `n_cohort_excluded` | Otherwise-valid entities removed by the complete-cohort rule. |
| `n_zero_weight` | Entities having valid rows but no positive weight. |
| `n_rows_missing` | Raw rows with invalid selected coordinates or weights. |
| `n_distance_incomplete` | Used entities lacking a complete coordinate vector for the requested distance space. |
| `n_rows_distance_incomplete` | Otherwise usable raw rows with an incomplete requested distance vector. |
| `n_duplicate_rows` | Rows beyond one row per entity and time, collapsed before the centroid. |

A requested slice with no usable entities remains in the result with an `NA`
centroid and a diagnostic; it is not silently removed.

## Distance space

### Selected ENA axes

`distance_space = "selected"` calculates step, cumulative distance, speed, and
centroid-difference magnitude in the selected ENA axes. In the application,
these are normally the three X/Y/Z axes active at run time. This answers how far
the centroid moves in the displayed analytical subspace.

### Full ENA rotation

`distance_space = "full"` calculates those metrics across every ENA dimension
in the loaded rotation. The exported centroid columns still contain only the
selected axes. The selected-axis centroid cohort is fixed independently of the
distance option: incomplete extra dimensions make the affected full-space
metric unavailable rather than changing the displayed centroid or `n_used`.

Distances from different spaces are not interchangeable. Full-space distances
usually increase as omitted movement is included and can be sensitive to the
number and scaling of dimensions. Record the space and dimension list when
reporting a result. Both are included in export metadata.

Changing the 2D projection, camera, marker/line styling, or network overlay does
not change either distance calculation.

## Participant-cluster bootstrap

When **Show uncertainty** is enabled, the application calls
`bootstrap_centroid_path()` using the selected repetitions, confidence level,
and seed. The hosted application defaults are 500 repetitions, 95% confidence,
and seed 2026, and accepts 200–500 repetitions per run. The direct API accepts
smaller values for workflow testing, but that does not make a small run
scientifically adequate for confidence intervals.

Each bootstrap replicate:

1. constructs the analytically eligible entity-ID pool under the selected
   cohort and missing-value policies;
2. samples eligible entity IDs with replacement;
3. retains all raw rows and all periods for every sampled entity;
4. gives repeated draws clone IDs so they remain separate contributions;
5. recomputes the complete trajectory; and
6. records centroid coordinates, deltas, step distance, speed, and cumulative
   distance.

For `cohort_policy = "complete"`, the sampling pool contains only IDs with a
valid participant-period estimate at every requested period. The bootstrap
therefore targets the same complete-case population as the point estimate;
incomplete IDs are not sampled as empty clusters.

The application and direct API expose three participant-resampling designs:

- `"cluster"` treats one raw ID as one global entity across every trajectory
  group and resamples that entity with all of its rows;
- `"stratified"` treats the ID namespace as group-local, resamples independently
  within each group, and keeps every stratum's original eligible sample size;
  and
- `"auto"` selects stratified resampling when multiple groups have disjoint
  eligible ID sets, otherwise global cluster resampling to preserve dependence
  for IDs that occur in more than one group.

Choose explicitly when the study's ID namespace is known; observed overlap is
only a heuristic and cannot establish entity identity. The requested and
resolved design, sampling unit, eligible ID keys globally and by stratum,
eligible participant/sampling-unit counts, and fixed stratum sizes are stored in
`bootstrap_spec` and application export metadata.

The reported intervals are pointwise percentile intervals. Each bootstrapped
metric receives:

- `<metric>_lower`;
- `<metric>_upper`; and
- `<metric>_boot_n`, the number of finite replicates for that row.

Interval bounds are emitted only when all of the following are true:

1. the base metric is finite;
2. every slice needed by that metric has at least two participant clusters
   (both adjacent slices for an interval metric and the full prefix for a
   cumulative metric); and
3. the finite-replicate count is at least
   `max(ceiling(0.80 * n_boot), ceiling(10 / (1 - conf_level)))`.

The second term provides at least five expected finite replicates in each
percentile tail. If any condition fails, the lower and upper bounds are `NA`,
while `_boot_n` remains available for audit. The API returns this diagnostic
result even for a deliberately small `n_boot`; the hosted UI validates the
minimum before starting. `bootstrap_insufficient_clusters` and
`bootstrap_insufficient_replicates` identify the reason.

A fixed seed makes the direct API deterministic and restores the caller's R
random-number state after the operation. Plot hover reports `_boot_n / n_boot`.
Unavailable or invalid intervals are omitted from hover and error bars rather
than rendered as if they were valid.

These intervals preserve within-entity dependence across time, but they are not
simultaneous confidence bands and do not incorporate uncertainty from creating
the ENA model or estimating its rotation. They also do not account for a higher
level of clustering unless that level is the selected entity ID.

Bootstrap cost grows with the number of entities, periods, groups, dimensions,
and repetitions. Use a small value only for workflow testing; use a
substantively justified value for reported results.

## Paired condition comparison

When a group/condition variable, two distinct levels, and **Compute an exact
paired A/B trajectory comparison** are selected, the application creates the
descriptive group paths and calls
`compare_centroid_paths()` for levels A and B.

Before calculating either paired centroid, the comparison:

1. reduces duplicates to one entity-period estimate within each side;
2. matches the two sides by exact entity ID and time (and by any additional
   grouping variables in direct API use);
3. applies the selected cohort policy to those matched pairs; and
4. computes both centroids from the same entities.

The application first displays the A/B raw-ID overlap and exact ID-time match
count. The user must confirm that a repeated raw ID denotes the same physical
entity across conditions. This confirmation is a semantic assertion about the
source data; matching text alone is not evidence of identity.

With API weights, the default `pair_weight_policy = "require_equal"` requires
the matched participant-period weights on A and B to agree and stops otherwise.
This prevents an unannounced change of estimand. An analyst may explicitly set
`pair_weight_policy = "geometric"` to use the geometric mean of the two side
weights; the comparison specification records that policy and emits a
`geometric_pair_weights` warning. Application comparisons use equal weights and
the default equality requirement.

The difference direction is always:

```text
B - A
```

Important output columns include:

| Column family | Meaning |
|---|---|
| `centroid_a_<dimension>`, `centroid_b_<dimension>` | Paired side centroids. |
| `difference_<dimension>` | Coordinate difference, B minus A. |
| `delta_a_*`, `delta_b_*`, `delta_difference_*` | Adjacent coordinate changes and their difference. |
| `centroid_difference_distance` | Euclidean magnitude of B minus A in the configured distance space. |
| `step_distance_a`, `step_distance_b`, `step_distance_difference` | Side path steps and B-minus-A difference. |
| `speed_a`, `speed_b`, `speed_difference` | Side speeds and B-minus-A difference. |
| `cumulative_distance_a`, `cumulative_distance_b`, `cumulative_distance_difference` | Side cumulative paths and B-minus-A difference. |
| `n_a_total`, `n_b_total` | IDs observed on each side before analytical exclusions. |
| `n_a_valid`, `n_b_valid` | IDs with valid participant-period estimates on each side. |
| `n_matched` | Valid IDs present on both sides before the complete-cohort rule. |
| `n_used` | Matched IDs used after the cohort rule. |
| `n_unmatched_a`, `n_unmatched_b` | Valid IDs present on only one side. |
| `n_dropped_a`, `n_dropped_b` | Side IDs lost to invalid analytical values. |
| `n_cohort_excluded` | Matched IDs removed by complete-cohort enforcement. |

The comparison uses a matched-participant cluster percentile bootstrap and adds
the same `_lower`, `_upper`, and `_boot_n` suffixes to its comparison metrics.
It uses the configured bootstrap settings even when path uncertainty is not
shown. Enabling the comparison exposes those controls without also requiring
path uncertainty.

For direct-API comparisons with additional grouping variables,
`bootstrap_design` has the same `"auto"`, `"cluster"`, and `"stratified"`
meaning as path uncertainty. Stratified draws independently resample each
group's matched-ID pool at that pool's fixed size; global cluster draws preserve
one raw entity across groups. The requested/resolved design, eligible matched
IDs, stratum membership and sizes, and sampling-unit counts are retained in the
comparison bootstrap specification and exports. The same cluster and
finite-replicate requirements govern comparison interval bounds.

This is a paired comparison, not an independent-groups test. If A and B contain
different people with no shared IDs, the descriptive paths can still be useful,
but the paired result has no matched sample and must not be interpreted as an
independent condition contrast. Use the direct
`compare_independent_centroid_paths()` API described below for that design.

## Independent-group condition comparison API

`compare_independent_centroid_paths()` is the direct-R interface for two groups
whose participants are different people. The ID namespaces are deliberately
side-specific: `Student 1` in side A and `Student 1` in side B are never matched
or paired. Each side first collapses duplicate rows to participant-period
entities and applies the requested available/complete cohort policy separately.
All coordinates must still come from the same ENA rotation.

The reported direction is always B minus A. The table contains both side
centroids and paths, coordinate and adjacent-change contrasts, centroid
separation, and step/speed/cumulative-distance contrasts. Side-specific count
columns (`n_a_used`, `n_b_used`, cohort exclusions, and incomplete-distance
counts) make the independent denominators explicit.

Two distinct resampling procedures are used:

1. An independent-side participant-cluster percentile bootstrap supplies
   pointwise confidence intervals. Participants are sampled with replacement
   separately inside A and B and inside each optional trajectory-group stratum;
   all eligible periods for a sampled participant stay together.
2. A Monte Carlo participant-cluster label permutation supplies p-values. It
   pools the two independent participant trajectories inside each optional
   trajectory-group stratum, permutes whole-trajectory side labels, and
   preserves the original A/B participant counts. Signed B-minus-A contrasts
   use two-sided absolute statistics. The non-negative centroid-separation
   statistic uses an upper-tail test. Every p-value uses the finite-sample
   correction `(1 + exceedances) / (1 + valid permutations)`.

By default, Holm adjustment controls family-wise error over every finite
coordinate, adjacent-change, and movement contrast returned by the call.
`<metric>_p_value` is the raw permutation p-value,
`<metric>_p_adjusted` is the multiplicity-adjusted value, `<metric>_perm_n` is
the finite permutation count, and `<metric>_significant` evaluates the adjusted
p-value against `1 - conf_level`. The first path node has no adjacent-change,
step, speed, or cumulative-distance test; those inferential cells are `NA`
rather than artificial baseline tests.

The permutation test requires participant trajectories to be exchangeable
between A and B under the null within every stratum. That assumption is natural
for randomized group assignment but needs substantive justification in an
observational comparison. Neither bootstrap intervals nor permutation p-values
include uncertainty from fitting or rotating the ENA model, and neither alone
establishes causality.

## Visualization and selected-time network

Each trajectory is one Plotly `lines+markers` trace with stable group colors.
Centroid-node fill uses a shared, perceptually ordered Viridis scale keyed by
the global `time_order` domain. Thus every ordered period has a distinct color,
and the same period retains the same fill across trajectory groups, 2D/3D
views, camera changes and row order. Group color remains on path lines,
direction arrows and node outlines. A scrollable node key lists
`Order · time value` beside the plot on desktop and moves below it on narrow
screens; unordered manual rows use a neutral gray key rather than a
misleading ordered color.
Direction is shown by a conventional two-wing arrowhead for every finite,
non-zero adjacent centroid segment. The arrow reaches the destination centroid,
then the pixel-sized centroid marker is redrawn above it; this masks the
interior and makes the visible two-wing head meet the node's circular outer
edge instead of occupying the middle of the connecting line or covering the
node. In 3D, the two-wing plane follows the active camera to remain visible.
Arrowheads share the trajectory color, do not create additional legend entries
or hover targets, and are omitted at missing-value breaks, zero-length steps,
and unordered or non-increasing time positions. The **Show direction arrows on
path segments** control is enabled by default in both 3D and 2D views. Arrow
geometry is a display-only overlay: it is not added to the analytical path
table, does not change centroid coordinates or distances, and is not included
in exports.
Hover text reports time/order, centroid coordinates and intervals, sample size,
movement and elapsed-time metrics, missing/excluded counts, distance space, and
row-level warnings. Distance space is also retained in trace data and export
metadata.
Bootstrap coordinate intervals appear as axis-aligned error bars, with the
finite count shown as `boot_n / n_boot`. A bound is drawn only when both limits
are finite, ordered, and bracket the finite point estimate.

The 2D view is a projection of the same path table used by the 3D view. Changing
view mode or projection axes rerenders the plot without rerunning the analysis.

The optional network overlay shows raw code-node positions and the non-zero
mean edge weights at the selected time. Its scope can be overall across all
trajectory groups or one selected group. It averages all line-weight rows that
match that time and optional group; it does not automatically inherit the
trajectory's available/complete analytical cohort, missing-value exclusions,
or bootstrap resamples. The status message therefore labels it as contextual.

Edge weights are mapped only by exact adjacency endpoint names, accepting the
documented reverse endpoint order. If an endpoint column is missing, duplicated,
or ambiguous, the overlay is withheld instead of falling back to positional
column matching. Numeric and POSIX selected-time filters use the same lossless
keys as the time-order control. The overlay never changes centroid or distance
values.

## Diagnostics

Warnings are part of the analytical result, not merely console messages. The
application displays them above the plot and includes them in the metadata CSV.
The direct API stores the structured table in
`attr(result, "trajectory_warnings")`.

Common diagnostics include:

| Code | Meaning |
|---|---|
| `time_order_requires_review` / `implicit_character_order` | A labeled time order needs substantive review. |
| `missing_key_rows` | Rows with invalid ID, time, or group keys were excluded. |
| `missing_period` | A requested group-time slice has no rows. |
| `duplicate_id_time` | Duplicate entity-period rows were collapsed. |
| `one_entity_slice` | A centroid uses one entity, so between-entity uncertainty is not estimable. |
| `zero_variance_slice` | All selected coordinates have zero between-entity variance. |
| `changing_cohort` | The available entity set changes over time. |
| `full_distance_incomplete` | The selected centroid is retained, but a full-space metric is unavailable because extra coordinates are incomplete. |
| `nonpositive_elapsed_interval` | Speed is undefined for a non-positive interval. |
| `bootstrap_insufficient_clusters` | A bound is unavailable because a contributing slice/interval has fewer than two participant clusters. |
| `bootstrap_insufficient_replicates` | A bound is unavailable because too few finite replicates meet the 80% and five-per-tail rule. |
| `unmatched_participants` | Paired comparison IDs occur on only one side. |
| `dropped_invalid_pairs` | Invalid analytical values removed potential pairs. |
| `no_matched_participants` | No valid IDs match between A and B. |
| `changing_matched_cohort` | The exact matched-ID set changes across periods. |
| `one_pair_slice` | A paired slice contains only one matched entity. |
| `missing_paired_period` | A requested period contains no matched pair. |
| `comparison_levels_unavailable` | Two distinct condition levels were not available. |
| `geometric_pair_weights` | An explicit API request combined unequal A/B weights by geometric mean, changing the weighted estimand. |
| `missing_independent_period_a` / `_b` | One independent side has no valid participant in a requested period. |
| `one_entity_slice_a` / `_b` | One independent side's centroid uses only one participant. |
| `changing_cohort_a` / `_b` | An independent side's available participant composition changes over time. |
| `full_distance_incomplete_a` / `_b` | One independent side retains selected centroids but lacks complete full-space coordinates. |
| `permutation_insufficient_clusters` | A contrast lacks at least two participant clusters on each independent side. |
| `permutation_insufficient_replicates` | Too few finite label permutations are available for a p-value. |

A warning does not necessarily make every row unusable, but it requires review.
A validation error stops the run and leaves no new analytical result.

## CSV exports

### Analysis bundle ZIP

The recommended archival download packages `path.csv`, optional uncertainty and
comparison tables, `diagnostics.csv`, `metadata.csv`, and a versioned
`manifest.json` together. The manifest uses the stable schema identifier
`urn:3dena:trajectory-analysis-bundle:1` and identifies the 3dena.com analysis,
analysis/build settings, SHA-256 dataset and rotation fingerprints, R/package
versions, and every included file. Use the bundle rather than mixing CSV files
from separate runs.

### Path CSV

Contains one row per group and ordered time, with counts, selected centroid
coordinates, coordinate deltas, step distance, elapsed interval, speed, and
cumulative distance.

### Uncertainty CSV

Available after path uncertainty is computed. It contains the full path plus
bootstrap interval and valid-replicate columns.

### Comparison CSV

Available when two distinct condition levels have been compared. It contains
paired counts, both side centroids and paths, B-minus-A differences, and
bootstrap intervals.

### Metadata CSV

Contains key/value analysis settings followed by structured diagnostic entries.

Every analytical CSV repeats provenance columns prefixed with `.analysis_`,
including:

- time, ID, group, and selected condition levels;
- selected and full dimension lists;
- distance, cohort, and missing-value policies;
- explicit time order;
- bootstrap enabled flag, repetitions, confidence level, seed, sampling unit,
  requested/resolved design, eligible IDs and per-stratum membership/counts,
  interval-validity thresholds, failed-replicate count, and RNG-restoration
  status;
- paired-ID identity confirmation and raw-ID/ID-time overlap counts;
- raw point-row count;
- SHA-256 dataset and rotation fingerprints, app/build/Git identity, and
  R/package versions; and
- UTC generation timestamp.

An empty CSV cell represents `NA`, not zero. Character cells beginning with
`=`, `+`, `-`, or `@` are prefixed with an apostrophe to prevent spreadsheet
formula execution; this escaping policy is itself recorded in metadata. Source
or output fields that already use a target `.analysis_*` name cause export to
stop. Keep the metadata and diagnostic export with any derived table or figure.

## Direct R API

The analytical layer has no Shiny, Plotly, data.table, or rENA dependency.

```r
source("R/trajectory_analysis.R")

path <- compute_centroid_path(
  points = ena_obj$points,
  time_var = "Week",
  id_var = "Name",
  group_vars = NULL,
  dimensions = c("SVD1", "SVD2", "SVD3"),
  order = 0:14,
  cohort_policy = "available",
  na_policy = "complete",
  distance_space = "selected"
)

warnings <- attr(path, "trajectory_warnings")
specification <- attr(path, "trajectory_spec")
```

For clustered intervals:

```r
boot <- bootstrap_centroid_path(
  points = ena_obj$points,
  time_var = "Week",
  id_var = "Name",
  dimensions = c("SVD1", "SVD2", "SVD3"),
  order = 0:14,
  n_boot = 1000,
  conf_level = 0.95,
  seed = 2026,
  bootstrap_design = "auto"
)

bootstrap_specification <- attr(boot, "bootstrap_spec")
```

For paired A/B tables:

```r
comparison <- compare_centroid_paths(
  points_a = condition_a_points,
  points_b = condition_b_points,
  time_var = "Week",
  id_var = "Name",
  dimensions = c("SVD1", "SVD2", "SVD3"),
  order = 0:14,
  cohort_policy = "complete",
  n_boot = 1000,
  conf_level = 0.95,
  seed = 2026,
  labels = c("A", "B"),
  pair_weight_policy = "require_equal",
  bootstrap_design = "auto"
)

comparison_specification <- attr(comparison, "comparison_spec")
```

For independent A/B groups whose same-looking IDs identify different people:

```r
independent_comparison <- compare_independent_centroid_paths(
  points_a = experimental_points,
  points_b = control_points,
  time_var = "Lesson",
  id_var = "Name",
  dimensions = c("MR1", "SVD2", "SVD3"),
  order = c("Lesson 1", "Lesson 2"),
  cohort_policy = "complete",
  distance_space = "full",
  full_dimensions = ena_dimension_names,
  n_boot = 1000,
  n_perm = 1999,
  conf_level = 0.95,
  seed = 2026,
  labels = c("Experimental", "Control"),
  p_adjust_method = "holm"
)

comparison_specification <- attr(independent_comparison, "comparison_spec")
bootstrap_specification <- attr(independent_comparison, "bootstrap_spec")
permutation_specification <- attr(independent_comparison, "permutation_spec")
```

To calculate movement in the full rotation while displaying only three axes,
set `distance_space = "full"` and supply every coordinate name through
`full_dimensions`.

## Interpretation and current limitations

- A centroid path is a group-level summary and can hide dispersion,
  multimodality, and heterogeneous participant paths.
- Available-cohort movement may reflect changing composition. Complete-cohort
  movement applies only to entities observed validly at every requested time.
- All compared points must share one ENA rotation. Axis signs and orientations
  are model-dependent.
- Euclidean movement depends on the chosen dimensions and their scaling.
- Full-space and selected-space distances answer different questions and
  should not be compared as if they used one metric.
- The path contains observed adjacent-centroid segments only; there is no
  smoothing, interpolation, extrapolation, or temporal model.
- Arrowheads encode the declared order of adjacent centroids; their rendered
  size is a visual aid and does not encode elapsed time, speed, or uncertainty.
- Once an ordered path is discontinuous, its origin-to-current cumulative
  distance remains unavailable even when a later adjacent step can be computed.
- Speed is meaningful only when time spacing is numeric and substantively
  interpretable.
- Bootstrap intervals are pointwise participant-resampling intervals. They are
  not simultaneous bands, causal estimates, formal longitudinal models, or
  uncertainty estimates for ENA model construction.
- The application comparison UI is paired by exact ID and time. The direct API
  additionally supports independent groups; equal raw-ID text across its two
  sides is intentionally treated as different people.
- Independent-group permutation p-values require whole participant trajectories
  to be exchangeable between sides under the null within each trajectory-group
  stratum. Holm adjustment is pointwise across returned contrasts, not a
  simultaneous confidence band or longitudinal model.
- The application UI supports one trajectory group variable and equal entity
  weights. The direct API supports multiple grouping columns and weights.
- The selected-time network overlay may be overall or group-filtered, but it is
  a contextual line-weight average and does not enforce the trajectory cohort.
- Large full-rotation bootstraps can be computationally expensive.
- The hosted application enforces both a repetition cap and an estimated CPU
  budget (`ENA3D_MAX_BOOTSTRAP_SECONDS`, 60 seconds by default). Jobs above the
  budget must reduce repetitions/dimensions or run in an approved offline
  environment.

## Reporting checklist

For a reproducible result, report at least:

1. the ENA set and shared rotation used;
2. time variable and explicit order;
3. repeated entity ID;
4. group variable and compared level direction, if any;
5. selected dimensions and distance space;
6. available or complete cohort policy;
7. missing-value policy and slice counts;
8. bootstrap method, repetitions, confidence level, seed, requested/resolved
   resampling design, eligible ID namespace, and valid-replicate counts;
9. for a paired comparison, the ID-identity assertion, overlap counts, and
   paired-weight policy;
10. for an independent comparison, the side-specific ID namespaces and sample
    sizes, permutation count, exchangeability justification, p-value correction,
    multiplicity method, and significance level;
11. all material diagnostics; and
12. whether the figure is a 2D projection or 3D view, and the network overlay
    scope/cohort distinction if an overlay is shown.

Archive the path/comparison CSV together with the metadata CSV rather than
relying on a screenshot alone.
