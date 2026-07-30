# Bundled trusted samples

Files in this directory are reviewed native R fixtures loaded only from the
read-only application image. Browser uploads never enter this trust boundary.

The four bundled fixtures are:

- `newfrat_enaset.Rdata`: a 15-week repeated-participant example;
- `sample_enaset.Rdata`: a small ordered cross-sectional group profile;
- `student_enaset.RData`: an ordered cross-sectional performance profile; and
- `class1_timepoints_enaset.RData`: the pseudonymized Class 1 TP1-TP3 example.

`class1_timepoints_enaset.RData` is the longitudinal Class 1 example for the
Trajectory workspace. It contains one shared ENA rotation with 72
student-by-period networks from five groups across TP1, TP2, and TP3. The
fixture preserves the reviewed SVD coordinates, code nodes, and line weights;
it does not contain message text.

Before publication, the source object was reduced through the version-1 3dENA
exchange contract. Learner names and ENA unit labels were replaced with stable
group-scoped pseudonyms (`G1-S01`, and so on), and condition labels were
standardized to `GenAI group` and `Non-GenAI group`. The trusted trajectory
defaults are `Period` (time), `Speaker` (repeated entity), and `Group`
(trajectory group).

After computing the five group paths, choose `G1` under **Displayed trajectory
levels** to reproduce the Group 1 figure. That selector is display-only: it
does not refit the shared rotation, recompute centroids, or remove the other
groups from analytical exports.

The deterministic preparation entry point is:

```sh
Rscript tools/prepare_class1_timepoints_sample.R INPUT.RData \
  sample_data/class1_timepoints_enaset.RData
```

The private source path is intentionally not stored in the fixture or script.
The absence of names and message text in this reviewed example is not a claim
that arbitrary classroom data are anonymous. Do not publish or upload a new
dataset until its direct and indirect identifiers have been reviewed under the
applicable research-data policy.
