# validation/

* `legacy_equivalence_synthetic.R` — sources the frozen legacy analysis code (read-only) and compares its results with the package **on the synthetic data only**
  (cumulative grid; final and, with `--archived`, archived DLMs). Writes summary/detail/timing CSVs. Run: `Rscript validation/legacy_equivalence_synthetic.R --out DIR`.
* Real-data validation is done with `Rscript run.R validate <csv> <reference_folder>` in the private environment. Reference file names are declared in
  `config/sieccwinter_config.json` (`validation.references`); a missing reference file is reported as `NOT_TESTED`. Tolerances are in the same file.
* Nothing in this folder contains study data.
