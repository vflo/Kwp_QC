# KPLANT Quality Control (Kwp_QC)

This repository contains the automated **quality-control (QC) pipeline** used
to generate the per-dataset HTML reports archived alongside the
[KPLANT database](https://zenodo.org/records/19816125) on Zenodo.

> **Naming note.** The KPLANT database was originally referred to as *Kwp*
> (whole-plant water-potential-derived hydraulic conductance) during data
> collection, hence the repository name. *Kwp* and *KPLANT* refer to the
> same project; *KPLANT* is the current name used in the data paper and the
> Zenodo deposit.

## Data paper

> Zhao, Y., Mencuccini, M., *et al.*, Flo, V. KPLANT: a global database of plant
> water transport capacity. *Scientific Data*, *in preparation* (DOI pending).

## Repository contents

| File / folder | Purpose |
| --- | --- |
| `Kwp_database_template_14_05_2025.xlsx` | Standardised Excel template used by data contributors to submit per-study datasets. |
| `QC_function.R` | Core QC functions applied to each submitted dataset. |
| `setup_functions.R` | Helper functions (unit checks, taxonomy, plotting). |
| `Data_Quality_Report.Rmd` | R Markdown template that renders the per-dataset HTML QC report. |
| `report_exclusion_message.Rmd` | Auxiliary R Markdown template used when a dataset is excluded. |
| `QC_Run_script.R` | Run the QC pipeline interactively on a single dataset. |
| `QC_Run_auto_script.R` | Batch-run the QC pipeline over a folder of datasets. |
| `test.R`, `test_that/` | Unit tests for the QC functions. |
| `TO_DO/` | Pending items and notes. |

## Installation

Clone the repository and open the RStudio project:

```bash
git clone https://github.com/vflo/Kwp_QC.git
```

Required R packages:

```r
install.packages(c("readxl", "dplyr", "tidyr", "purrr", "stringr",
                   "ggplot2", "rmarkdown", "knitr", "taxize", "testthat"))
```

Tested with R ≥ 4.3 and Pandoc (bundled with RStudio).

## Usage

The pipeline expects datasets to follow the structure of
`Extraction_template.xlsx`. Two entry points are provided:

1. **Single dataset (interactive)** — open `QC_Run_script.R`, set the path to
   the dataset, and run. The script renders `Data_Quality_Report.Rmd` and
   produces an HTML QC report.
2. **Batch (automated)** — open `QC_Run_auto_script.R`, set the folder
   containing the datasets, and run. The script iterates over all `.xlsx`
   files matching the template and produces one HTML report per dataset.

The HTML reports archived on Zenodo (compressed zip file `Qc_reports.zip`) were generated
with this pipeline.

## Related repositories

- [`vflo/Kplant_database`](https://github.com/vflo/Kplant_database) — R code
  for data control and technical validation of the KPLANT database.

## License

Code in this repository is released under the [MIT License](LICENSE).

## Contact

Víctor Flo — CREAF — `v.flo@creaf.uab.cat`
