## scaledescr

**scaledescr** provides a structured workflow for transforming raw psychometric scale data into clean, publication-ready statistical tables.

Designed for psychology, psychiatry, behavioral science, and public health research, it integrates:

**Data preparation → Scale totals → Reliability → Descriptives → Inferential tests → Export**

---

## Installation

From CRAN:

```r
install.packages("scaledescr")
```

Development version:

```r
devtools::install_github("dharaiyadarshan-create/scaledescr")
```

---

## Minimal Workflow Example

```r
library(scaledescr)

# Reverse items
data <- make_reverse_score(data, vars = c("A1", "C4"), min_val = 1, max_val = 5)

# Compute total
data <- make_scale_total(data, vars = c("N1","N2","N3"), new_var = "Neuroticism")

# Reliability
alpha_res <- compute_alpha(data, items = c("N1","N2","N3"))
print(alpha_res)

# Descriptives
make_scale_description_table(data, columns = "Neuroticism")

# Inferential test
t_res <- t.test(data$Neuroticism, mu = 15)
make_one_sample_t_test_table(t_res)

# Export
make_dataframe_to_output(result_table, filename = "results", format = "excel")
```

---

## Core Modules

* Data preparation
* Scale computation
* Reliability analysis
* Descriptive summaries
* Inferential test tables
* Export-ready outputs

---

## Design Principles

* Clear separation of computation and reporting
* Reproducible psychometric workflow
* Publication-oriented tables
* Transparent statistical structure

---

## Intended Users

* Psychiatry and psychology researchers
* Postgraduate students
* Public health researchers
* Clinicians preparing tables for publication

---


## Citation

If you use scaledescr in your research, please cite:
```
Dharaiya, D. (2026). scaledescr: Descriptive, Reliability, and Inferential
Tables for Psychometric Scales. R package version 0.2.4.
https://doi.org/10.32614/CRAN.package.scaledescr
```

---
