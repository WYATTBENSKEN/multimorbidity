
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multimorbidity

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/multimorbidity)](https://cran.r-project.org/package=multimorbidity)
![CRAN\_Download\_Counter](http://cranlogs.r-pkg.org/badges/grand-total/multimorbidity)
<!-- badges: end -->

The goal of `multimorbidity` is to create a single package which can
take original claims data, with the ability to clean and organize them
to a simple format, which can then be immediately used to obtain any of
a number of comorbidity, frailty, and multimorbidity measures. This
package is meant to be a simple and transparent one-stop-shop for those
working with claims or other administrative health care data. The
measures included in this package have been developed by other
researchers and are detailed in depth below.

This package is meant to be both user-friendly and transparent. An
individual should feel comfortable understanding what’s under the hood
with these various metrics. Given this, this function has been written
in a way that makes the code, including specific diagnosis codes,
accessible.

For examples of the various functions included in this package, please
see the documentation or the vignette.

## Installation

You can install `multimorbidity` from CRAN with:

``` r
install.packages("multimorbidity")
```

The development version of `multimorbidity` can be downloaded from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("WYATTBENSKEN/multimorbidity")
```

## Citation Information

In addition to citing this R Package, we ask that you please cite the
original manuscripts which developed these algorithms. Portions of this
package, specifically the Elixhauser and Charlson diagnoses codes, were
adapted from another package,
[`comorbidity`](https://github.com/ellessenne/comorbidity).

## Data Cleaning Functions

There are two data cleaning functions in this package. The first,
`prepare_data()` should be run first as it prepares the data to a
uniform format, which the various measures rely on. The end-goal is to
have a dataset that has 1 column with a patient ID, 1 column which
contains the diagnosis code, and 1 column which will note if it’s ICD-9
(9), ICD-10 (10), or HCPCS/CPT (1). There are other variables that may
be of interest depending on the specification including type (inpatient
or outpatient) and date.

The second function, `comorbidity_window()`, is not highly suggest as
the `prepare_data()` function is but may be useful to some
investigators. Oftentimes, we may be interested in limiting our claims
to a specific window, such as the 1-year before diagnosis. To
accommodate this, `comorbidity_window()` will merge your prepared
diagnosis dataset with an ID dataset and limit the claims/diagnoses to a
specific time window relative to a date of interest.

## Included Comorbidities and Indices

| Index                                                                     | Citation(s)                                                                                                                                                                                         |
|:--------------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [Elixhauser Comorbidities and Index](#elixhauser-comorbidities-and-index) | [Elixhauser (1998)](https://pubmed.ncbi.nlm.nih.gov/9431328/), [Moore et al. (2017)](https://pubmed.ncbi.nlm.nih.gov/28498196/)                                                                     |
| [Charlson Comorbidities and Index](#charlson-comorbidities-and-index)     | [Charlson et al. (1987)](https://pubmed.ncbi.nlm.nih.gov/3558716/), [Deyo et al. (1992)](https://pubmed.ncbi.nlm.nih.gov/1607900/), [Quan et al. (2005)](https://pubmed.ncbi.nlm.nih.gov/16224307/) |
| [Claims Frailty Index](#claims-frailty-index)                             | [Kim et al. (2018)](https://pubmed.ncbi.nlm.nih.gov/29244057/)                                                                                                                                      |
| [Multimorbidity Weighted Index](#multimorbidity-weighted-index)           | [Wei et al. (2020)](https://pubmed.ncbi.nlm.nih.gov/31917465/)                                                                                                                                      |
| [Nicholson and Fortin Conditions](#nicholson-and-fortin-conditions)       | [Nicholson et al. (2015)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5636032/), [Fortin et al. (2017)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5772378/)                                     |

### Elixhauser Comorbidities and Index

The Elixhauser Comorbidities and Comorbidity Index are a widely-used set
of comorbidities originally developed in 1998 by
[Elixhauser](https://pubmed.ncbi.nlm.nih.gov/9431328/), with two indices
for mortality and readmission created in 2017 by [Moore et
al.](https://pubmed.ncbi.nlm.nih.gov/28498196/)

In this package, we used the codes provided in the format programs by
the Agency for Healthcare Research and Quality for
[ICD-9](https://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp),
[ICD-10
Beta](https://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10_archive.jsp),
and
[ICD-10](https://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp).
The ICD-10 data contain a larger set of comorbidities and, as of this
writing, no calculator for the indices has been released, and thus when
data contain both ICD-9 and ICD-10, we will use the ICD-9 comorbidities
with the Beta code. Finally, the original algorithm takes into account
DRG, which this package currently does not accommodate.

> You can obtain the Elixhauser comorbidities and index by running the
> `elixhauer()` function.

### Charlson Comorbidities and Index

The Charlson Comorbidities and Index are, similarly, a widely-used set
of comorbidities. First developed in 1987 by [Charlson et
al.](https://pubmed.ncbi.nlm.nih.gov/3558716/), they’ve been modified a
number of times. This algorithm employs the [Deyo et
al.](https://pubmed.ncbi.nlm.nih.gov/1607900/) list of 17 comorbidities,
with the adaptations included in [Quan et
al.](https://pubmed.ncbi.nlm.nih.gov/16224307/)

> You can obtain the Charlson comorbidities and index by running the
> `charlson()` function.

### Claims Frailty Index

The Claims Frailty Index (CFI) is based off of work by [Kim et
al.](https://pubmed.ncbi.nlm.nih.gov/29244057/) in 2018. This algorithm
uses ICD-9, ICD-10, and procedure codes to establish the frailty score
for each patient. The code included in this package is largely developed
from publicly-available code which can be found on the [Harvard
dataverse](https://dataverse.harvard.edu/dataverse/cfi). As the original
algorithms included HCPCS/CPT procedure codes, so does this.

> You can obtain the CFI by running the `cfi()` function.

### Multimorbidity Weighted Index

The Multimorbidity Weighted Index (MWI) was created by [Wei et
al.](https://pubmed.ncbi.nlm.nih.gov/31917465/) in 2020. This uses ICD-9
codes (note: ICD-10 is not yet available for MWI) to establish a
multimorbidity index for each individual. The R code used in this
package was developed based on the supplement included in the previously
linked manuscript.

> You can obtain the MWI by running the `mwi()` function.

### Nicholson and Fortin Conditions

The Nicholson and Fortin Conditions were first [published in
2015](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5636032/) and then
[updated to ICD-10 in
2017](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5772378/). These 20
chronic conditions are a standardized list used for multimorbidity
research, and developed from a community-based primary healthcare
project.

> You can obtain the Nicholson and Fortin Conditions by running the
> `nicholsonfortin()` function.
