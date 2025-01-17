
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NoButter: An R package for reducing transcript dispersion in CosMx Spatial Molecular Imaging data

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.0.0.9000-blue.svg)](https://github.com/jelmar-quist/NoButter)
[![](https://img.shields.io/badge/doi-10.1101/2024.11.25.625243-yellow.svg)](https://doi.org/10.1101/2024.11.25.625243)
<!-- badges: end -->

Transcript dispersion can introduce considerable levels of technical
noise to the data that can negatively impact downstream analysis.
NoButter is an easy to use R package to evaluate transcript dispersion
in CosMx Spatial Molecular Imaging (SMI) data. Using raw transcript
data, transcript dispersion can be systematically assessed across
Z-slices, FOVs and samples. Clean transcript data can be consolidated
into a set of ‘flat files’ for further downstream analysis.

On average, NoButter discards an additional ~10% of raw transcripts,
resulting in reduced background noise in CosMx SMI data.

Transcript dispersion detection relies on the localisation of the
transcripts in relation to the cell boundaries and it can be challenging
to detect, particularly in FOVs encompassing densely packed cells. To
confidently identify transcript dispersion, it is recommended to assess
FOVs that are located at the edge of the tissue section, cover gaps or
breaks present in the tissue section or are positioned in regions with
distinct tissue architecture.

## Installation

You can install the development version of NoButter from
[GitHub](https://github.com/) with:

``` r
library("devtools")
install_github("jelmar-quist/NoButter")
```
