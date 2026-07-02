# rfaScrape

<!-- badges: start -->

[![R >= 4.1.0](https://img.shields.io/badge/R-%3E%3D4.1.0-276DC3.svg)](https://www.r-project.org/)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

The rfaScrape package helps export data and results from RMC-RFA projects. The package aims to improve organization and reporting efficiency by scraping/exporting RFA project data from `.rfa.sqlite` files into organized CSV directories. This package will be incorporated in future developments of `rfaR`.

## Installation

```r
# install.packages("remotes")
remotes::install_github("danmcgraw94/rfaScrape")
```

## Quick Start

A small example RFA project is included with the package, so you can try it immediately without your own `.rfa.sqlite` file.

```r
library(rfaScrape)

sample_path <- system.file("extdata", "example_RFA_project.rfa.sqlite", package = "rfaScrape")
scrape_rfa_sqlite(sample_path, base_dir = tempdir())
```

## Further Reading

- [DBI](https://github.com/r-dbi/DBI): Used to open connections to .sqlite database files
- [RSQLite](https://github.com/r-dbi/RSQLite): Used to open connections to .sqlite database files
- [rfaR](https://github.com/USACE-RMC/rfaR): Future development to improve accessibility to `rfaR`
