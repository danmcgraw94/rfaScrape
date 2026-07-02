# rfaScrape

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

The rfaScrape package helps export data and results from RMC-RFA projects. The package aims to improve organization and reporting efficiency by scraping/exporting RFA project data from `.sqlite` files into organized CSV directories. This package will be incorporated in future developments of `rfaR`.

## Installation

```r
# install.packages("remotes")
remotes::install_github("danmcgraw94/rfaScrape")
```

## Quick Start

```r
library(rfaScrape)

sqlite_file <- "~/path/to/my_rfa_project.rfa.sqlite"
out_dir <- "/data/rfa_scrape/rfa_outputs"

scrape_rfa_sqlite(sqlite_file, out_dir)
```

## Further Reading

- [DBI](https://github.com/r-dbi/DBI): Used to open connections to .sqlite database files
- [RSQLite](https://github.com/r-dbi/RSQLite): Used to open connections to .sqlite database files
- [rfaR](https://github.com/USACE-RMC/rfaR): Future development to improve accessibility to `rfaR`
