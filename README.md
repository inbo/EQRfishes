
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Project Status: Concept – Minimal or no implementation has been done
yet, or the repository is only intended to be a limited example, demo,
or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg?style=flat)](https://www.gnu.org/licenses/gpl-3.0.html)
[![Release](https://img.shields.io/github/release/inbo/EQRfishes.svg)](https://github.com/inbo/EQRfishes/releases)
![GitHub](https://img.shields.io/github/license/inbo/EQRfishes) [![R
build
status](https://github.com/inbo/EQRfishes/workflows/check%20package%20on%20main/badge.svg)](https://github.com/inbo/EQRfishes/actions)
![r-universe
name](https://inbo.r-universe.dev/badges/:name?color=c04384)
![r-universe package](https://inbo.r-universe.dev/badges/EQRfishes)
[![Codecov test
coverage](https://codecov.io/gh/inbo/EQRfishes/branch/main/graph/badge.svg)](https://app.codecov.io/gh/inbo/EQRfishes?branch=main)
![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/inbo/EQRfishes.svg)
![GitHub repo
size](https://img.shields.io/github/repo-size/inbo/EQRfishes.svg)
<!-- badges: end -->

# EQRfishes

The goal of R package `EQRfishes` is to calculate an index of biotic
integrity (ibi) and ecological quality ratio (EQR) based on fish
measures. Rivers and water bodies with different characteristics are
assessed with different criteria, each called an indextypology, as is
described in the different publications on which the calculations are
based:

Belpaire et al. (2000), Breine et al. (2004), Breine and Van Thuyne
(2018), Breine et al. (2015), Breine and Van Thuyne (2013), Breine et
al. (2010), Breine and Van Thuyne (2020), Breine et al. (2021)

The calculations itself are also described into detail in
`vignette("overview_calculations", package = "EQRfishes")`.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-belpaire_index_2000" class="csl-entry">

Belpaire, Claude, Roel Smolders, Ina Vanden Auweele, et al. 2000. “An
Index of Biotic Integrity Characterising Fish Populations and Ecological
Quality of Flandrian Water Bodies.” *Hydrobiologia* 434: 17–33.
<https://doi.org/10.1023/A:1004026121254>.

</div>

<div id="ref-breine_zone-specific_2010" class="csl-entry">

Breine, Jan, Paul Quataert, Maarten Stevens, et al. 2010. *A
Zone-Specific Fish-Based Biotic Index as a Management Tool for a
Temperate Estuary (Zeeschelde, Belgium)*. 60 (7): 1099–112.
<https://doi.org/10.1016/j.marpolbul.2010.01.014>.

</div>

<div id="ref-breine_fish-based_2004" class="csl-entry">

Breine, Jan, Ilse Simoens, Peter Goethals, et al. 2004. “A Fish-Based
Index of Biotic Integrity for Upstream Brooks in Flanders (Belgium).”
*Hydrobiologia* 522 (1-3): 133–48.
<https://doi.org/10.1023/B:HYDR.0000029991.42922.a4>.

</div>

<div id="ref-breine_new_2021" class="csl-entry">

Breine, Jan, Ericia Van den Bergh, Gerlinde Van Thuyne, and Claude
Belpaire. 2021. “A New Fish-Based Index of Biotic Integrity for Lowland
Rivers in Flanders (Belgium).” *Belgian Journal of Zoology* 151: 107–37.
<https://doi.org/10.26496/bjz.2021.89>.

</div>

<div id="ref-breine_het_2013" class="csl-entry">

Breine, Jan, and Gerlinde Van Thuyne. 2013. *Het Visbestand in Het
IJzer-Estuarium: Viscampagnes 2008-2012.* INBO.R. 2013.8. Rapporten van
Het Instituut Voor Natuur- En Bosonderzoek 2013. Instituut voor Natuur-
en Bosonderzoek.
<https://pureportal.inbo.be/nl/publications/het-visbestand-in-het-ijzer-estuarium-viscampagnes-2008-2012>.

</div>

<div id="ref-breine_een_2018" class="csl-entry">

Breine, Jan, and Gerlinde Van Thuyne. 2018. *Een Index van Biotische
Integriteit Voor de Evaluatie van de Ecologische Toestand van
Visgemeenschappen in Brongebieden*. No. 61. Rapporten van Het Instituut
Voor Natuur- En Bosonderzoek 2018. Instituut voor Natuur- en
Bosonderzoek. <https://doi.org/10.21436/inbor.14586362>.

</div>

<div id="ref-breine_visindex_2020" class="csl-entry">

Breine, Jan, and Gerlinde Van Thuyne. 2020. *Visindex Voor Getijgebonden
Zijrivieren in Het Zeeschelde-Estuarium*. No. 14. Rapporten van Het
Instituut Voor Natuur- En Bosonderzoek 2020. Instituut voor Natuur- en
Bosonderzoek. <https://doi.org/10.21436/inbor.17941691>.

</div>

<div id="ref-breine_development_2015" class="csl-entry">

Breine, Jan, Gerlinde Van Thuyne, and Luc De Bruyn. 2015. “Development
of a Fish-Based Index Combining Data from Different Types of Fishing
Gear. A Case Study of Reservoirs in Flanders (Belgium).” *Belgian
Journal of Zoology* 145 (1): 17–39.
<https://doi.org/10.26496/bjz.2015.55>.

</div>

</div>

## Installation

To install `EQRfishes` from the [INBO
universe](https://inbo.r-universe.dev/builds), start a new R session and
run this code (before loading any packages):

``` r
# Enable the INBO universe (not needed for INBO employees, as this is the default setting)
options(
  repos = c(
    inbo = "https://inbo.r-universe.dev", CRAN = "https://cloud.r-project.org"
  )
)
# Install the packages
install.packages("EQRfishes")
```

To install EQRfishes from GitHub, start a new R session and run this
code (before loading any packages):

``` r
# install.packages("remotes")
remotes::install_github("inbo/EQRfishes")
```

## Use `EQRfishes`

The main function, `calculate_eqr()`, calculates de fish-based
ecological quality ratio (EQR) based on a dataset with sampling location
characteristics (`data_sample`) including the indextypology that should
be used and a dataset with fish measure data for each location
(`data_fish`). The function documentation of `calculate_eqr()` explains
how these datasets should be composed.

Some examples for different indextypologies:

``` r
library(EQRfishes)
# freshwater river indexes (lowland IBI, upstream,...)
data_sample <- read.csv2(
  system.file("testdata/freshwater_sample.csv", package = "EQRfishes")
)
data_fish <- read.csv2(
  system.file("testdata/freshwater_fish_data.csv", package = "EQRfishes")
)
# add information on index that should be calculated
index_info <- data.frame(
  order_id = 1:12,
  sample_id = c(
    11652, 13384, 8681, 13282, 13561, 8434, 4550, 11611, 13534, 13512, 8507,
    2258
  ),
  indextypology = c(
    rep("brabeel", 6), "bron", "upstream", "vlagzalm", rep("brabeel", 2),
    "forel"
  )
)
data_sample <- data_sample |>
  inner_join(index_info, by = "sample_id")
# calculate index
calculate_eqr(data_sample, data_fish)
#> Warning in calculate_eqr(data_sample, data_fish): Some taxoncodes given in
#> data_fish are unknown fishes and these records will be excluded from the
#> analysis: ERI.SIN.
#> # A tibble: 12 × 7
#>    sample_id indextypology  year calc_method_old   ibi eqr_class   eqr
#>    <chr>     <chr>         <int> <lgl>           <dbl>     <dbl> <dbl>
#>  1 11611     upstream       2019 TRUE             3.22         3 0.544
#>  2 11652     brabeel        2019 FALSE            5.08         1 0.17 
#>  3 13282     brabeel        2020 FALSE            9            2 0.333
#>  4 13384     brabeel        2020 FALSE            8            2 0.292
#>  5 13512     brabeel        2021 FALSE           12            2 0.458
#>  6 13534     vlagzalm       2021 TRUE             4.33         4 0.767
#>  7 13561     brabeel        2021 FALSE            5.08         1 0.17 
#>  8 2258      forel          1994 TRUE             2.67         3 0.433
#>  9 4550      bron           2012 FALSE            7            2 0.25 
#> 10 8434      brabeel        2013 FALSE           12            2 0.458
#> 11 8507      brabeel        2013 FALSE            5.08         1 0.17 
#> 12 8681      brabeel        2014 FALSE           20            4 0.792
calculate_eqr(data_sample, data_fish, output = "metric")
#> Warning in calculate_eqr(data_sample, data_fish, output = "metric"): Some
#> taxoncodes given in data_fish are unknown fishes and these records will be
#> excluded from the analysis: ERI.SIN.
#> $eqr
#> # A tibble: 12 × 7
#>    sample_id indextypology  year calc_method_old   ibi eqr_class   eqr
#>    <chr>     <chr>         <int> <lgl>           <dbl>     <dbl> <dbl>
#>  1 11611     upstream       2019 TRUE             3.22         3 0.544
#>  2 11652     brabeel        2019 FALSE            5.08         1 0.17 
#>  3 13282     brabeel        2020 FALSE            9            2 0.333
#>  4 13384     brabeel        2020 FALSE            8            2 0.292
#>  5 13512     brabeel        2021 FALSE           12            2 0.458
#>  6 13534     vlagzalm       2021 TRUE             4.33         4 0.767
#>  7 13561     brabeel        2021 FALSE            5.08         1 0.17 
#>  8 2258      forel          1994 TRUE             2.67         3 0.433
#>  9 4550      bron           2012 FALSE            7            2 0.25 
#> 10 8434      brabeel        2013 FALSE           12            2 0.458
#> 11 8507      brabeel        2013 FALSE            5.08         1 0.17 
#> 12 8681      brabeel        2014 FALSE           20            4 0.792
#> 
#> $metric
#> # A tibble: 76 × 9
#>    sample_id indextypology  year metric_name metric_score_name method_for_metric
#>    <chr>     <chr>         <int> <chr>       <chr>             <chr>            
#>  1 11611     upstream       2019 ManBio      SManBio_upstream  E                
#>  2 11611     upstream       2019 ManTyp      SManTyp_upstream  E                
#>  3 11611     upstream       2019 Mangkw      SMangkw           E                
#>  4 11611     upstream       2019 Manmigw     SManmigw          E                
#>  5 11611     upstream       2019 Manswi      SManswi           E                
#>  6 11611     upstream       2019 MniInd      <NA>              E                
#>  7 11611     upstream       2019 MnsBen      SMnsBen           E                
#>  8 11611     upstream       2019 MnsTot      SMnsTot_upstream  E                
#>  9 11611     upstream       2019 MpiInvt     SMpiInvt_upstream E                
#> 10 11611     upstream       2019 Mpigesp     SMpigesp          E                
#> # ℹ 66 more rows
#> # ℹ 3 more variables: metric_name_ext <chr>, metric_value <chr>,
#> #   metric_score <chr>

# lake index
data_sample <- read.csv2(
  system.file("testdata/kallemoeie_sample.csv", package = "EQRfishes")
)
data_fish <- read.csv2(
  system.file("testdata/kallemoeie_fish_data.csv", package = "EQRfishes")
)
cluster <- data.frame(
  sample_id = c("Kallemoeie_e", "Kallemoeie_f"),
  index_cluster = "Kallemoeie"
)

calculate_eqr(data_sample, data_fish, cluster = cluster)
#> # A tibble: 1 × 8
#>   sample_id index_cluster indextypology  year calc_method_old   ibi eqr_class
#>   <lgl>     <chr>         <chr>         <int> <lgl>           <dbl>     <dbl>
#> 1 NA        Kallemoeie    lakes          2003 FALSE               3         2
#> # ℹ 1 more variable: eqr <dbl>
calculate_eqr(data_sample, data_fish, output = "metric", cluster = cluster)
#> $eqr
#> # A tibble: 1 × 8
#>   sample_id index_cluster indextypology  year calc_method_old   ibi eqr_class
#>   <lgl>     <chr>         <chr>         <int> <lgl>           <dbl>     <dbl>
#> 1 NA        Kallemoeie    lakes          2003 FALSE               3         2
#> # ℹ 1 more variable: eqr <dbl>
#> 
#> $metric
#> # A tibble: 7 × 10
#>   sample_id indextypology  year metric_name metric_score_name  method_for_metric
#>   <lgl>     <chr>         <int> <chr>       <chr>              <chr>            
#> 1 NA        lakes          2003 BenWei      SBenWei_lakes_fyke SF               
#> 2 NA        lakes          2003 ManTol      SManTol_lakes      SF               
#> 3 NA        lakes          2003 MniInd      <NA>               SF               
#> 4 NA        lakes          2003 MnsPis      SMnsPis_lakes      SF               
#> 5 NA        lakes          2003 MpiInv      SMpiInv_lakes      E                
#> 6 NA        lakes          2003 MpiOmn      SMpiOmn_lakes      SF               
#> 7 NA        lakes          2003 MpiSpa      SMpiSpa_lakes      E                
#> # ℹ 4 more variables: metric_name_ext <chr>, metric_value <chr>,
#> #   metric_score <chr>, index_cluster <chr>
```

Function `determine_indextypology()` can help which indextypology would
be appropriate to use for a specific location:

``` r
# load data
data_sample <- read.csv2(
  system.file("testdata/freshwater_sample.csv", package = "EQRfishes")
) %>%
  mutate(
    index_type_code = "ZTWA"
  )
# the following function adds an indextypology to the dataset
# the result can be used to calculate the EQR
data_sample <- determine_indextypology(data_sample)
```
