
# CitaviR <img src='man/figures/logo.png' align="right" height="138" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
maturing](https://lifecycle.r-lib.org/articles/figures/lifecycle-maturing.svg)](https://www.tidyverse.org/lifecycle/#maturing)

> This is an unofficial helper package for dealing with Citavi. </br> I
> am not affiliated with Citavi, just a fan.

The reference management software **Citavi** ([Official
Website](https://www.citavi.com/de), [Official
GitHub](https://github.com/Citavi)) allows for [exports to
Excel](https://www1.citavi.com/sub/manual6/en/index.html?exporting_to_excel.html).
With a bit of effort (i.e. via customized [Citavi
macros](https://www1.citavi.com/sub/manual6/en/index.html?add_on_display_macros.html))
it also allows for [imports from
Excel](https://github.com/Citavi/Macros/blob/master/CIM%20Import/CIM007%20Import%20arbitrary%20data%20from%20Microsoft%20Excel%20into%20custom%20fields%20of%20existing%20references%20by%20short%20title/readme.de.md).
`CitaviR` provides functionality for dealing with the data while it is
*outside* Citavi to get the most out of it. Using CitaviR goes hand in
hand with using the custom Citavi macros in the [CitaviRMacros
repository](https://github.com/SchmidtPaul/CitaviRMacros).

## Installation

You can install the development version of `CitaviR` from GitHub:

``` r
devtools::install_github('SchmidtPaul/CitaviR')
```

## Workflow

The work flow from start to finish is structured in five steps.

<img src="man/figures/Workflow.png" width="60%" />

| Step                      | CitaviR | Effort: 1st time setup | Effort: regular use |
| ------------------------- | ------- | ---------------------- | ------------------- |
| Step 1: Citavi to xlsx    |         | 😏                      | 😎                   |
| Step 2: xlsx to R         | ✅       | 😎                      | 😎                   |
| Step 3: Process data in R | ✅       | 😏                      | 😎                   |
| Step 4: R to xlsx         | ✅       | 😎                      | 😎                   |
| Step 5: xlsx to Citavi    |         | 💥                      | 😏                   |

**Effort levels:** low effort 😎; acceptable effort 😏; can be cumbersome
💥

### Example

You can find an example workflow on the [**Get Started
page**](https://schmidtpaul.github.io/CitaviR/articles/CitaviR.html).
