# alphavantagepf (development version)

# alphavantagepf 0.8.4: in Progress

New feature todo
* calendars

## Major Changes

The Shiny interface has been completely redesigned (and recoded) to have an extendable command line interface
for all analytics.  New functions have been added for adding and managing data  outside the app. Users can also add their
own analytics, posssibly with the help of with several "helper" functions to interface with the GUI.  See vignettes
for details.

## New Features in API interface

* `av_get_pf()` returns a date column for melted data.
* `av_runShiny()` now returns a `ShinyAppHandle` object and does not block execution
* All helper functions (e.g. `av_extract_df`) now return an empty data.table if there is nothing to extract. Set `empty_dt_onerror=FALSE` to throw an error.
* `av_extract_fx()`  timestamps always returned in `Sys.timezone()` time zone.

Bug fixes

* Fixed app crash if no earnings transcript available.
* Fixed graphing so that stepPlots are chosen appropriately, not always.
* `splitfirst` ignored for single name plots.
* `av_get_pf(.,"OVERVIEW")` now properly produces a type-separated table.

## Breaking changes

* `av_api_key()` has been renamed to `avpf_api_key()` which has an additional parameter `entitlement`.  `av_api_key()`
is also in the `alphavantager` package.  Usage of that function is still compatible with this package, but an additional
step to set entitlement status is necessary.

# alphavantagepf 0.8.1 (RELEASE)

* Fixed initial user directory creation problem.

# alphavantagepf 0.8.0

* Release to CRAN

# alphavantagepf 0.7.8

* CRAN Ready
* av_extract_df now only extracts non-empty data.frames.

# alphavantagepf 0.7.7

* Documentation, refactors, small fixes in DES and av_add_data
* Formatting changes, fix live feeds for user and Crypto Data
* Added support for user supplied data.

# alphavantagepf 0.7.5

* Fixed cryptocurrency download conventions
* Enhanced Shiny data capture to external data and crypto.

# alphavantagepf 0.7.4

* Added timing delay to av_get_pf for multiple small requests.

# alphavantagepf 0.7.3

* Added support for INDEX_DATA and FX
* Refactored AV capture
* Redesigned NEWS page
* Eliminated hash package dependencies

# alphavantagepf 0.7.2

* Fixed parameter mapping for NEWS_SENTIMENT given ticker
* Added av_runShiny()
* Added av_extract_divs_or_splits()

# alphavantagepf 0.7.0

* Updated function maps, including INDEX
* Added new options to av_grep_opts
* Fixed Juneteenth option expiration calculation

# alphavantagepf 0.6.1

* Start of Shiny interface

# alphavantagepf 0.4.0

* Make sure empty columns are returned as reals
* Use default user-agent

# alphavantagepf 0.3.2

* CRAN feedback addressed

# alphavantagepf 0.3.1

* Filters NULL inputs from url creation.

# alphavantagepf 0.3.0

* Documentation changes

# alphavantagepf 0.2.1

* Initial CRAN submission.
