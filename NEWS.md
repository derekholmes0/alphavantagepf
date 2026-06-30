# alphavantagepf (development version)

# alphavantagepf 0.8.2: in Progress

Bug todo:

New feature todo

* Scattperplot first on second with run of secondTS
* Total return and basic statistics with graphed prices.
* Saving dividends and Earnings into database.
* Dividends and earnings into px graph
* P/E and P/D graphs
* Hooks for external functions
* Statistics
* calendars
* reset splitfirst on ActiveTS -- very confusing

New Features

* `av_runShiny()` now returns a `ShinyAppHandle` object and does not block execution
* `av_extract_df` now returns an empty data.table if there is nothing to extract. Set `empty_dt_onerror=FALSE` to throw an error.
* Scatter plot added to ActiveTS, with piecewise linear regression


Small Changes

* Improved filtering and search acrosss tables.
* Option sets and parameters are now persistent
* Slider for date windows now replaced  with date string
* GUI reorganization, including moving volatility parameters to AVOPTS

Bug fixes

* Fixed app crash if no earnings transcript available.
* Fixed graphing so that stepPlots are chosen appropriately, not always.

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
