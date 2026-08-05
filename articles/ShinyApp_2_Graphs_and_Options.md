# ShinyApp_2_Graphs_and_Options

This vignette describes in detail graphing and data options available in
[av_runShiny()](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.html).
Graphing options are typically set within the boxes to the side of the
main screen, while all other options are set in the AVOPTS tab.

## Application Options

Application defaults are initialized upon setup, and are saved
persistently in a file ()`avpf_constants.RD`) within a cache directory
chosen by `tools::R_user_dir("alphavantagepf", which = "cache")`. Data
itself is stored in the same directory unless a **Cache data directory**
is set up. The options can be categorized within three groups: required
API options, data management options, and others.

![](img/av_opts_detail.jpg)

### Required API Options

This app is centered around the Alphavantage API, and requires a valid
API key and entitlement status to function.

| Field | Description |
|:--:|:---|
| av api key (A above) | API key obtained form Alphavantage. |
| av entitlement (B above) | Entitlement status, either `delayed` or `realtime` |

### Data Management Options

Data is stored in one or two directories, depending on whether a **Dump
directory** is set.

- Price series and user time series are kept in a cache directory which
  defaults to the temporary directory created for the package, but can
  be set in the Cache Data Directory (C above). Earnings data is stored
  in the same directory.
- The results of each call to the Alphavantage API can optionally be
  stored in a **Dump directory**. The purpose of this is to allow users
  to “scrape” their thoughts, analyses, etc. on a per-call basis. Please
  be aware that this does increase the time spent on each command
  issued. That directory is set in Dump Directory (D above) and data is
  stored as a named (by function call name) list of `data.tables()`.

The options associated with the dumping are given below:

| Field | Description |
|:--:|:---|
| C Cache Data | Directory to store time series, earnings, and earnings estimates |
| D AV dump directory | Directory to store API call results. Must be set for dumping to occur. |
| E Capture AV Data | What to capture |
|  | `None` (default): Turn capture off, even if the directory is set. |
|  | `nopricesonly`: Capture anything *but* time series data. |
|  | ‘all’ Capture all calls |
| F Update or Cumulative | `update` data captured by symbol, or (`cum`) capture every time with a timestamp. |
| G Data Saving Options | Other options |
|  | `SaveEveryAVCall`: Save all calls cumulatively when the call is made. |
|  | `CleanOnStart`: Delete dump data on every startup of the app. |
|  | `None`: Keep dumped data in memory, avoiding I/O overhead. |
|  | `SaveNowonOptUpdate` |
| H Max Earnings Age | Number of days to go before reloading earnings data. |
| G Max Price Age | Number of hours to go before refreshing price data. |

### Non-analytic Options

Non analytic features are set as check boxes, and are described in the
following table. Note that they only change “officially” when the Set
Opts button is pressed.

| Field | Description |
|:--:|:---|
| UseTotRtn | Use total return data (i.e. adjusted for splits and dividends in price series) |
| UseLivePx | Use separate Alphavantage calls to always have the most recent price point. |
| verbose | Display (or not) informative messages in the console. |
| data2clipboard | Copy select data from each analysis to the clipboard for pasting into other applications. |
|  | This is useful for other ad-hoc analysis without going through download boxes or extra buttons |
| persistOuput | Keep analyses output (graphs, tables, etc) until replaced by new ones |
| showGeneralHelp | Show (or not) a general help screen when `AV.H` is run. |
| showWarnings | Suppress warnings from other packages (e.g. ggplot2) |

### Analytics Options

These are default statistical parameters:

| Field | Description |
|:--:|:---|
| J HistVolParams | Historical volatility parameters, see [TTR::volatility](https://www.rdocumentation.org/packages/TTR/versions/0.24.4/topics/volatility) |
| K Regr Significance | Significance level below which regression results are highlighted |

## Graphing Options

The default graphing package used is
[FinanceGraphs](https://derekholmes0.github.io/FinanceGraphs/) which
provides finance-specific graphing functions based on
[dygraphs](https://rstudio.github.io/dygraphs/) and
[ggplot2](https://ggplot2.tidyverse.org/reference/index.html). Any of
the features described there can be used, most notably:

- **Event Sets** which can be used to highlight events or regimes in a
  time series.
- **Automated rescaling** of data to put in total return (index) terms.
- **Annotations** to highlight levels on a graph, e.g. last values.
- **Full aesthetic control** of the graphing output, including colors,
  line types, and point types. For example, to change color sets for
  timeseries lines, use an aesthtic set (e.g. `lines` or `altlines_6`,
  specified in **L** above. See [Color
  customization](https://derekholmes0.github.io/FinanceGraphs/articles/Time-Series-dygraph.html#customization-colors)

Specific options for this app are described below, referencing this
example:

![GraphOptions](img/GraphOpts.jpg)

GraphOptions

- **Historical timeframes** are given by date strings in ther
  `HistDates` box. For example, time series for the past 4 years from
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html) are specified as
  `"-4y::"`. Note this is the *parameter is used for all historical
  analyses and requests*, not just graphs. For example, `IBM EA` would
  only report 4 years of data given the configuration described here.

- **Time Series Opts** are options that can be used to decorate or
  modify a time series graph. Choices are

|    TS Choice     | Description                                  |
|:----------------:|:---------------------------------------------|
|      `last`      | Add last value for each series at last point |
|    `splitts`     | Split first series into separate axis        |
|   `lastlabel`    | Add label for series at last point           |
| `highlightfirst` | Make first series bolder                     |
|     `hilow`      | Add high-low ranges if available             |

- **Scatter Options** apply to scatterplots, and are:

| Scatter Choice | Description |
|:--:|:---|
| `last` | Show last value as large point |
| `tailhedge` | Split return scatter plot regressions into three piecewise regressions, one each for big negative returns, big positive returns, and everything else |

- **Events** are dates or date ranges highlighted in the graph. Any
  valid event string can be used. For example, `tp,5` finds 5 turning
  points for the first series plotted, as seen in the picture above.

### Enhancements for this app.

A few enhancements have been added to the
[av_runShiny()](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.html)
set of options.

- **Tail Hedge regressions** (see above) separate out large moves from
  smaller moves in asset regressions.
- **Extra events**. Im addition to any events defined or created from
  the [FinanceGraphs](https://derekholmes0.github.io/FinanceGraphs/)
  package, you can also add to timeseries:

|  Event   | Description                                                  |
|:--------:|:-------------------------------------------------------------|
|  `earn`  | Show EPS at report dates                                     |
|  `surp`  | Show earnings suprises at report dates, color coded by sign. |
|  `div`   | Show dividends at ex-dates                                   |
| `divpct` | Show dividends as percent of close at ex-dates               |
