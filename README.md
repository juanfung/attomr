
<!-- README.md is generated from README.Rmd. Please edit that file -->

# attomr

<!-- badges: start -->

<!-- badges: end -->

The goal of attomr is to make calls to the ATTOM API easy from R\!
Relies on
[`httr`](https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html)
on the back end.

## Installation

In development. You can install the current version of attomr from
GitHub with:

``` r
devtools::install_github("juanfung/attomr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(attomr)

## basic example code
## NB: requires setting apikey variable
apikey = 'YOUR_API_KEY'

## (optional) set user agent globally
set_ua('github.com/juanfung/attomr')

## create full endpoint path
path = build_path('basic')

## basic list of query parameters
query = build_query('basic')

## append query parameters
query = update_query(
    query,
    list(address='721 Evergreen Terrace, Springfield'))

## make the query
response = attom_api(path, query, apikey)

## you can more easily iterate through a list of addresses:
testlist = list(
    ## Throws an error but does not stop
    list(address='741 EVERGREEN TERRACE, SPRINGFIELD'),
    list(address='4529 Winona Court, Denver, CO')
)

testout = search_list(queries=testlist, apikey=apikey, s='basic')

## parse responses to (list of) tables
testdf = parse_list(testout)
```

List of available endpoints:

  - ‘id’: (`/property/id`) returns list of properties that fit criteria
    (bedrooms, geoid)
  - ‘basic’: (`/property/basicprofile`) returns basic property info,
    most recent sale and taxes
  - ‘detail’: (`/property/detail`) property characteristics, given
    address; or property details, given attomid
  - ‘snapshot’: (`/property/snapshot`) return properties that fall
    within radius of lat/long
  - ‘address’: (`/property/address`) return properties that dall within
    radius of address or postalcode
  - ‘sales’: (`/sale/snapshot`) return sales within radius of property
