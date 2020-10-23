## R client for ATTOM API
## Author: Juan F. Fung
## Date: 2020-10-16

## header
accept = 'application/json' ## default

## set user agent
## [TODO] allow user to set user agent
ua = httr::user_agent('https://juanfung.github.io')

## define base url and endpoint (NB: endpoint may change)
base_url = 'https://api.gateway.attomdata.com'
endpoint = '/propertyapi/v1.0.0/'

## variables for specific endpoints (endpoint + service)

## /property/id
## - return list of properties that fit criteria (bedrooms, geoid)
endpoint_id = paste0(endpoint, 'property/id')

## /property/basicprofile
## - return basic property info, most recent sale and taxes 
endpoint_basicprofile = paste0(endpoint, 'property/basicprofile')

## /property/detail
## - property characteristics, given address
## - property details, given attomid
endpoint_detail = paste0(endpoint, 'property/detail')

## /property/snapshot
## - returns properties within radius of lat/long
## - returns properties and characteristics by city and lotsize
## - properties wihin postalcade by UniversalSize
endpoint_snapshot = paste0(endpoint, 'property/snapshot')

## /property/address
## - return properties that fall within radius of address
## - return properties within postalcode
endpoint_address = paste0(endpoint, 'property/address')

## /sale/snapshot
## - return sales within radius of property
## - return sales within geography (geoid)
endpoint_sales = paste0(endpoint, 'sale/snapshot')


## functions

#' GET call to API
#'
#' This function takes an API endpoint path, query parameters, and the
#' user's API key to make a GET call to the ATTOM API (borrowed from httr
#' vignette).
#' @param path Path to endpoint (e.g., '/endpoint/property/snapshot')
#' @param query List of named parameters to pass to query
#' @param apikey The user's ATTOM API Key (required)
#'
#' @return An object of class `attom_api` that includes parsed
#'     response, the path queried, and the raw response
#' @export
attom_api = function(path, query, apikey) {
    ## Client to GET and parse response from API
    url = httr::modify_url(base_url, path=path)
    resp = httr::GET(url=url,
                     ua,
                     httr::add_headers(
                               Accept=accept,
                               apikey=apikey),
                     query=query
                     ## [TODO] use httr::with_verbose
                     ## verbose()
                     )
    ## Check expected response format
    if (httr::http_type(resp) != "application/json") {
        warning("API did not return json", call. = FALSE)
    }
    parsed = jsonlite::fromJSON(httr::content(resp, 'text'))
    ## Print error message
    if (httr::http_error(resp)) {
        warning(
            sprintf(
                'API request failed [%s]\n%s',
                httr::status_code(resp),
                parsed$status$msg),
            call. = FALSE
        )
    }
    structure(
        list(
            parsed=parsed,
            path=path,
            resp=resp),
        class='attom_api'
    )
}


#' Print GET response
#'
#' Adds print method to object of class `attom_api` (borrowed from
#' httr vignette)
#'
#' @param x An object of class `attom_api`
#'
#' @export
print.attom_api = function(x, ...) {
    ## [TODO]
    cat("<ATTOM ", x$path, ">\n", sep = "")
    str(x$parsed)
    invisible(x)
}

#' Function to create full endpoint path
#'
#' This helper function creates the full endpoint path (e.g.,
#' 'endpoint/property/snapshot) to pass to function attom_api()
#'
#' @param s One of 'basic' (for 'property/basicprofile'), 'detail'
#'     (for 'property/detail'), 'address' (for 'property/address'),
#'     or 'sales' (for 'sale/snapshot')
#'
#' @return path Full endpoint path (endpoint/service)
#'
#' @export
build_path = function(s) {
    if (s == 'basic') {
        path = endpoint_basicprofile
    } else if (s == 'detail') {
        path = endpoint_detail
    } else if (s == 'address') {
        path = endpoint_address
    } else if (s == 'sales') {
        path = endpoint_sales
    } else {
        warning('Invalid search option.', call.=FALSE)
        path = endpoint
    }
    return(path)
}

#' Function to build base query
#'
#' This helper function builds a basic list of query parameters that
#' is common across queries
#'
#' @param s One of 'basic' (for 'property/basicprofile'), 'detail'
#'     (for 'property/detail'), 'address' (for 'property/address'),
#'     or 'sales' (for 'sale/snapshot')
#' @param ... Other named query parameters
#'
#' @return query List of query parameters to pass to attom_api()
#'
#' @export
build_query = function(s, ...) {
    args = list(...)
    ## if (length(args) == 0) {
    ##     warning('No arguments to build query.', call.=FALSE)
    ## }
    query = list()
    if (s %in% c('basic', 'detail')) {
        ## Return empty list
    } else if (s %in% c('address', 'sales')) {
        query[['radius']] = ifelse('radius' %in% names(args), args$radius, 20)
        query[['propertytype']] = ifelse('propertytype' %in% names(args), args$propertytype, 'SFR')
        query[['page']] = ifelse('page' %in% names(args), args$page, 1)
        query[['pagesize']] = ifelse('pagesize' %in% names(args), args$pagesize, 100)
        if (s == 'sales') {
            query[['minsaleamt']] = ifelse('min' %in% names(args), args$min, 100000)
            query[['maxsaleamt']] = ifelse('max' %in% names(args), args$max, 1000000)
        }
    } else {
        warning('Missing or invalid query parameters.', call.=FALSE)
    }
    return(query)
}

#' Function to update query
#'
#' This helper function updates a list of query parameters by
#' appending new parameters to the query
#'
#' @param query A list of named query parameters
#' @param l A list of named parameters to append to query
#'
#' @return query An updated list of named query parameters
#'
#' @export
update_query = function(query, l) {
    ## [TODO] Generalize to search by address or by lat/long (property/snapshot)
    if (length(l) == 1) {
        address = l[['address']]
        if (is.null(address)) {
            warning('No address parameter provided for query', call.=FALSE)
        }
        query[['address']] = address
    } else if (length(l) == 2) {
        address1 = l[['address1']]
        address2 = l[['address2']]
        if (is.null(address1) | is.null(address2)) {
            warning('Incomplete address parameters provided for query', call.=FALSE)
        }
        query[['address1']] = address1
        query[['address2']] = address2
    } else {
        warning('Invalid or missing query parameters', call.=FALSE)
    }
    return(query)
}

#' Function to iterate through list of queries (list of addresses)
#'
#' Given a list of queries, iteratively call attom_api() by building
#' and passing a query
#'
#' @param queries A list of lists, where each list is a set of query
#'     parameters to pass to attom_api()
#' @param apikey The user's ATTOM API Key
#' @param s One of 'basic' (for 'property/basicprofile'), 'detail'
#'     (for 'property/detail'), 'address' (for 'property/address'),
#'     or 'sales' (for 'sale/snapshot')
#' @param ... Other named query parameters (passed to build_query())
#'
#' @return responses A list of responses of class `attom_api`, for each
#'     item in the list queries
#'
#' @export
search_list = function(queries, apikey, s, ...) {
    ## [TODO] parameterize user agent?
    ## [TODO] enforce checking daily/monthly limits
    ## - 5000 *parcels*/day
    ## - 15000 *requests*/month
    path = build_path(s)
    responses = list()
    query = build_query(s, ...)
    for (i in 1:length(queries)) {
        query = update_query(query, queries[[i]])
        responses[[i]] = attom_api(path, query, apikey)
        Sys.sleep(5)
    }
    return(responses)
}

#' Function to post-process list of responses
#'
#' Given a list of responses, extract the response data.frame that
#' contains the property information (coerced to tibble)
#'
#' @param resps List of responses (e.g., from search_list())
#'
#' @return parsed_list List of tibbles with response property data
#'
#' @export
parse_list = function(resps) {
    ## [TODO] add query ID
    ## iterate through list of resps
    parsed_list = list()
    for (i in 1:length(resps)) {
        ## append (table of) responses to list
        if (!httr::http_error(resps[[i]][['resp']])) {
            ## skip responses with errors, since nothing to append
            parsed_list[[i]] = tibble::as_tibble(resps[[i]][['parsed']][['property']])
        }
    }
    return(parsed_list)
}
