## R client for ATTOM API
## Author: Juan F. Fung
## Date: 2020-10-16

## create new environment for local variables
attomr.env = new.env()

## header
## [TODO] allow user to set Accept header;
## - no immediate need for xml, so no function needed
assign('accept', 'application/json', envir=attomr.env) ## default

## set API Key
## [TODO] allow user to (optionally) set at beginning of session

## define base url and endpoint (NB: endpoint may change)
assign('base_url', 'https://api.gateway.attomdata.com', envir=attomr.env)
assign('endpoint', '/propertyapi/v1.0.0/', envir=attomr.env)

## variables for specific endpoints (endpoint + service)
assign('endpoint_list',
       list(
           ## /property/id
           ## - return list of properties that fit criteria (bedrooms, geoid)
           'id'='property/id',
           ## /property/basicprofile
           ## - return basic property info, most recent sale and taxes 
           'basic'='property/basicprofile',
           ## /property/detail
           ## - property characteristics, given address
           ## - property details, given attomid
           'detail'='property/detail',
           ## /property/snapshot
           ## - returns properties within radius of lat/long
           ## - returns properties and characteristics by city and lotsize
           ## - properties wihin postalcade by UniversalSize
           'snapshot'='property/snapshot',
           ## /property/address
           ## - return properties that fall within radius of address
           ## - return properties within postalcode
           'address'='property/address',
           ## /sale/snapshot
           ## - return sales within radius of property
           ## - return sales within geography (geoid)
           'sales'='sale/snapshot'
       ),
       envir=attomr.env
       )



## functions

#' function to optionally set user agent globally
#'
#' @param a A string to pass to httr::user_agent
#'
#' @export
set_ua = function(a){
    assign(x='ua',
           value=httr::user_agent(a),
           envir=attomr.env)
    message(sprintf('User agent set to %s.', a))
}

#' GET call to API
#'
#' This function takes an API endpoint path, query parameters, and the
#' user's API key to make a GET call to the ATTOM API (borrowed from httr
#' vignette).
#'
#' List of available endpoints:
#' - 'id': (`/property/id`) returns list of properties that fit criteria (bedrooms, geoid)
#' - 'basic': (`/property/basicprofile`) returns basic property info, most recent sale and taxes
#' - 'detail': (`/property/detail`) property characteristics, given address; or property details, given attomid
#' - 'snapshot': (`/property/snapshot`) return properties that fall within radius of lat/long
#' - 'address`: (`/property/address`) return properties that dall within radius of address or postalcode
#' - 'address': (`/sale/snapshot`) return sales within radius of property
#' 
#' @param path Path to endpoint (e.g., '/endpoint/property/snapshot')
#' @param query List of named parameters to pass to query
#' @param apikey The user's ATTOM API Key (required)
#'
#' @return An object of class `attom_api` that includes parsed
#'     response, the path queried, and the raw response
#' @export
attom_api = function(path, query, apikey) {
    ## Client to GET and parse response from API
    url = httr::modify_url(attomr.env$base_url, path=path)
    resp = httr::GET(url=url,
                     ## user agent
                     ifelse(exists('ua', where=attomr.env),
                            attomr.env$ua,
                            httr::user_agent('https://github.com/juanfung/attomr')),
                     ## header parameters
                     httr::add_headers(
                               Accept=attomr.env$accept,
                               apikey=apikey),
                     ## query parameters
                     query=query
                     ## [TODO] use httr::with_verbose
                     ## verbose()
                     )
    ## Check expected response format
    if (httr::http_type(resp) != attomr.env$accept) {
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
#' @param ... Other named parameters passed to print method
#'
#' @importFrom utils str
#'
#' @export
print.attom_api = function(x, ...) {
    ## [TODO] add more functionality
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
    if (s %in% names(attomr.env$endpoint_list)) {
        path = paste0(attomr.env$endpoint, attomr.env$endpoint_list[[s]])
    } else {
        warning('Invalid search option.', call.=FALSE)
        path = attomr.env$endpoint
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
#' @param update A list of named parameters to append to query
#'
#' @return query An updated list of named query parameters
#'
#' @export
update_query = function(query, update) {
    updates = names(update)
    knowns = c('address', 'address1', 'address2', 'longitude', 'latitude', 'radius')
    if (length(setdiff(knowns, updates)) == length(knowns)) {
        warning('Invalid or missing query parameters', call.=FALSE)
    } else {
        for (k in knowns) {
            if (k %in% updates) {
                query[[k]] = update[[k]]
            }
        }
    }
    ## TODO: check that {address, address1, address2} are not passed together
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

#' Function to create list of lists of addresses from data frame
#'
#' @param d A data frame such that each row is an address
#'
#' @return queries A list of lists of queries
#'
#' @export
create_queries = function(d) {
    ## convert each row list and collect all into a single list
    ## [NB] Is it easier to pass data frame of addresses to search_list?
    ## eg: if (class(queries) != "list") {queries = create_queries(queries)}
    queries = lapply(seq(nrow(d)), function(i) as.list(d[i, ]))
    return(queries)
}
