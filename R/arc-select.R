#' Retrieve a feature layer
#'
#' Give a `FeatureLayer` or `Table` object, retrieve its data as an `sf` object or `tibble` resepctively.
#'
#' @inheritParams source
#' @param x an object of class `FeatureLayer` or `Table`.
#' @param fields a character vector of the field names that you wish to be returned. By default all fields are returned.
#' @param where a simple SQL where statement indicating which features should be selected.
#' @param crs the spatial reference to be returned. If the CRS is different than the `FeatureLayer`'s CRS, a transformation will occur server-side. Ignored for `Table` objects.
#' @param n_max the maximum number of features to return. By default returns every feature available. Unused at the moment.
#' @param ... additional query parameters passed to the API. See [reference documentation](https://developers.arcgis.com/rest/services-reference/enterprise/query-feature-service-layer-.htm#GUID-BC2AD141-3386-49FB-AA09-FF341145F614) for possible arguments.
#' @export
arc_select <- function(
    x,
    fields,
    where,
    crs = sf::st_crs(x),
    filter_geom,
    predicate = "intersects",
    n_max = Inf,
    ...
) {
  # Developer note:
  # For this function we extract the query object and manipulate the elements
  # inside of the query object to modify our request. We then splice those
  # values back into `x` and send our request
  # note that everything that goes into our quey must be the json that will
  # be sent directly to the API request which is why we convert it to json
  # before we use `update_params()`

  # object class checking
  obj_check_layer(x)

  # extract the query object
  query <- attr(x, "query")

  # modify fields if not missing
  if (!missing(fields) && (length(fields) > 1)) {
    # if not missing fields collapse to scalar character
    # check if incorrect field names provided
    x_fields <- x[["fields"]][["name"]]
    nindex <- tolower(fields) %in% tolower(x_fields)

    # handle the case where a field is being selected that
    # is not one of the available fields in the feature layer
    if (any(!nindex)) {
      cli::cli_abort(
        "Field{?s} not in {.arg x}: {.var {fields[!nindex]}}"
      )
    }
    # collapse together
    query[["outFields"]] <- paste0(fields, collapse = ",")
  }

  # handle filter geometry if not missing
  if (!missing(filter_geom)) {
    # determine the appropriate crs
    # should probably be made into a utility function to impute crs
    filt_crs <- if (is.na(sf::st_crs(filter_geom))) {
      sf::st_crs(x)
    } else {
      sf::st_crs(filter_geom)
    }
    spatial_filter <- prepare_spatial_filter(filter_geom, filt_crs, predicate)

    # append spatial filter fields to the query
    query <- c(query, spatial_filter)
  }

  # handle SR if not missing
  if (!is.na(crs)) {
    query[["outSR"]] <- jsonify::to_json(validate_crs(crs)[[1]], unbox = TRUE)
  }

  # update the parameters based on our query list
  x <- update_params(x, !!!query)

  # send the request
  collect_layer(x, n_max = n_max, ...)
}

# This is the workhorse function that actually executes the queries
#' @keywords internal
collect_layer <- function(x, n_max = Inf, token = Sys.getenv("ARCGIS_TOKEN"), ..., error_call = rlang::caller_env()) {

  obj_check_layer(x, call = error_call)

  # 1. Make base request
  # 2. Identify necessary query parameters
  # 3. Figure out offsets and update query parameters
  # 4. Make list of requests
  # 5. Make requests
  # 6. Identify errors (if any) -- skip for now
  # 7. Parse:
  req <- httr2::request(x[["url"]])

  # stop if query not supported
  if (!grepl("query", x[["capabilities"]], ignore.case = TRUE)) {
    cli::cli_abort(
      "{class(x)} {.val {x[['name']]}} does not support querying",
      call = error_call
    )
  }

  # extract existing query
  query <- attr(x, "query")

  # set returnGeometry depending on on geometry arg and is FeatureLayer
  if (is.null(query[["returnGeometry"]]) && inherits(x, "FeatureLayer")) {
    query[["returnGeometry"]] <- TRUE
  }

  # if the outSR isn't set, set it to be the same as x
  if (inherits(x, "FeatureLayer") && is.null(query[["outSR"]])) {
    query[["outSR"]] <- jsonify::to_json(validate_crs(sf::st_crs(x))[[1]], unbox = TRUE)
  }

  # parameter validation ----------------------------------------------------
  # get existing parameters
  query_params <- validate_params(query, token = token)

  # Offsets -----------------------------------------------------------------
  feats_per_page <- x[["maxRecordCount"]]

  # count the number of features in a query
  n_req <-
    httr2::req_url_query(
      httr2::req_url_query(
        httr2::req_url_path_append(req, "query"),
        !!!query_params[c("where", "outFields", "f", "token")]
      ),
      returnCountOnly = "true"
    )

  suppressMessages(
    n_feats <- httr2::resp_body_json(
      httr2::req_perform(
        httr2::req_url_query(n_req, f = "pjson"),
        error_call = error_call
      ), check_type = FALSE
    )[["count"]]
  )

  if (is.null(n_feats)) {
    cli::cli_abort(
      c("Can't determine the number of features for {.arg x}.",
      "*" = "Check to make sure your {.arg where} statement is valid."),
      call = error_call
    )
  }

  # identify the number of pages needed to return all features
  # if n_max is provided need to reduce the number of pages
  if (n_feats > n_max) {
    n_feats <- n_max
    # set `resultRecordCount` to `n_max`
    query_params[["resultRecordCount"]] <- n_max
  }

  n_pages <- floor(n_feats / feats_per_page)

  # identify the offsets needed to get all pages
  # if n_pages is 0 we set offsets to 0 straight away
  if (n_pages == 0) {
    offsets <- 0
  } else {
    offsets = c(0, (feats_per_page * 1:n_pages) + 1)
  }

  # create a list of requests
  all_requests <- lapply(offsets, add_offset, req, query_params)

  # make all requests and store responses in list
  all_resps <- httr2::multi_req_perform(all_requests)

  # identify any errors
  has_error <- vapply(all_resps, function(x) inherits(x, "error"), logical(1))
  #
  #   if (any(has_error)) {
  # TODO determine how to handle errors
  #   }

  # fetch the results
  res <- lapply(
    all_resps[!has_error],
    function(x) parse_esri_json(
      httr2::resp_body_string(x)
    )
  )

  # combine
  res <- do.call(rbind, res)

  if (is.null(res)) {
    cli::cli_alert_info(
      "No features returned from query",
      call = error_call
    )

    return(data.frame())
  }

  if (inherits(res, "sf")) sf::st_crs(res) <- sf::st_crs(x)

  res

}


# utility -----------------------------------------------------------------

#' Check if x is a FeatureLayer or Table class object
#' @keywords internal
obj_check_layer <- function(x,
                            arg = rlang::caller_arg(x),
                            call = rlang::caller_env()) {
  check_inherits_any(
    x,
    class = c("FeatureLayer", "Table"),
    arg = arg,
    call = call
  )
}

#' Check if x inherits any of the supplied class values and error if not
#' @inheritParams cli::cli_vec
#' @inheritParams rlang::inherits_any
#' @inheritParams rlang::args_error_context
#' @keywords internal
check_inherits_any <- function(x,
                               class,
                               arg = rlang::caller_arg(x),
                               call = rlang::caller_env()) {
  if (rlang::inherits_any(x, class)) {
    return(invisible(NULL))
  }

  class <- cli::cli_vec(
    class,
    style = list("before" = "`", "after" = "`", "vec-last" = " or ")
  )

  cli::cli_abort(
    "{.arg {arg}} must be a {class} object, not {.obj_simple_type {x}}.",
    call = call
  )
}

# This is the function that takes named arguments and updates the query
#' Modify Query Parameters
#'
#' @param x a `FeatureLayer` object
#' @param ... key value pairs of query parameters and values.
#' @keywords internal
update_params <- function(x, ...) {
  query <- attr(x, "query")
  params <- rlang::list2(...)

  for (name in names(params)) {
    query[[name]] <- params[[name]]
  }

  attr(x, "query") <- query
  x
}

#' This function takes a list of query parameters and creates a query request
#' Importantly, this creates the paginated results that will be needed for
#' Feature Layers with more than 2000 observations
#' @keywords internal
add_offset <- function(offset, request, params) {
  params[["resultOffset"]] <- offset
  req <- httr2::req_url_path_append(request, "query")
  httr2::req_url_query(req, !!!params)
}


# This function ensures that the minimal parameters are correctly set

#' @keywords internal
validate_params <- function(params, token) {
  # set the token
  params[["token"]] <- token

  # if output fields are missing set to "*"
  if (is.null(params[["outFields"]])) params[["outFields"]] <- "*"

  # if where is missing set it to 1=1
  if (is.null(params[["where"]])) params[["where"]] <- "1=1"

  # set output type to geojson if we return geometry, json if not
  if (is.null(params[["returnGeometry"]]) || isTRUE(params[["returnGeometry"]])) {
    params[["f"]] <- "json"
  } else {
    params[["f"]] <- "json"
  }

  params
}


