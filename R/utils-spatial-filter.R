
#' Prepare json for spatial filters
#'
#' @param filter_geom an object of class `bbox`, `sfc` or `sfg` used to filter
#'   query results based on a predicate function. If an `sfc` object is provided
#'   it will be transformed to the layers spatial reference. If the `sfc` is
#'   missing a CRS (or is an `sfg` object) it is assumed to be in the layers
#'   spatial reference. If an `sfc` object has multiple features, the features
#'   are unioned with [sf::st_union()]. If an `sfc` object has MULTIPOLYGON
#'   geometry, the features are treated as polygonal coverage and unioned with
#'   `is_coverage = TRUE` before being cast to POLYGON geometry with
#'   [sf::st_cast()].
#' @param predicate default `"intersects"`. Possible options are `"intersects"`,  `"contains"`,  `"crosses"`,  `"overlaps"`,  `"touches"`, and `"within"`.
#'
#' ### Spatial Binary Predicates:
#'
#' - esriSpatialRelIntersects
#' - esriSpatialRelContains
#' - esriSpatialRelCrosses
#' - esriSpatialRelOverlaps
#' - esriSpatialRelTouches
#' - esriSpatialRelWithin
#' @export
#' @rdname spatial_filter
prepare_spatial_filter <- function(
    filter_geom,
    crs,
    predicate
) {

  # Developer Note: CRS cannot be missing
  if (inherits(filter_geom, "bbox")) {
    filter_geom <- sf::st_as_sfc(filter_geom)
  }

  # if its an sfc object it must be length one
  if (inherits(filter_geom, "sfc")) {
    if (length(filter_geom) > 1) {
      filter_geom <- sf::st_union(filter_geom)
    }

    # extract the sfg object which is used to write Esri json
    filter_geom <- filter_geom[[1]]
  }

  # if a multi polygon stop, must be a single polygon see
  # related issue: https://github.com/R-ArcGIS/arcgislayers/issues/4
  if (inherits(filter_geom, "MULTIPOLYGON")) {
    cli::cli_inform(
      c(
        "!" = "{.arg filter_geom} cannot be a {.val MULTIPOLYGON} geometry.",
        "i" = "Using {.fn sf::st_union} and {.fn sf::st_cast} to create a
        coverage {.val POLYGON} for {.arg filter_geom}."
      ),
      call = rlang::caller_env()
    )

    filter_geom <- sf::st_union(filter_geom, is_coverage = TRUE)
    filter_geom <- sf::st_cast(filter_geom, to = "POLYGON")
  }

  list(
    geometryType = determine_esri_geo_type(filter_geom),
    geometry = as_esri_geometry(filter_geom),
    spatialRel = match_spatial_rel(predicate)
    # TODO is `inSR` needed if the CRS is specified in the geometry???
  )
}


#' @export
#' @rdname spatial_filter
match_spatial_rel <- function(predicate) {
  # determine the spatial relationship (predicate)
  predicate <- tolower(predicate)
  esri_predicates <- c(
    "esriSpatialRelIntersects",
    "esriSpatialRelContains",
    "esriSpatialRelCrosses",
    # "esriSpatialRelEnvelopeIntersects",
    #   - don't provide this, just provide bbox and intersects
    # "esriSpatialRelIndexIntersects", idk what this is remove
    "esriSpatialRelOverlaps",
    "esriSpatialRelTouches",
    "esriSpatialRelWithin"
  )

  pred_arg_vals <- tolower(
    substr(esri_predicates, 15, nchar(esri_predicates))
  )

  # ensure a correct one has been chosen
  rlang::arg_match(predicate, pred_arg_vals)

  esri_predicates[grepl(predicate, esri_predicates, ignore.case = TRUE)]
}
