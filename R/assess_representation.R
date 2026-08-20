#' Assess regional representation of features within conservation areas
#'
#' Calculates the representation of features within each conservation area
#' relative to their representation across the full regional dataset. The
#' function supports both polygon/multipolygon and point/multipoint spatial
#' data. For polygon data, representation is calculated as the proportion of
#' each feature's total area that occurs within a conservation area. For point
#' data, representation is calculated as the proportion of observations for
#' each feature that occur within a conservation area.
#'
#' Conservation areas are then ranked within their respective regions using
#' the calculated representation values. The area with the lowest
#' representation receives the lowest score and the area with the highest
#' representation receives the highest score, with scores ranging from 0 to
#' 100. Areas with no represented features receive a representation value of
#' zero.
#'
#' @param data An \code{sf} object containing the spatial features or
#'   observations to assess. Must contain either polygon/multipolygon or
#'   point/multipoint geometries.
#' @param areas An \code{sf} object containing the conservation areas to
#'   assess.
#' @param indicator_var_name A character string or tidy-evaluation expression
#'   identifying the column in \code{data} that contains the feature or
#'   indicator identifier.
#' @param areaID A character string or tidy-evaluation expression identifying
#'   the column in \code{areas} containing the unique conservation area
#'   identifier.
#' @param regionID A character string or tidy-evaluation expression identifying
#'   the column in \code{areas} containing the regional grouping used to rank
#'   conservation areas.
#' @param scoring A character string specifying the scoring scheme. Must be
#'   \code{"representation: regional relative ranking"}.
#' @param indicator Optional indicator name to include in the output.
#' @param type Optional indicator type to include in the output.
#' @param units Optional units associated with the indicator to include in the
#'   output.
#' @param PPTID Optional project or pressure-threat identifier(s) to include
#'   in the output.
#' @param project_short_title Optional project short title to include in the
#'   output.
#' @param climate Optional climate category or classification to include in
#'   the output.
#' @param design_target Optional design target associated with the indicator
#'   to include in the output.
#' @param other_nest_variables Optional character vector of additional columns
#'   from \code{data} to retain in the nested output data.
#'
#' @return A data frame containing one row per conservation area, including
#'   the regional relative-ranking score and nested information on the
#'   represented features. The returned object includes the following
#'   columns:
#'   \describe{
#'     \item{areaID}{The conservation area identifier.}
#'     \item{data}{A nested data frame containing the represented features and
#'       any requested additional variables.}
#'     \item{score}{A 0--100 regional relative-ranking score.}
#'     \item{indicator}{The supplied indicator name.}
#'     \item{type}{The supplied indicator type.}
#'     \item{units}{The supplied indicator units.}
#'     \item{scoring}{The scoring scheme used.}
#'     \item{PPTID}{The supplied project or pressure-threat identifier(s).}
#'     \item{project_short_title}{The supplied project short title.}
#'     \item{climate}{The supplied climate classification.}
#'     \item{design_target}{The supplied design target.}
#'     \item{status_statement}{A text description of the representation
#'       achieved within the conservation area.}
#'     \item{trend_statement}{A statement indicating that the assessment has
#'       no temporal dimension.}
#'   }
#'
#' @details
#' For polygon data, the function calculates representation separately for
#' each feature as the area of the feature occurring within a conservation
#' area divided by the feature's total area. The area-level representation is
#' then calculated as the mean representation across represented features.
#'
#' For point data, the function calculates representation for each feature as
#' the number of unique observations occurring within a conservation area
#' divided by the total number of observations for that feature in the input
#' dataset. The area-level representation is then calculated as the mean
#' representation across represented features.
#'
#' Conservation areas are ranked separately within each region using
#' \code{\link[dplyr]{cume_dist}}. A single conservation area within a region
#' is assigned a score of 100. Conservation areas with no represented
#' features are assigned a representation value of zero before regional
#' ranking.
#'
#' The spatial reference system of \code{data} is transformed to match
#' \code{areas} before spatial operations are performed.
#'
#' @examples
#' \dontrun{
#' result <- assess_representation_regional(
#'   data = species_data,
#'   areas = MPAs,
#'   indicator_var_name = "species",
#'   areaID = "NAME_E",
#'   regionID = "region",
#'   scoring = "representation: regional relative ranking",
#'   indicator = "Species representation",
#'   type = "Representation",
#'   units = "proportion"
#' )
#' }
#'
#' @export

assess_representation_regional <- function(
    data,
    areas,
    indicator_var_name,
    areaID,
    regionID,
    scoring,
    indicator = NULL,
    type = NULL,
    units = NULL,
    PPTID = NULL,
    project_short_title = NULL,
    climate = NULL,
    design_target = NULL,
    other_nest_variables = NULL
) {

  # ---------------------------------------------------------
  # 1. Validate inputs
  # ---------------------------------------------------------
  if (!inherits(data, "sf")) {
    stop("data must be an sf object for representation scoring")
  }

  if (!inherits(areas, "sf")) {
    stop("areas must be an sf object")
  }

  indicator_col <- {{indicator_var_name}}
  area_col <- {{areaID}}
  region_col <-{{regionID}}

  if (!indicator_col %in% names(data)) {
    stop("indicator_var_name column not found in data")
  }

  if (!area_col %in% names(areas)) {
    stop("areaID column not found in areas")
  }

  if (!region_col %in% names(areas)) {
    stop("regionID column not found in areas")
  }

  if (scoring != "representation: regional relative ranking") {
    stop("scoring must be 'representation: regional relative ranking'")
  }
  data <- sf::st_transform(data, sf::st_crs(areas))

  # ---------------------------------------------------------
  # 2. Identify geometry type
  # ---------------------------------------------------------

  geometry_types <- unique(as.character(sf::st_geometry_type(data)))

  is_polygon <- all(geometry_types %in% c("POLYGON", "MULTIPOLYGON"))

  is_point <- all(geometry_types %in% c("POINT", "MULTIPOINT"))

  if (!is_polygon && !is_point) {
    stop("data must contain either polygon/multipolygon or point/multipoint geometries")
  }

  # ---------------------------------------------------------
  # 3. Prepare data
  # ---------------------------------------------------------

  data <- data |>
    sf::st_make_valid() |>
    dplyr::filter(
      !is.na(.data[[indicator_col]])
    )

  areas <- areas |>
    sf::st_make_valid()

  # Columns retained in nested data
  geometry_col <- attr(data, "sf_column")

  if (!(is.na(other_nest_variables))) {
    nest_cols <- unique(c(
      indicator_col,
      geometry_col,
      other_nest_variables
    ))
  } else {
    nest_cols <- unique(c(
      indicator_col,
      geometry_col
    ))

  }



  # ---------------------------------------------------------
  # 4. Calculate representation
  # ---------------------------------------------------------

  if (is_polygon) {
    # -------------------------------------------------------
    # Polygon representation
    # -------------------------------------------------------
    # Total area of each feature in the source dataset
    feature_area <- sf::st_area(data) |>
      units::set_units("km^2") |>
      as.numeric()

    data_with_area <- data |>
      dplyr::mutate(
        .feature_area_km2 = feature_area
      )

    # Area of each conservation area
    area_size <- areas |>
      dplyr::mutate(
        .area_size_km2 = sf::st_area(geometry) |>
          units::set_units("km^2") |>
          as.numeric()
      ) |>
      sf::st_drop_geometry() |>
      dplyr::select(
        dplyr::all_of(area_col),
        .area_size_km2
      )

    # Intersect features with conservation areas
    represented <- sf::st_intersection(
      data_with_area,
      areas
    )

    if (nrow(represented) > 0) {
      represented <- represented |>
        dplyr::mutate(
          .represented_area_km2 = sf::st_area(geometry) |>
            units::set_units("km^2") |>
            as.numeric()
        ) |>
        dplyr::select(
          dplyr::all_of(area_col),
          dplyr::all_of(nest_cols),
          .feature_area_km2,
          .represented_area_km2
        ) |>
        dplyr::mutate(
          representation =
            .represented_area_km2 /
            .feature_area_km2
        )

    } else {
      represented <- data.frame()
    }
  } else {
    # -------------------------------------------------------
    # Point / multipoint representation
    # -------------------------------------------------------
    # Give every source observation an ID
    data_with_id <- data |>
      dplyr::mutate(
        .observation_id = dplyr::row_number()
      )

    # Total number of observations for each feature
    total_occurrences <- data_with_id |>
      sf::st_drop_geometry() |>
      dplyr::count(
        .data[[indicator_col]],
        name = ".total_occurrences"
      )

    # Intersect observations with conservation areas
    represented <- sf::st_intersection(
      data_with_id,
      areas
    )
    if (nrow(represented) > 0) {
      geometry_col <- attr(data, "sf_column")
      represented <- represented |>
        dplyr::left_join(total_occurrences, by = indicator_col) |>
        dplyr::select(
          dplyr::all_of(c(area_col, nest_cols, ".total_occurrences", ".observation_id"))
        ) |>
        dplyr::group_by(
          .data[[area_col]],
          .data[[indicator_col]],
          .data$.total_occurrences
        ) |>
        dplyr::summarise(
          .occurrences = dplyr::n_distinct(.data$.observation_id),
          !!geometry_col := sf::st_union(.data[[geometry_col]]),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          representation = .occurrences / .total_occurrences
        )

    } else {

      represented <- data.frame()
    }
  }

  # ---------------------------------------------------------
  # 5. Create one row per conservation area
  # ---------------------------------------------------------
  all_areas <- areas |>
    sf::st_drop_geometry() |>
    dplyr::select(
      dplyr::all_of(area_col),
      dplyr::all_of(region_col)
    ) |>
    dplyr::distinct()

  if (nrow(represented) > 0) {
    representation_by_area <- represented |>
      sf::st_drop_geometry() |>
      dplyr::group_by(.data[[area_col]]) |>
      dplyr::summarise(
        representation = mean(representation, na.rm = TRUE),
        .groups = "drop"
      )
    nesteddata <- represented |>
      dplyr::select(-dplyr::any_of("representation")) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(area_col))) |>
      tidyr::nest(data = dplyr::all_of(nest_cols)) |>
      dplyr::ungroup() |>
      dplyr::left_join(representation_by_area, by = area_col)

  } else {
    nesteddata <- tibble::tibble(
      !!area_col := character(),
      data = list(),
      representation = numeric()
    )
  }

  # ---------------------------------------------------------
  # 6. Add areas with no representation
  # ---------------------------------------------------------

  nesteddata <- all_areas |>
    dplyr::left_join(nesteddata, by = area_col) |>
    dplyr::mutate(
      representation = dplyr::coalesce(representation, 0)
    )

  # ---------------------------------------------------------
  # 7. Calculate regional relative ranking
  # ---------------------------------------------------------
  nesteddata <- nesteddata |>
    dplyr::group_by(dplyr::across(dplyr::all_of(region_col))) |>
    dplyr::mutate(
      score = if (dplyr::n() == 1) 100 else dplyr::cume_dist(representation) * 100
    ) |>
    dplyr::ungroup()

  # ---------------------------------------------------------
  # 8. Status and trend statements
  # ---------------------------------------------------------

  nesteddata <- nesteddata |>
    dplyr::mutate(
      indicator = indicator,
      type = type,
      units = units,
      scoring = scoring,
      PPTID = paste0(
        PPTID,
        collapse = " ;;; "
      ),
      project_short_title = paste0(
        project_short_title,
        collapse = " ;;; "
      ),
      climate = climate,
      design_target = design_target,

      status_statement = dplyr::if_else(
        representation == 0,
        "No features are represented.",
        paste0(
          .data[[area_col]],
          " represents ",
          round(representation * 100, 1),
          "% of the features represented in the dataset."
        )
      ),

      trend_statement =
        "There is no temporal dimension in this data."
    )

  # ---------------------------------------------------------
  # 9. Remove region and intermediate representation
  # ---------------------------------------------------------

  nesteddata |>
    dplyr::select(
      -dplyr::all_of(region_col),
      -representation
    )
}
