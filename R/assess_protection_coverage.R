#' Assess protection coverage
#'
#' Calculates the proportion of an indicator feature that occurs within
#' conservation areas and converts the resulting protection coverage into a
#' score based on predefined minimum and maximum protection targets.
#'
#' Two scoring approaches are supported. `"protection coverage"` calculates
#' coverage based on the proportion of each feature's area that intersects a
#' conservation area. `"protection coverage: weighted"` additionally weights
#' coverage according to a variable supplied in `other_nest_variables`.
#'
#' Scores are based on the relationship between observed protection coverage
#' and the indicator's `min_target` and `max_target`. Coverage between the two
#' targets receives a score of 100. Coverage below the minimum target receives
#' a proportional score, while coverage above the maximum target is penalized
#' according to the amount by which it exceeds the maximum target.
#'
#' @param data An `sf` object containing the spatial features to be assessed.
#'   The data must contain the column specified by `indicator_var_name`, as
#'   well as `min_target`, `max_target`, and `plainname` columns.
#' @param areas An `sf` object containing the conservation area boundaries.
#' @param indicator_var_name Character string identifying the column containing
#'   the indicator or feature being assessed.
#' @param areaID Character string identifying the column in `areas` that
#'   identifies each conservation area.
#' @param scoring Character string specifying the protection coverage scoring
#'   method. Must be either `"protection coverage"` or
#'   `"protection coverage: weighted"`.
#' @param indicator Optional character string identifying the indicator.
#' @param type Optional character string describing the indicator type.
#' @param units Optional character string specifying the units of the
#'   indicator.
#' @param PPTID Optional identifier for the associated PPT. Multiple values
#'   are collapsed using `" ;;; "`.
#' @param project_short_title Optional short title of the project. Multiple
#'   values are collapsed using `" ;;; "`.
#' @param climate Optional character string describing the climate context of
#'   the indicator.
#' @param design_target Optional design target associated with the indicator.
#' @param other_nest_variables Optional character vector containing additional
#'   variables. For `"protection coverage: weighted"`, the first variable is
#'   used as the weighting variable.
#'
#' @return A data frame containing one row per conservation area, with the
#'   following columns:
#'   \describe{
#'     \item{areaID}{Identifier for the conservation area.}
#'     \item{data}{Nested spatial data representing the indicator features
#'       intersecting the conservation area.}
#'     \item{coverage}{Proportion of the indicator protected within the
#'       conservation area.}
#'     \item{score}{Protection coverage score from 0 to 100.}
#'     \item{indicator}{Indicator associated with the assessment.}
#'     \item{type}{Indicator type.}
#'     \item{units}{Indicator units.}
#'     \item{scoring}{Protection coverage scoring method used.}
#'     \item{PPTID}{Associated PPT identifier.}
#'     \item{project_short_title}{Project short title.}
#'     \item{climate}{Climate context associated with the indicator.}
#'     \item{design_target}{Design target associated with the indicator.}
#'     \item{status_statement}{Statement describing the observed protection
#'       coverage and established protection target.}
#'     \item{trend_statement}{Statement indicating that the assessment has no
#'       temporal dimension.}
#'   }
#'
#' @details
#' The input geometries are transformed to the coordinate reference system of
#' `areas` and made valid before spatial intersection. Features with missing
#' indicator values are excluded.
#'
#' For the standard `"protection coverage"` method, the area of each input
#' feature is calculated before intersection with conservation areas. The
#' proportion of each feature occurring within a conservation area is then
#' calculated as:
#'
#' \deqn{
#' coverage = \frac{covered\ area}{total\ feature\ area}
#' }
#'
#' For `"protection coverage: weighted"`, the first variable specified in
#' `other_nest_variables` is used as a weight. The contribution of each
#' feature to protection coverage is calculated from its weight and the
#' proportion of its area that is protected.
#'
#' Conservation areas with no intersecting indicator features are retained in
#' the output and assigned a coverage of zero.
#'
#' The protection coverage score is calculated using the minimum and maximum
#' protection targets:
#'
#' \itemize{
#'   \item{Below `min_target`: the score increases proportionally with
#'   coverage.}
#'   \item{Between `min_target` and `max_target`: the score is 100.}
#'   \item{Above `max_target`: the score decreases as coverage exceeds the
#'   maximum target.}
#' }
#'
#' The function assumes that `min_target` and `max_target` are proportions
#' between 0 and 1.
#'
#' @examples
#' \dontrun{
#' # Standard protection coverage
#' result <- assess_protection_coverage(
#'   data = habitat_data,
#'   areas = MPAs,
#'   indicator_var_name = "habitat_type",
#'   areaID = "NAME_E",
#'   scoring = "protection coverage",
#'   indicator = "Habitat protection",
#'   type = "habitat",
#'   units = "proportion"
#' )
#'
#' # Weighted protection coverage
#' result <- assess_protection_coverage(
#'   data = habitat_data,
#'   areas = MPAs,
#'   indicator_var_name = "habitat_type",
#'   areaID = "NAME_E",
#'   scoring = "protection coverage: weighted",
#'   other_nest_variables = "habitat_weight",
#'   indicator = "Weighted habitat protection",
#'   type = "habitat",
#'   units = "proportion"
#' )
#' }
#'
#' @export


assess_protection_coverage <- function(
    data,
    areas,
    indicator_var_name,
    areaID,
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
    stop("data must be an sf object for protection coverage scoring")
  }

  if (!inherits(areas, "sf")) {
    stop("areas must be an sf object")
  }

  indicator_col <- {{ indicator_var_name }}
  area_col <- {{ areaID }}

  if (!indicator_col %in% names(data)) {
    stop("indicator_var_name column not found in data")
  }

  if (!area_col %in% names(areas)) {
    stop("areaID column not found in areas")
  }

  if (!scoring %in% c("protection coverage","protection coverage: weighted")) {
    stop("scoring must be 'protection coverage' or ","'protection coverage: weighted'")
  }

  # Transform data to areas CRS
  data <- sf::st_transform(data, sf::st_crs(areas))

  # Make geometries valid
  data <- data |>
    sf::st_make_valid() |>
    dplyr::filter(!is.na(.data[[indicator_col]]))

  areas <- sf::st_make_valid(areas)

  geometry_col <- attr(data, "sf_column")

  # ---------------------------------------------------------
  # 2. Identify weighted variable
  # ---------------------------------------------------------

  if (scoring == "protection coverage: weighted") {

    if (is.null(other_nest_variables) ||
        length(other_nest_variables) < 1) {
      stop("protection coverage: weighted requires a weighting ","variable in other_nest_variables")
    }

    weight_col <- other_nest_variables[1]

    if (!weight_col %in% names(data)) {
      stop( "Weighting variable '", weight_col,"' not found in data")
    }
  }

  # ---------------------------------------------------------
  # 3. Calculate coverage
  # ---------------------------------------------------------

  if (scoring == "protection coverage") {
    # Total area of each feature
    data_with_area <- data |>
      dplyr::mutate(
        .feature_area = sf::st_area(.data[[geometry_col]])
      )

    # Intersect features with conservation areas
    represented <- sf::st_intersection(
      data_with_area,
      areas
    )

    if (nrow(represented) > 0) {

      represented <- represented |>
        dplyr::mutate(
          .covered_area = sf::st_area(.data[[geometry_col]])
        ) |>
        dplyr::group_by(
          .data[[area_col]],
          .data[[indicator_col]]
        ) |>
        dplyr::summarise(
          .covered_area = sum(.covered_area),
          .feature_area = dplyr::first(.feature_area),
          !!geometry_col := sf::st_union(.data[[geometry_col]]),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          coverage = as.numeric(
            .covered_area / .feature_area
          )
        )

    } else {
      represented <- tibble::tibble()
    }
  } else {
    # -------------------------------------------------------
    # Weighted coverage
    # -------------------------------------------------------

    data_with_weight <- data |>
      dplyr::mutate(
        .weight = .data[[weight_col]],
        .feature_area = sf::st_area(.data[[geometry_col]])
      )

    represented <- sf::st_intersection(
      data_with_weight,
      areas
    )

    if (nrow(represented) > 0) {
      represented <- represented |>
        dplyr::mutate(
          .intersection_area =
            sf::st_area(.data[[geometry_col]]),
          .weighted_coverage =
            .weight *
            as.numeric(.intersection_area / .feature_area)
        ) |>
        dplyr::group_by(
          .data[[area_col]],
          .data[[indicator_col]]
        ) |>
        dplyr::summarise(
          .weighted_coverage = sum(.weighted_coverage, na.rm = TRUE),
          .total_weight = dplyr::first(
            sum(.weight, na.rm = TRUE)
          ),
          !!geometry_col := sf::st_union(.data[[geometry_col]]),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          coverage =
            .weighted_coverage / .total_weight
        )
    } else {
      represented <- tibble::tibble()
    }
  }

  # ---------------------------------------------------------
  # 4. Calculate coverage by conservation area
  # ---------------------------------------------------------

  if (nrow(represented) > 0) {

    coverage_by_area <- represented |>
      sf::st_drop_geometry() |>
      dplyr::group_by(
        .data[[area_col]]
      ) |>
      dplyr::summarise(
        coverage = sum(coverage, na.rm = TRUE),
        .groups = "drop"
      )

    nesteddata <- represented |>
      dplyr::group_by(.data[[area_col]]) |>
      tidyr::nest(data = -dplyr::all_of(area_col)) |>
      dplyr::ungroup() |>
      dplyr::left_join(coverage_by_area, by = area_col)

  } else {
    nesteddata <- tibble::tibble(
      !!area_col := character(),
      data = list(),
      coverage = numeric()
    )
  }

  # ---------------------------------------------------------
  # 5. Add conservation areas with no coverage
  # ---------------------------------------------------------

  all_areas <- areas |>
    sf::st_drop_geometry() |>
    dplyr::select(
      dplyr::all_of(area_col)
    ) |>
    dplyr::distinct()

  nesteddata <- all_areas |>
    dplyr::left_join(
      nesteddata,
      by = area_col
    ) |>
    dplyr::mutate(
      coverage = dplyr::coalesce(coverage, 0)
    )

  # ---------------------------------------------------------
  # 6. Score against protection target
  # ---------------------------------------------------------

  if (!"min_target" %in% names(data) ||!"max_target" %in% names(data)) {
    stop(
      "data must contain 'min_target' and 'max_target' ",
      "for protection coverage scoring"
    )
  }

  min_target <- unique(data$min_target)[1]
  max_target <- unique(data$max_target)[1]

  nesteddata <- nesteddata |>
    dplyr::mutate(
      score = dplyr::case_when(
        coverage < min_target ~
          coverage / min_target * 100,

        coverage > max_target ~
          100 -
          (coverage - max_target) /
          (1 - max_target) * 100,

        TRUE ~ 100
      )
    )

  # ---------------------------------------------------------
  # 7. Status and trend statements
  # ---------------------------------------------------------

  nesteddata <- nesteddata |>
    dplyr::mutate(
      indicator = indicator,
      type = type,
      units = units,
      scoring = scoring,
      PPTID = paste0(PPTID, collapse = " ;;; "),
      project_short_title =
        paste0(project_short_title, collapse = " ;;; "),
      climate = climate,
      design_target = design_target,

      status_statement = paste0(
        .data[[area_col]],
        " has ",
        round(coverage * 100, 1),
        "% protection coverage of ",
        unique(data$plainname)[1],
        ". The established protection target is ",
        round(min_target * 100, 1),
        "% to ",
        round(max_target * 100, 1),
        "%."
      ),

      trend_statement =
        "There is no temporal dimension in this data."
    )

  # ---------------------------------------------------------
  # 8. Return
  # ---------------------------------------------------------

  nesteddata
}
