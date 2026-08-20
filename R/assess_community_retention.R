#' Assess community retention
#'
#' Calculates a community retention score by comparing the number of species
#' detected in the most recent sampling period with the historically expected
#' species richness. Historical expected richness is estimated using the Chao1
#' estimator to account for species that may have been present but not
#' detected.
#'
#' The function assigns samples to conservation areas, separates observations
#' into historical and most recent sampling periods, and constructs a
#' species-by-sample community matrix from detection data. A score from 0 to
#' 100 is calculated as the proportion of historically expected species
#' richness represented in the most recent sampling period, with scores capped
#' at 100.
#'
#' @param data An `sf` object or data frame containing species detection data.
#'   Required columns are `ID`, `year_of_data_collection`, `species`, and
#'   `detections`. If `data` is not already an `sf` object, `latitude` and
#'   `longitude` columns are required so that the observations can be
#'   converted to an `sf` object.
#' @param areas An `sf` object containing the conservation area boundaries.
#' @param areas_use An `sf` object containing the conservation area geometries
#'   to use when spatially joining observations to areas.
#' @param areaID Column name identifying the conservation area. This is used
#'   to assign observations to areas and is evaluated using tidy evaluation.
#' @param scoring Character string specifying the scoring method. Must be
#'   exactly `"community retention"`.
#' @param crs Coordinate reference system used when converting point data to
#'   an `sf` object.
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
#'
#' @return A data frame containing one row per conservation area, with the
#'   following additional columns:
#'   \describe{
#'     \item{score}{Community retention score from 0 to 100.}
#'     \item{status_statement}{Statement describing recent observed richness
#'       relative to historically expected richness.}
#'     \item{trend_statement}{Statement describing species retained,
#'       historically detected species not recently detected, and newly
#'       detected species.}
#'     \item{quality_statement}{Statement describing the number of recent and
#'       historical samples used in the assessment.}
#'   }
#'
#' @details
#' The function first checks that the scoring method is
#' `"community retention"` and that the required data columns are present.
#' Point data that are not already spatial are converted to `sf` using
#' latitude and longitude coordinates.
#'
#' Observations are spatially joined to the supplied conservation areas and
#' grouped by area. For each area, the most recent sampling year is identified
#' and compared with all previous sampling years.
#'
#' Detection data are converted into a community matrix, with samples as rows
#' and species as columns. Detections are summed across samples separately for
#' the historical and most recent periods. Historical detections are used with
#' `vegan::estimateR()` to estimate expected species richness using the Chao1
#' estimator.
#'
#' The community retention score is calculated as:
#'
#' \deqn{
#'   score = \min\left(\frac{observed\ recent\ richness}
#'   {historical\ expected\ richness} \times 100,\ 100\right)
#' }
#'
#' A score of 100 indicates that observed recent richness is equal to or
#' greater than the estimated historical expected richness. Lower scores
#' indicate that fewer species were detected recently relative to the
#' historical expectation.
#'
#' At least two sampling years are required. If insufficient historical
#' detections are available to estimate expected richness, the score is
#' returned as `NA` and explanatory status, trend, and quality statements are
#' provided.
#'
#' If a `common_name` column is available, common names are included alongside
#' scientific names when listing species that were not recently detected or
#' were newly detected.
#'
#' @seealso
#' [vegan::estimateR()]
#'
#' @examples
#' \dontrun{
#' result <- assess_community_retention(
#'   data = detection_data,
#'   areas = MPAs,
#'   areas_use = MPAs,
#'   areaID = NAME_E,
#'   scoring = "community retention",
#'   crs = 4326,
#'   indicator = "Community composition",
#'   type = "species richness",
#'   units = "species",
#'   PPTID = "PPT001",
#'   project_short_title = "Example project",
#'   climate = NULL,
#'   design_target = NULL
#' )
#' }
#'
#' @export


assess_community_retention <- function(
    data,
    areas,
    areas_use,
    areaID,
    scoring,
    crs,
    indicator = NULL,
    type = NULL,
    units = NULL,
    PPTID = NULL,
    project_short_title = NULL,
    climate = NULL,
    design_target = NULL
) {

  # ---------------------------------------------------------
  # 1. Check scoring scheme
  # ---------------------------------------------------------

  if (scoring != "community retention") {
    stop(
      "This function requires scoring = 'community retention'"
    )
  }

  # ---------------------------------------------------------
  # 2. Check required columns
  # ---------------------------------------------------------

  required_columns <- c(
    "ID",
    "year_of_data_collection",
    "species",
    "detections"
  )

  missing_columns <- setdiff(
    required_columns,
    names(data)
  )

  if (length(missing_columns) > 0) {
    stop(
      "The following required columns are missing: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  # ---------------------------------------------------------
  # 3. Convert point data to sf
  # ---------------------------------------------------------

  if (!inherits(data, "sf")) {

    if (!all(c("latitude", "longitude") %in% names(data))) {
      stop(
        "Community retention requires latitude and longitude columns."
      )
    }

    data <- data |>
      dplyr::filter(
        !is.na(latitude),
        !is.na(longitude)
      ) |>
      sf::st_as_sf(
        coords = c("longitude", "latitude"),
        crs = crs
      )

  }

  # ---------------------------------------------------------
  # 4. Join samples to conservation areas
  # ---------------------------------------------------------

  data <- data |>
    sf::st_join(
      dplyr::select(
        areas_use,
        {{ areaID }}
      ),
      left = FALSE
    ) |>
    dplyr::rename(
      areaID = {{ areaID }}
    )

  # ---------------------------------------------------------
  # 5. Create community results
  # ---------------------------------------------------------

  nesteddata <- data |>
    dplyr::group_by(
      areaID
    ) |>
    tidyr::nest()

  # Add metadata
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
      design_target = design_target
    )

  # ---------------------------------------------------------
  # 6. Preallocate results
  # ---------------------------------------------------------

  n <- nrow(nesteddata)

  score <- rep(NA_real_, n)

  status_statement <- rep(
    NA_character_,
    n
  )

  trend_statement <- rep(
    NA_character_,
    n
  )

  quality_statement <- rep(
    NA_character_,
    n
  )

  # ---------------------------------------------------------
  # 7. Assess each conservation area
  # ---------------------------------------------------------

  for (i in seq_len(n)) {

    DATA <- nesteddata$data[[i]]

    if (is.null(DATA) || nrow(DATA) == 0) {

      status_statement[i] <- "No data available."
      trend_statement[i] <- "No data available."
      quality_statement[i] <- "No data available."

      next
    }

    # -------------------------------------------------------
    # Identify recent year
    # -------------------------------------------------------

    years <- sort(
      unique(
        DATA$year_of_data_collection[
          !is.na(DATA$year_of_data_collection)
        ]
      )
    )

    if (length(years) < 2) {

      score[i] <- NA_real_

      status_statement[i] <-
        "Insufficient sampling years to assess community retention."

      trend_statement[i] <-
        "At least two sampling periods are required to assess community retention."

      quality_statement[i] <- paste0(
        "The assessment included ",
        nrow(DATA),
        " samples from ",
        length(years),
        " sampling year."
      )

      next
    }

    recent_year <- max(years)

    # -------------------------------------------------------
    # 1. Create community matrix
    # -------------------------------------------------------

    community <- DATA |>
      sf::st_drop_geometry() |>
      dplyr::select(
        ID,
        year_of_data_collection,
        species,
        detections
      ) |>
      tidyr::pivot_wider(
        names_from = species,
        values_from = detections,
        values_fill = 0,
        values_fn = sum
      )

    # -------------------------------------------------------
    # 2. Identify historical vs recent samples
    # -------------------------------------------------------

    metadata <- community |>
      dplyr::select(
        ID,
        year_of_data_collection
      )

    community_matrix <- community |>
      dplyr::select(
        -ID,
        -year_of_data_collection
      )

    historical <- metadata$year_of_data_collection < recent_year

    recent <- metadata$year_of_data_collection == recent_year

    historical_matrix <- community_matrix[
      historical,
      ,
      drop = FALSE
    ]

    recent_matrix <- community_matrix[
      recent,
      ,
      drop = FALSE
    ]

    # -------------------------------------------------------
    # 3. Collapse detections across samples
    # -------------------------------------------------------

    historical_total <- colSums(
      historical_matrix,
      na.rm = TRUE
    )

    recent_total <- colSums(
      recent_matrix,
      na.rm = TRUE
    )

    # -------------------------------------------------------
    # 4. Estimate historical expected richness
    # -------------------------------------------------------

    if (
      length(historical_total) == 0 ||
      sum(historical_total) == 0
    ) {

      score[i] <- NA_real_

      status_statement[i] <-
        "Insufficient historical detections to estimate expected community richness."

      trend_statement[i] <-
        "Insufficient historical data to assess community retention."

      quality_statement[i] <- paste0(
        "The assessment included ",
        nrow(recent_matrix),
        " recent samples and ",
        nrow(historical_matrix),
        " historical samples, but there were insufficient historical detections for a richness estimate."
      )

      next
    }

    historical_est <- vegan::estimateR(
      historical_total
    )

    expected_species <- as.numeric(
      historical_est["S.chao1"]
    )

    # -------------------------------------------------------
    # 5. Calculate observed recent richness
    # -------------------------------------------------------

    observed_recent <- sum(
      recent_total > 0
    )

    # -------------------------------------------------------
    # 6. Calculate score
    # -------------------------------------------------------

    score[i] <- min(
      (observed_recent / expected_species) * 100,
      100
    )

    # -------------------------------------------------------
    # 7. Identify species
    # -------------------------------------------------------

    historical_species <- names(
      historical_total[
        historical_total > 0
      ]
    )

    recent_species <- names(
      recent_total[
        recent_total > 0
      ]
    )

    missing_species <- setdiff(
      historical_species,
      recent_species
    )

    new_species <- setdiff(
      recent_species,
      historical_species
    )

    # -------------------------------------------------------
    # 8. Species names
    # -------------------------------------------------------

    if ("common_name" %in% names(DATA)) {

      species_lookup <- DATA |>
        sf::st_drop_geometry() |>
        dplyr::filter(
          !is.na(species)
        ) |>
        dplyr::distinct(
          species,
          common_name
        ) |>
        dplyr::group_by(
          species
        ) |>
        dplyr::summarise(
          common_name = dplyr::first(
            stats::na.omit(common_name)
          ),
          .groups = "drop"
        )

      species_labels <- stats::setNames(
        ifelse(
          is.na(species_lookup$common_name) |
            species_lookup$common_name == "",
          species_lookup$species,
          paste0(
            species_lookup$species,
            " (",
            species_lookup$common_name,
            ")"
          )
        ),
        species_lookup$species
      )

      format_species <- function(x) {

        out <- species_labels[x]

        out[is.na(out)] <- x[is.na(out)]

        out
      }

    } else {

      format_species <- function(x) x

    }

    # -------------------------------------------------------
    # 9. Status statement
    # -------------------------------------------------------

    status_statement[i] <- paste0(
      "In ",
      recent_year,
      ", ",
      observed_recent,
      " species were detected. ",
      "The historical community had an estimated expected richness of approximately ",
      round(expected_species, 0),
      " species based on the Chao1 estimator. ",
      "This represents ",
      round(score[i], 0),
      "% retention of the historically expected community."
    )

    # -------------------------------------------------------
    # 10. Trend statement
    # -------------------------------------------------------

    trend_statement[i] <- paste0(
      "The most recent community contained ",
      observed_recent,
      " detected species compared with ",
      round(expected_species, 0),
      " species expected based on the historical community. "
    )

    if (length(missing_species) > 0) {

      trend_statement[i] <- paste0(
        trend_statement[i],
        length(missing_species),
        " historically detected species were not detected in the most recent sampling period: ",
        paste(
          format_species(missing_species),
          collapse = ", "
        ),
        "."
      )

    } else {

      trend_statement[i] <- paste0(
        trend_statement[i],
        "All historically detected species were also detected in the most recent sampling period."
      )

    }

    if (length(new_species) > 0) {

      trend_statement[i] <- paste0(
        trend_statement[i],
        " ",
        length(new_species),
        " additional species were detected in the most recent sampling period: ",
        paste(
          format_species(new_species),
          collapse = ", "
        ),
        "."
      )
    }

    # -------------------------------------------------------
    # 11. Quality statement
    # -------------------------------------------------------

    quality_statement[i] <- paste0(
      "The assessment was based on ",
      nrow(recent_matrix),
      " samples collected in ",
      recent_year,
      " and ",
      nrow(historical_matrix),
      " historical samples. ",
      "Historical expected richness was estimated using the Chao1 estimator to account for potentially undetected species."
    )
  }

  # ---------------------------------------------------------
  # 12. Return results
  # ---------------------------------------------------------

  nesteddata$score <- score

  nesteddata$status_statement <-
    status_statement

  nesteddata$trend_statement <-
    trend_statement

  nesteddata$quality_statement <-
    quality_statement

  nesteddata
}
