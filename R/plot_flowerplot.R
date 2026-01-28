#' Create a weighted indicator flower plot
#'
#' Generates a radial "flower plot" visualization summarizing
#' indicator scores using weighted segments and letter-grade
#' classification. Individual indicators are displayed as petals,
#' grouped into higher-level categories, with both indicator-level
#' and group-level scores encoded by colour and radial extent.
#'
#' This plot is designed to communicate overall status and
#' composition of indicators within a site, network, or region.
#'
#' @param df
#' A data frame containing indicator scores and weights. Must
#' include columns referenced by \code{grouping}, \code{labels},
#' \code{score}, \code{weight}, and optionally \code{area_name}.
#'
#' @param grouping
#' Character string giving the column name used to group indicators
#' (e.g. indicator category or theme).
#'
#' @param labels
#' Character string giving the column name used as indicator labels.
#'
#' @param score
#' Character string giving the column name containing numeric
#' indicator scores.
#'
#' @param weight
#' Character string giving the column name containing indicator
#' weights. Weights are normalized internally to sum to 1.
#'
#' @param title
#' Plot title. Defaults to the unique value of
#' \code{df[["area_name"]]}. Set to \code{FALSE} to suppress the
#' title.
#'
#' @param max_score
#' Numeric maximum value of the scoring scale.
#'
#' @param min_score
#' Numeric minimum value of the scoring scale.
#'
#' @param bintextsize
#' Numeric text size used for group labels in the outer ring.
#'
#' @param zeroline
#' Logical; if \code{TRUE}, adds a horizontal reference line at
#' zero.
#'
#' @details
#' Indicator-level scores are aggregated using weighted means and
#' converted to letter grades via \code{calc_letter_grade()}.
#' Group-level scores are computed by aggregating across indicators
#' within each \code{grouping} category.
#'
#' The flower plot uses polar coordinates where:
#' \itemize{
#'   \item Petal width represents indicator weight.
#'   \item Radial height represents indicator score.
#'   \item Colour represents letter-grade classification.
#' }
#'
#' Indicator labels are automatically wrapped for readability,
#' and missing scores are visually muted.
#'
#' @return
#' A \code{ggplot} object representing the flower plot.
#' @importFrom ggplot2 ggplot aes geom_crossbar geom_errorbar
#'   geom_text coord_polar scale_x_continuous scale_y_continuous
#'   scale_fill_manual scale_color_manual labs theme
#' @importFrom dplyr arrange mutate if_else
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#' \dontrun{
#' # Create a flower plot summarizing indicator status
#' plot_flowerplot(
#'   df = indicator_scores,
#'   grouping = "theme",
#'   labels = "indicator_name",
#'   score = "score",
#'   weight = "weight",
#'   title = "Example MPA",
#'   max_score = 100,
#'   min_score = 0
#' )
#' }
#'
#' @export
plot_flowerplot <- function(df,grouping="grouping",labels="labels",score="score",weight="weight",title=unique(df[["area_name"]]),max_score = 100,min_score = 0,bintextsize=3,zeroline=FALSE){
  scalerange <- max_score-min_score

  grades <- c("A", "B", "C", "D", "F")
  flowerPalette <- rev(colorRampPalette(brewer.pal(5,"RdYlBu"))(length(grades)))

  names(flowerPalette) <- grades

  ngroups <- length(unique(df[[grouping]]))

  df <- arrange(df,!!sym(grouping),!!sym(labels))
  rawdata <- data.frame(grouping=factor(df[[grouping]],levels = unique(df[[grouping]])),
                     labels=factor(df[[labels]],levels = unique(df[[labels]])),
                     score=df[[score]],
                     weight=df[[weight]]) |>
    mutate(weight=weight/sum(weight))
  data <- calc_group_score(df = rawdata,
                     grouping_var = "labels",
                     score_var = "score",
                     weight_var = "weight") |>
    mutate(pos=cumsum(weight)-weight/2,
           bg=dplyr::if_else(is.nan(score),"#EDEDED","white"))
  data$score[which(is.nan(data$score))] <- NA

  # browser()

  grouped_df <- calc_group_score(df = rawdata,
                                 grouping_var = c("labels","grouping"),
                                 score_var = "score",
                                 weight_var = "weight") |>
    mutate(y=max_score+scalerange*.5,
           x=cumsum(weight)-weight/2,
           angle=360-x*360,
           angle=dplyr::if_else(angle>90&angle<270,
                         angle-180,
                         angle)
    )

  wrap_label <- function(text) {
    # Convert to character if it's a factor
    text <- as.character(text)

    words <- strsplit(text, " ")[[1]]
    n_words <- length(words)

    # If only 1 word, no wrapping needed
    if (n_words == 1) {
      return(text)
    }

    # If 2 words, always put on separate lines
    if (n_words == 2) {
      return(paste(words, collapse = "\n"))
    }

    # Identify small words (<=4 chars)
    word_lengths <- nchar(words)
    is_small <- word_lengths <= 4

    # Special case: if first word is small, treat it as long
    if (is_small[1]) {
      is_small[1] <- FALSE
    }

    # Strategy: Long words stand alone; small words cluster together
    lines <- character()
    i <- 1

    while (i <= n_words) {
      if (!is_small[i]) {
        # Long word - put it on its own line
        lines <- c(lines, words[i])
        i <- i + 1
      } else {
        # Small word - gather consecutive small words
        small_cluster <- character()
        while (i <= n_words && is_small[i]) {
          small_cluster <- c(small_cluster, words[i])
          i <- i + 1
        }
        lines <- c(lines, paste(small_cluster, collapse = " "))
      }
    }

    return(paste(lines, collapse = "\n"))
  }

  p <- ggplot(data=data,aes(width = weight))+
    geom_crossbar(aes(x = pos, y = max_score-scalerange*1.5, ymax = max_score-scalerange*1.5, ymin = min_score,fill=calc_letter_grade(weighted.mean(score,weight,na.rm = TRUE))),color="transparent")+
    ggplot2::geom_crossbar(stat="identity",linewidth=0.2,color='lightgrey',aes(x=pos,y=max_score,ymax=max_score,ymin=min_score),fill=data$bg)+
    ggplot2::geom_crossbar(stat="identity",linewidth=0.2,color='black',aes(x=pos,y=score,ymax=score,ymin=min_score,fill=calc_letter_grade(score)))+
    ggplot2::coord_polar()+
    theme(panel.grid.major = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          axis.line  = ggplot2::element_blank(),
          axis.text  = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          legend.position="none")+
    scale_x_continuous(labels = data$labels,
                       breaks = data$pos)+
    scale_y_continuous(limits = c(max_score-scalerange*1.5,max_score+scalerange*.55))+ #PROBLEM HERE
    scale_fill_manual(values=flowerPalette)+
    scale_color_manual(values=flowerPalette)+
    ggplot2::geom_errorbar(data=grouped_df,
                           inherit.aes=FALSE,
                           aes(x=x,
                               ymin=y,
                               ymax=y,
                               width=weight-0.01,
                               col = calc_letter_grade(score)),
                           linewidth = 7)+
    ggplot2::geom_text(aes(label=calc_letter_grade(weighted.mean(score,weight,na.rm = TRUE))),
                       x=min_score,
                       y=max_score-scalerange*1.5,
                       size=8)+
    ggplot2::geom_text(aes(label=sapply(labels, wrap_label),
                           x=pos,
                           y=max_score+scalerange*.2),
                       size=2)+

    ggplot2::geom_text(data=grouped_df,
                       inherit.aes = FALSE,
                       aes(label=grouping,
                           x=x,
                           y=y,
                           angle=angle),
                       size=bintextsize)
  if(zeroline){
    p <- p + ggplot2::geom_hline(yintercept = 0,linetype="dotted")

  }



  if(title==FALSE){
    return(p)
  } else {
    return(p+
             labs(title=title) +
             theme(plot.title = element_text(hjust = 0.5)))
  }

}
