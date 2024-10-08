#' Plot a flowerplot
#'
#' @param df data.frame with indicator scores, grouping, labels, and relative weight
#' @param grouping character string for the name of the grouping column in `df`
#' @param labels character string for the name of the labels column in `df`
#' @param score character string for the name of the score column in `df`
#' @param max_score numeric value for the maximum possible value of the scale (i.e. petal length). Default is 100
#' @param min_score numeric value for the minimum possible value of the scale (i.e. petal length). Default is 0
#' @param weight character string for the name of the weight column in `df`
#' @param title Defaults to the unique value of the `area_name` column of the `df`, but can take any character value. Alternatively, use `FALSE` to avoid having a title.
#'
#' @return plot
#' @export
#'
#' @examples
#'
#' indicatorbins <- data.frame(grouping=rep(c("Biodiversity",
#'                                            "Habitat",
#'                                            "Productivity"),
#'                                          times=c(3,5,3)),
#'                             labels=c("Genetic Diversity",
#'                                      "Species Diversity",
#'                                      "Functional Diversity",
#'
#'                                      "Environmental Representativity",
#'                                      "Key Fish Habitat",
#'                                      "Connectivity",
#'                                      "Uniqueness",
#'                                      "Threats to Habitat",
#'
#'                                      "Biomass Metrics",
#'                                      "Structure and Function",
#'                                      "Threats to Productivity"),
#'                             score=runif(11,55,100),
#'                             weight=1,
#'                             area_name = "Random Example MPA")
#'
#' plot_flowerplot(indicatorbins)
#'
#'
plot_flowerplot <- function(df,grouping="grouping",labels="labels",score="score",weight="weight",title=unique(df[["area_name"]]),max_score = 100,min_score = 0,bintextsize=3,zeroline=FALSE){
  # browser()
  scalerange <- max_score-min_score

  calc_letter_grade <- function(percent){
    cutoffs=c(min_score, seq(max_score-scalerange*.4, max_score, by = 10/3/100*scalerange))
    grades=c("F", paste0(toupper(rep(letters[4:1], each = 3)), rep(c("-","","+"),4)))
    cut(percent,cutoffs,grades)
  }

  ngroups <- length(unique(df[[grouping]]))
  data <- data.frame(grouping=factor(df[[grouping]],levels = unique(df[[grouping]])),
                     labels=factor(df[[labels]],levels = unique(df[[labels]])),
                     score=df[[score]],
                     weight=df[[weight]]) |>
    group_by(grouping,labels) |>
    reframe(score = weighted.mean(score,weight,na.rm = TRUE),
            weight = sum(weight)) |>
    mutate(weight=weight/sum(weight),
           pos=cumsum(weight)-weight/2,
           bg=if_else(is.nan(score),"grey93","white"))

  grouped_df <- data |>
    group_by(grouping) |>
    reframe(y=max_score+scalerange*.5,
            n=n(),
            weight=sum(weight)) |>
    mutate(x=cumsum(weight)-weight/2,
           angle=360-x*360,
           angle=if_else(angle>90&angle<270,
                         angle-180,
                         angle)
    )

  p <- ggplot(data=data,aes(width = weight))+
    geom_crossbar(stat="identity",linewidth=0.2,color='lightgrey',aes(x=pos,y=max_score,ymax=max_score,ymin=min_score),fill=data$bg)+
    geom_crossbar(stat="identity",linewidth=0.2,color='black',aes(x=pos,y=score,ymax=score,ymin=min_score,fill=calc_letter_grade(score)))+
    coord_polar()+
    theme(panel.grid.major = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          axis.line  = ggplot2::element_blank(),
          axis.text  = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          legend.position="none")+
    scale_x_continuous(labels = data$labels,
                       breaks = data$pos)+
    scale_y_continuous(limits = c(max_score-scalerange*1.5,max_score+scalerange*.55))+
    scale_fill_brewer(palette = "RdBu")+
    geom_text(aes(label=calc_letter_grade(weighted.mean(score,weight,na.rm = TRUE))),
              x=min_score,
              y=max_score-scalerange*1.5,
              size=8)+
    geom_text(aes(label=gsub(" ","\n",labels),
                  x=pos,
                  y=max_score+scalerange*.2),
              size=2)+
    geom_errorbar(data=grouped_df,
                  inherit.aes=FALSE,
                  aes(x=x,
                      ymin=0.95*y,
                      ymax=0.95*y,
                      width=weight-0.01))+
    geom_text(data=grouped_df,
              inherit.aes = FALSE,
              aes(label=grouping,
                  x=x,
                  y=y,
                  angle=angle),
              size=bintextsize)
  if(zeroline){
    p <- p + geom_hline(yintercept = 0,linetype="dotted")

  }



  if(title==FALSE){
    return(p)
  } else {
    return(p+
             labs(title=title) +
             theme(plot.title = element_text(hjust = 0.5)))
  }

}
