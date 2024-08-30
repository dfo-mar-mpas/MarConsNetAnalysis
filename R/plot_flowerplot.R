#' Plot a flowerplot
#'
#' @param df data.frame with indicator scores, grouping, labels, and relative weight
#' @param grouping character string for the name of the grouping column in `df`
#' @param labels character string for the name of the labels column in `df`
#' @param score character string for the name of the score column in `df`
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
plot_flowerplot <- function(df,grouping="grouping",labels="labels",score="score",weight="weight",title=unique(df[["area_name"]])){
  # browser()

  calc_letter_grade <- function(percent){
    cutoffs=c(0, seq(60, 100, by = 10/3))
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
    reframe(y=150,
            n=n(),
            weight=sum(weight)) |>
    mutate(x=cumsum(weight)-weight/2,
           angle=360-x*360,
           angle=if_else(angle>90&angle<270,
                         angle-180,
                         angle)
    )

  p <- ggplot(data=data,aes(width = weight))+
    geom_bar(stat="identity",linewidth=0.2,color='lightgrey',aes(x=pos,y=100),fill=data$bg)+
    geom_bar(stat="identity",linewidth=0.2,color='black',aes(x=pos,y=score,fill=calc_letter_grade(score)))+
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
    scale_y_continuous(limits = c(-50,155))+
    scale_fill_brewer(palette = "RdBu")+
    geom_text(aes(label=calc_letter_grade(weighted.mean(score,weight,na.rm = TRUE))),
              x=0,
              y=-50,
              size=8)+
    geom_text(aes(label=gsub(" ","\n",labels),
                  x=pos,
                  y=120),
              size=2)+
    geom_errorbar(data=grouped_df,
                  inherit.aes=FALSE,
                  aes(x=x,
                      ymin=rep(145,ngroups),
                      ymax=rep(145,ngroups),
                      width=weight-0.01))+
    geom_text(data=grouped_df,
              inherit.aes = FALSE,
              aes(label=grouping,
                  x=x,
                  y=y,
                  angle=angle),
              size=3)
  if(title==FALSE){
    return(p)
  } else {
    return(p+
             labs(title=title) +
             theme(plot.title = element_text(hjust = 0.5)))
  }

}
