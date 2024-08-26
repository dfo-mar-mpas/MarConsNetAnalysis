#' Plot a flowerplot
#'
#' @param df
#' @param grouping
#' @param labels
#' @param score
#' @param weight
#'
#' @return
#' @export
#'
#' @examples
plot_flowerplot <- function(df,grouping="grouping",labels="labels",score="score",weight="weight"){
  # browser()

  calc_letter_grade <- function(percent){
    cutoffs=c(0, seq(60, 100, by = 10/3))
    letters=c("F", paste0(toupper(rep(letters[4:1], each = 3)), rep(c("-","","+"),4)))
    cut(percent,cutoffs,letters)
  }

  ngroups <- length(unique(df[[grouping]]))
  data <- data.frame(grouping=factor(df[[grouping]],levels = unique(df[[grouping]])),
                     labels=factor(df[[labels]],levels = unique(df[[labels]])),
                     score=df[[score]],
                     weight=df[[weight]]) |>
    group_by(grouping,labels) |>
    reframe(score = weighted.mean(score,weight),
            weight = sum(weight)) |>
    mutate(weight=weight/sum(weight),
           pos=cumsum(weight)-weight/2)

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

  ggplot(data=data,aes(width = weight))+
    geom_bar(stat="identity",linewidth=0.2,color='lightgrey',aes(x=pos,y=100),fill="white")+
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
    geom_text(aes(label=calc_letter_grade(weighted.mean(score,weight))),
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
}
