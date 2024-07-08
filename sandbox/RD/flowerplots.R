require(ggplot2)
require(dplyr)

# more flexible version of the OHI flowerplots inspired by https://github.com/OHI-Science/ohicore/blob/master/R/PlotFlower.R#L331

EBM <- data.frame(grouping=rep(c("Ecological",
                                 "Economic",
                                 "Governance",
                                 "Social & Cultural"),
                               times=c(3,3,3,4)),
                  labels=c("Habitat",
                               "Biodiversity",
                               "Productivity",
                               "Economic Effiency",
                               "Economic Equity",
                               "Economic Sustainability",
                               "Governance Outcomes",
                               "Governance Structure & Processes",
                               "Legal Obligations & Other Commitments",
                               "Culture",
                               "Ethical & Just Activities",
                               "Health & Well-being",
                               "Sustainable Communities"),
                  score=runif(13,55,100)) |>
  group_by(grouping) |>
  mutate(weight=1/n()) |>
  ungroup()

grade <- function(percent){
  cutoffs=c(0, seq(60, 100, by = 10/3))
  letters=c("F", paste0(toupper(rep(letters[4:1], each = 3)), rep(c("-","","+"),4)))
  cut(percent,cutoffs,letters)
}

Ecological <- data.frame(grouping=rep(c("Habitat",
                                      "Biodiversity",
                                      "Productivity"),
                                      times=c(3,3,3)),
                         labels=paste("indicator",1:9),
                         score=runif(9,55,100)) |>
  group_by(grouping) |>
  mutate(weight=1/n()) |>
  ungroup()

flowerplot <- function(df,grouping="grouping",labels="labels",score="score",weight="weight"){
  # browser()


  ngroups <- length(unique(df[[grouping]]))
  data <- data.frame(grouping=df[[grouping]],
                   labels=df[[labels]],
                   score=df[[score]],
                   weight=df[[weight]]) |>
    mutate(pos=cumsum(weight)-weight/2,
           lettergrade=grade(.data[[score]]))

  grouped_df <- data |>
    group_by(grouping) |>
    reframe(y=150,
            n=n()) |>
    mutate(x=1:ngroups-0.5,
           angle=360-(1:ngroups-0.5)/ngroups*360,
           angle=if_else(angle>90&angle<270,
                         angle-180,
                         angle)
           )

  ggplot(data,aes(width = weight))+
    geom_bar(stat="identity",size=0.2,color='lightgrey',aes(x=pos,y=100),fill="white")+
    geom_bar(stat="identity",size=0.2,color='black',aes(x=pos,y=score,fill=grade(score)))+
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
    geom_text(aes(label=grade(weighted.mean(score,weight))),
              x=0,
              y=-50,
              size=8)+
    geom_text(aes(label=gsub(" ","\n",labels),
                  x=pos,
                  y=120),
              size=2)+
    geom_errorbar(data=grouped_df,
                  inherit.aes=FALSE,
                  aes(x=ngroups:1-0.5,
                      ymin=rep(145,ngroups),
                      ymax=rep(145,ngroups)))+
    geom_text(data=grouped_df,
              inherit.aes = FALSE,
              aes(label=grouping,
                  x=x,
                  y=y,
                  angle=angle),
              size=3)
}

flowerplot(EBM)
flowerplot(Ecological)
