require(ggplot2)
require(shiny)
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

Ecological <- data.frame(grouping=rep(c("Biodiversity",
                                        "Habitat",
                                      "Productivity"),
                                      times=c(3,5,3)),
                         labels=c("Genetic Diversity",
                                  "Species Diversity",
                                  "Functional Diversity",

                                 "Environmental & Representativity",
                                 "Key Fish Habitat",
                                 "Connectivity",
                                 "Uniqueness",
                                 "Threats to Habitat",

                                 "Biomass Metrics",
                                 "Structure and Function",
                                 "Threats to Productivity"),
                         score=runif(11,55,100)) |>
  # group_by(grouping) |>
  # mutate(weight=1/n()) |>
  mutate(weight=runif(11,1,10)) |>
  ungroup()|>
  mutate(angle=(cumsum(weight)-weight/2)/sum(weight)*360)


flowerplot <- function(df,grouping="grouping",labels="labels",score="score",weight="weight"){
  # browser()


  ngroups <- length(unique(df[[grouping]]))
  data <- data.frame(grouping=factor(df[[grouping]],levels = unique(df[[grouping]])),
                   labels=factor(df[[labels]],levels = unique(df[[labels]])),
                   score=df[[score]],
                   weight=df[[weight]]) |>
    mutate(weight=weight/sum(weight),
           pos=cumsum(weight)-weight/2,
           lettergrade=grade(.data[[score]]))

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

# flowerplot(EBM)
flowerplot(Ecological)

# Shiny example of how to return the label when the figure is clicked

ui <- fluidPage(
  plotOutput('flower', click = "click"),
  textOutput('cut')
)

server <- function(input, output, session) {
  output$flower <- renderPlot({
    flowerplot(Ecological)
  })

  output$cut <- renderText({
    req(input$click)

    xscale <- 0.5
    yscale <- 205/2
    clickangle <- 90-atan2((input$click$y+50-yscale)/yscale,
                   (input$click$x-xscale)/xscale)*180/pi
    if(clickangle<0) clickangle <- 360+clickangle

    paste(Ecological$labels[which.min(abs(Ecological$angle-clickangle))])


  })
}

shinyApp(ui, server)
