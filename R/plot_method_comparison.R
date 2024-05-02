#' Creates a method comparison plot
#'
#' This function creates a method comparison plot for
#' data collected in MPAs
#'
#' @param datas a data frame or list of data frames obtained from `get_project_data()`
#' @param type an argument indicating what the output should be. Options include
#' `bar`, which produces a bar chart, `venn`, which produces a Venn Diagram, or
#' `table`, which produces a table.
#' @importFrom ggplot2 geom_bar
#' @examples
#' \dontrun{
#' library(MarConsNetData)
#' library(MarConsNetAnalysis)
#' d <- get_project_data(ids=c(1093, 642))
#' plot_method_comparison(datas=d)
#' }
#' @importFrom vegan specaccum
#' @importFrom ggplot2 ggplot
#' @export

plot_method_comparison <- function(datas=NULL, type="bar") {
  if (is.null(datas)) {
    stop("Must provide a datas argument, which is an output from getAppData()")
  }

  if (length(datas) < 2) {
    stop("Must provide at least two datasets to compare")
  }

  if (!(type %in% c("bar", "table", "venn"))) {
    stop("type must be either bar, table, or venn.")
  }

  if (type == "bar") {
  if (length(names(datas[[1]])) > 15) {
    warning("If you have not grouped species into taxonomic groups, consider doing getAppData(taxize=TRUE) to better see the results on a bar chart.")
  }
  }

  allSpecies <- unique(unlist(unname(lapply(datas, function(x) tolower(names(x))))))
  allSpecies <- sort(allSpecies[-(which(allSpecies %in% c("id", "lat", "lon")))])


  uniqueSpecies <- lapply(seq_along(datas), function(i) {
    other_indices <- setdiff(seq_along(datas), i)
    setdiff(tolower(names(datas[[i]])), tolower(unlist(lapply(other_indices, function(j) names(datas[[j]])))))
  })

  df <- data.frame("Group"=rep(allSpecies, each = 2), "sample"=rep(names(d),length(allSpecies)), "Presence"=0, "Proportion"=0)
#browser()
for (i in seq_along(datas)) {
  data_df <- datas[[i]]

  # Filter df based on the conditions
  # filtered_df <- df %>%
  #   filter(Group %in% tolower(names(data_df)), sample %in% unique(data_df$id))

  filtered_df <- df$Presence[intersect(which(df$Group %in% tolower(names(data_df))),which(df$sample %in% unique(data_df$id)))] <- 1

  # indices <- which(df$Group %in% filtered_df$Group & df$sample %in% filtered_df$sample)
  #
  # # Update the Presence column to 1 for the filtered rows
  # df$Presence[indices] <- 1
}
  prop <- vector(mode = "list", length = length(datas))
  for (i in seq_along(datas)) {
    datas[[i]] <-  datas[[i]][, order(names(datas[[i]]))]
    dd <- datas[[i]]
    NAMES <- names(dd)
    for (j in seq_along(NAMES)) {
      if (!(NAMES[j] %in% c("id", 'lat', 'lon'))) {
        prop[[i]][j] <- round(length(which(!(dd[,j] == 0)))/length(dd[,j])*100,1)
      }
    }
  }
  names(prop) <- names(datas)


  for (i in seq_along(datas)) {
    prop[[i]] <- prop[[i]][which(!(is.na(prop[[i]])))]
    dd <- datas[[i]]
    df$Proportion[which(df$sample == names(datas)[i] & df$Group %in% tolower(names(dd)))] <- prop[[i]]
  }


  #df$Group[which(df$sample == names(datas)[2] & df$Group %in% tolower(names(datas[[2]])))]




  if (type == "bar") {
  ggplot(df, aes(
    x = Group,
    fill = factor(sample),
    y = Proportion
  )) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_vline(
      xintercept = seq(0.5, length(unique(df$Group)) + 0.5, 1),
      color = "black",
      linetype = "solid",
      linewidth = 0.5
    ) +  # Vertical lines between species
    # geom_text(aes(label = Proportion, y = 0.5),
    #           position = position_dodge(width = 0.9),
    #           angle = 90) +
    labs(title = "Proportion of Sample Presence by Taxonomic Groups",
         x = "Taxonomic Groups",
         y = "Proportion (%)") +
    # scale_fill_manual(values = setNames(viridis(length(input$points)), unique(df$sample)), name = NULL) +  # Remove legend title
    scale_fill_brewer(palette = "Paired",name=NULL)+
    scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +  # Set y-axis to show only 0 and 1
    theme_bw()+
    theme(axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ))

  } else if (type == "table") {

    df2 <- data.frame(matrix(nrow = length(unique(df$Group)), ncol = length(datas)+1), stringsAsFactors = FALSE)
    names(df2) <- c("Group", names(datas))
    df2$Group <- unique(df$Group)

    for (i in seq_along(datas)) {
      df2[[names(datas)[[i]]]][which(df2$Group %in% tolower(names(datas[[i]])))] <- "true"
      df2[[names(datas)[[i]]]][which(is.na(df2[[names(datas)[[i]]]]))] <- "false"
    }

    return(df2)
  } else if (type == "venn") {
    stop("This has not been coded yet.")
  }

}













