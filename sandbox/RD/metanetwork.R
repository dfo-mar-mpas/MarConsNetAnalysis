require(DiagrammeR)
require(dplyr)
require(tidyr)

typelegend <- data.frame(Type=c("Site-level Conservation Objectives",
                                "Regional-level Conservation Objectives",
                                "General Framework",
                                "Habitat Objective",
                                "Productivity Objective",
                                "Biodiversity Objective",
                                "Indicator"),
                         shape="rectangle",
                         fillcolor=c("#e41a1c",
                                     "#e41a1c",
                                     "#e41a1c",
                                     "#377eb8",
                                     "#4daf4a",
                                     "#984ea3",
                                     "#ff7f00"))

mf <- read.csv("sandbox/RD/metaframework.csv")
mf_ind <- read.csv("sandbox/RD/mf_indicators.csv") |>
  left_join(mf, by=c("Objective"="label_Objective"))

nodes <- data.frame(label=c(mf$label_Framework,
                            mf$label_Objective,
                            mf_ind$Indicator),
                    Type=c(mf$type_Framework,
                           paste(mf$type_Objective,"Objective"),
                           rep("Indicator",nrow(mf_ind))),
                    cluster=c(rep("Framework",nrow(mf)),
                              rep("Objective",nrow(mf)),
                              rep("Indicator",nrow(mf_ind)))) |>
  unique() |>
  arrange(Type) |>
  mutate(id=1:n(),
         color="transparent") |>
  left_join(typelegend,by="Type")


el <- data.frame(from=c(mf$label_Framework,
                        mf_ind$Objective),
                 to=c(mf$label_Objective,
                      mf_ind$Indicator))
eln <- el
eln[] <- nodes$id[match(unlist(el), nodes$label)]


g <- create_graph() |>
  add_nodes_from_table(nodes, label_col = label) |>
  add_edges_from_table(eln, from_col=from, to_col=to, from_to_map = id_external) |>
  add_global_graph_attrs("layout", "dot", "graph") |>
  add_global_graph_attrs("rankdir", "LR","graph") |>
  select_nodes_by_id(nodes = 1:length(labels)) |>
  set_node_attrs(node_attr = "fixedsize",values = FALSE) |>
  clear_selection()

render_graph(g)


g2 <- g |>
  select_nodes(conditions = Type != "Biodiversity Objective" & cluster == "Objective") |>
  delete_nodes_ws() |>
  select_nodes_by_degree(expressions = "deg==0")|>
  delete_nodes_ws()


g2|>
  render_graph()


#####

g3 <- g |>
  select_nodes(conditions = !(id %in% get_all_connected_nodes(g,node=nodes$id[nodes$label=="SSB"])))|>
  delete_nodes_ws() #|>
  # select_nodes_by_degree(expressions = "deg==0")|>
  # delete_nodes_ws()


g3 |>
  render_graph()

