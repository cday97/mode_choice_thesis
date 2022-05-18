
##--maps-----------------------------------------------------------------------------------------------------------------------##
make_taz_map <- function(){
  sf_use_s2(FALSE)
  geo <- st_read('https://raw.githubusercontent.com/byu-transpolab/populationsim_wfrc/master/inputs/taz.geojson') %>%
    st_transform(crs = 4326) %>%
    filter(CO_FIPS != 0)
  utah <- st_read("data/Utah_State_Boundary-shp/Utah.shp") %>%
    st_transform(crs = 4326) %>%
    head(1)
  map1 <- ggplot() + 
    annotation_map_tile() +
    geom_sf(data = geo, mapping = aes(color = factor(CO_FIPS))) +
    scale_color_brewer(labels = c("Box Elder", "Davis", "Salt Lake", "Utah", "Weber"), palette = "Dark2") +
    theme_bw() +
    guides(color=guide_legend("County")) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  map1
}

make_bignet_map <- function(){
  roads <- st_read("data/Utah_Roads/Utah_Roads.shp") %>%
    st_transform(crs = 4326)
  croproads <- st_crop(roads,ymax = 41.4, ymin = 39.9, xmax = -112.15, xmin = -111.58)
  bignetwork <- ggplot() + 
    geom_sf(data = croproads, size = .1, color = "black") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  bignetwork
}

get_bignet_pic <- function(bignet){
  ggsave("pics/network.png",bignet)
  knitr::include_graphics("pics/network.png")
}

make_smallnet_map <- function(){
  roads <- st_read("data/Utah_Roads/Utah_Roads.shp") %>%
    st_transform(crs = 4326)
  cropsalt <- st_crop(roads,ymax = 40.7, ymin = 40.5, xmax = -112, xmin = -111.8)
  smallnetwork <- ggplot() + 
    geom_sf(data = cropsalt, size = .1, color = "black") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  smallnetwork
}

get_smallnet_pic <- function(smallnet){
  ggsave("pics/smallnetwork.png",smallnet)
  knitr::include_graphics("pics/smallnetwork.png")
}

##--diagrams-----------------------------------------------------------------------------------------------------------------------##

make_mnl_flow <- function(){
  #https://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html
  DiagrammeR::grViz("digraph {
    graph [layout = dot, rankdir = LR]
    node [shape = rectangle, style = filled, fillcolor = Linen]

    input1 [label = 'Alternatives', shape = folder, fillcolor = Beige]
    input2 [label = 'Person & \n Household \n Attributes', shape = folder, fillcolor = Beige]
    input3 [label = 'Destination \n Activity', shape = folder, fillcolor = Beige]
    input4 [label = 'MNL Utility \n Parameters', shape = folder, fillcolor = Beige]
    calculator [label =  'Mode Choice \n Calculator']
    logit [label = 'Multinomial \n Logit']
    mode [label= 'Trip Mode', shape = oval, fillcolor = Moccasin]

    {input1 input2 input3}  -> calculator -> logit -> mode
     input4 -> logit}")
}

make_lccm_flow <- function(){
  DiagrammeR::grViz("digraph {
    graph [layout = dot, rankdir = LR]
    node [shape = rectangle, style = filled, fillcolor = Linen]

    input1 [label = 'Alternatives', shape = folder, fillcolor = Beige]
    input2 [label = 'Person & \n Household \n Attributes', shape = folder, fillcolor = Beige]
    input3 [label = 'Destination \n Activity', shape = folder, fillcolor = Beige]
    input4 [label = 'LCCM Utility \n Parameters', shape = folder, fillcolor = Beige]
    calculator [label =  'Mode Choice \n Calculator']
    lccm [label = 'Latent Class \n Choice Model']
    logit [label = 'Multinomial \n Logit']
    mode [label= 'Trip Mode', shape = oval, fillcolor = Moccasin]

    {input1 input2 input3}  -> calculator -> lccm -> logit -> mode
     input4 -> lccm}")
}

make_tpcm_flow <- function(){
  DiagrammeR::grViz("digraph {
    graph [layout = dot, rankdir = LR]
    node [shape = rectangle, style = filled, fillcolor = Linen]

    input1 [label = 'Alternatives', shape = folder, fillcolor = Beige]
    input2 [label = 'Person & \n Household \n Attributes', shape = folder, fillcolor = Beige]
    input3 [label = 'Destination \n Activity', shape = folder, fillcolor = Beige]
    input4 [label = 'Tour Purpose', shape = folder, fillcolor = Beige]
    input5 [label = 'ActivitySim \n Utility \n Parameters', shape = folder, fillcolor = Beige]
    calculator [label =  'Mode Choice \n Calculator']
    tpcm [label = 'Tour Purpose \n Choice Model']
    logit [label = 'Multinomial \n Logit']
    mode [label= 'Trip Mode', shape = oval, fillcolor = Moccasin]

    {input1 input2 input3 input4}  -> calculator -> tpcm -> logit -> mode
     input5 -> tpcm}")
}









