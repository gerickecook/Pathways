#### Load necessary packages and data ####
library(shiny)
library(networkD3)

options(shiny.error = browser)

setwd("C:/VS/CSF/Tools/Pathways_app")
load("data/AQIM_NetworkData.RData")

#Create unique list of available years for subsetting network data
years <- edges %>% select(Year) %>% unique()
rownames(years) <- c()


#very handy color ref: https://www.hexcolortool.com
ColourScale <- 'd3.scaleOrdinal()
            .domain(["Country", "State"])
            .range(["#4c83bd","#ff910a"]);'

#####  UI first so we can pass inputs to server ####

ui <- shinyUI(fluidPage(
  
  titlePanel("Network Analysis of CSF Import Risk to the States"),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$style("#force{height:100vh !important;}")),
      #sets opacity for the links
      sliderInput("opacity", "Opacity for links", value = 0.4, min = 0,
                  max = 1, step = .1),
      #sets sparsity of network to plot
      sliderInput("sparsity", "Percent to thin network", value = 0.5, min = 0,
                  max = 1, step = .1),
      #sets threshold for display of edges
      sliderInput("thresh", "Threshold link symbology", value = 100, min = 0,
                  max = 300, step = 20),
      #Pick year to subset network data before plotting
      selectInput("var", 
                  label = "Year",
                  choices = dput(years$Year),
                  selected = "2014")
    ),
    mainPanel(
      
      
      networkD3::forceNetworkOutput("force"),
      style = "border: 1px solid black;"
    )
  )
))

### Network function

plot_network <- function(data,in_year,opacity,sparsity, thresh){
  #subset by year
  edges <- subset(edges,Year==in_year)
  #Aggregate paths
  edges <- aggregate(Weight ~ SourceID + TargetID + SourceName + TargetName, data=edges,FUN=sum, na.rm=TRUE)
  
  #Rearrange df so weights are read correctly
  in_df <- data.frame(edges[,3:4],Weight=edges$Weight)
  
  net <- simplify(graph.data.frame(in_df, directed=TRUE))
  E(net)$Weight <- in_df$Weight
  
  #Sparsify the network
  cut.off <- quantile(in_df$Weight,probs=sparsity)
  net.sp <- delete.edges(net, E(net)[Weight<cut.off])
  net.sp <- delete.vertices(net.sp,which(degree(net.sp)<1))
  #Vertice attributes
  type <- merge(data.frame(name=V(net.sp)$name,idx=seq(1,length(V(net.sp)$name),1)), nodes, by.x = 'name',by.y="ISO")
  type <- type[order(type$idx),] 
  V(net.sp)$name <- V(net.sp)$name
  V(net.sp)$type <- type$Type
  maxE <- max(E(net.sp)$Weight)
  as <- authority_score(net.sp, weights=E(net.sp)$weight/maxE)$vector
  
  #Adjust nodes df for force network

  vertices <- data.frame(name = V(net.sp)$name, 
                         type <- V(net.sp)$type,
                         as = authority_score(net.sp, weights=E(net.sp)$weight/maxE)$vector)
  colnames(vertices) <- c("name","type","as")
  vertices$size <- vertices$as * 20
  
  d3_list <- igraph_to_networkD3(net.sp, group = type)
  #Add nodesize (authority score) to graph
  tmp <- merge(data.frame(name=V(net)$name,idx=seq(1,length(V(net)$name),1)), vertices, by="name")
  tmp <- tmp[order(tmp$idx),]
  d3_list$nodes$size <- tmp$size
  
  forceNetwork(Links = d3_list$links, 
               Nodes = vertices, 
               Source = "source", 
               Target = "target",
               Value = "value",
               NodeID = "name",
               Group = "type",
               fontSize = 12,
               Nodesize = 4,
               radiusCalculation = JS("Math.sqrt(d.nodesize)*4+2"),
               colourScale = ColourScale,
               linkColour = ifelse(d3_list$links$value > thresh, "rgba(67, 199, 203, 1)",paste("rgba(166, 235, 237,",opacity,")",sep='')),
               zoom = TRUE,
               legend = TRUE,
               linkDistance = 40,
               opacity = 1,
               opacityNoHover = 0.9,
               bounded=TRUE)
}

#### Server ####
server <- function(input, output) {

  output$force <- renderForceNetwork({
    plot_network(edges,input$var, input$opacity, input$sparsity, input$thresh)
  })
  
}



#### Run ####

shinyApp(ui = ui, server = server)
