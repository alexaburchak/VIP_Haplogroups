library(shiny)
library(leaflet)
library(data.tree)
library(getwiki)

# Define a list of vips a user can compare their haplogroup to (mtDNA vs y)
mt_famous_options <- vips_mtDNA$Individual
y_famous_options <- vips_yDNA$Individual

# Define UI
ui <- fluidPage(
  titlePanel("DNA Common Ancestor Finder"),
  sidebarLayout(
    sidebarPanel(
      selectInput("haplogroup_type", "Select haplogroup type:",
                  choices = c("(Select)", "mtDNA", "yDNA")),
      uiOutput("haplogroup_selector"), 
      textInput("user_haplogroup", "Enter your haplogroup (ISOGG format):", placeholder = "Type your haplogroup here"),
      actionButton("find_ancestor", "Find Common Ancestor")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Results", htmlOutput("ancestor_info")),
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Biography", htmlOutput("famous_biography"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Define reactive expression for dynamically updating haplogroup selector
  output$haplogroup_selector <- renderUI({
    if (input$haplogroup_type == "mtDNA") {
      selectInput("vip", "Select a VIP:",
                  choices = c("(Select)", mt_famous_options))
    } else if (input$haplogroup_type == "yDNA") {
      selectInput("vip", "Select a VIP:",
                  choices = c("(Select)", y_famous_options)) 
    }
  })
  
  # Store common ancestor details using reactiveValues
  common_ancestor_details <- reactiveValues(details = NULL)
  
  # Find common ancestor on button click
  observeEvent(input$find_ancestor, {
    # Call function to find common ancestor based on input parameters
    common_ancestor <- find_common_ancestor(input$user_haplogroup, input$vip, input$haplogroup_type)
    # Store common ancestor details in reactiveValues
    common_ancestor_details$details <- common_ancestor
    # Fetch famous person info
    famous_info <- get_wiki(input$vip)
    # Render famous biography
    output$famous_biography <- renderUI({
      if (is.null(famous_info)) {
        return(NULL)
      } else {
        HTML(paste("<h3>", famous_info, "</h3>"))
      }
    })
  })
  
  # Render map
  output$map <- renderLeaflet({
    # Check if common ancestor details are available
    if (is.null(common_ancestor_details$details)) {
      return(NULL)
    }
    # Create a leaflet map with common ancestor location
    leaflet() %>%
      addTiles() %>%
      setView(lng = as.numeric(common_ancestor_details$details$longitude),
              lat = as.numeric(common_ancestor_details$details$latitude), zoom = 4) %>%
      addMarkers(lng = as.numeric(common_ancestor_details$details$longitude),
                 lat = as.numeric(common_ancestor_details$details$latitude),
                 popup = common_ancestor_details$details$name)
  })
  
  # Render common ancestor information
  output$ancestor_info <- renderUI({
    # Check if common ancestor details are available
    if (is.null(common_ancestor_details$details)) {
      return(NULL)
    }
    # Render HTML with common ancestor information and additional details
    HTML(paste("<h3>", common_ancestor_details$details$name, "</h3>",
               "<p>Country of Origin: ", common_ancestor_details$details$country_of_origin,
               "</p>", "<p>Date: ", common_ancestor_details$details$date,
               "</p>"))
  })
}

# Function to find common ancestor
find_common_ancestor <- function(user_hap, vip, haplogroup_type) {
  # Define datasets to look through based on haplogroup_type
  if (haplogroup_type == "mtDNA") {
    vip_set <- vips_mtDNA
    AADR_set <- AADR_mtDNA
    phylo_set <- mtPhylo
  } else {
    vip_set <- vips_yDNA
    AADR_set <- AADR_yDNA
    phylo_set <- yPhylo 
  }
  
  # Extract haplogroup of specified vip 
  vip_hap <- vip_set[vip_set$Individual == vip, 2]
  
  # Generate tree from phylogenetic table 
  tree = FromDataFrameNetwork(phylo_set)
  
  # path to vip
  vip_path <- FindNode(tree, vip_hap)$path
  
  # path to user 
  user_path <- FindNode(tree, user_hap)$path
  
  # compare paths to find common ancestral haplogroup 
  ancestor_haplogroup <- intersect(vip_path, user_path)[length(intersect(vip_path, user_path))]
  #print(paste("The ancestral haplogroup shared between you and", vip, "is:", ancestor_haplogroup))
  
  # Find all samples in the AADR set with the ancestral haplogroup
  all_ancestors <- subset(AADR_set, haplogroup == ancestor_haplogroup)
  
  # Determine the most ancient ancestor from all_ancestors 
  ancient_ancestor <- all_ancestors[which.min(all_ancestors$Date), ]
  #print(paste("The most ancient shared ancestor between you and", vip, "is:", ancient_ancestor[2]))
  
  # extract information from AADR table 
  country_of_origin <- ancient_ancestor$Origin
  longitude <- ancient_ancestor$Long.
  latitude <- ancient_ancestor$Lat.
  date <- ancient_ancestor$Date 
  name <- ancient_ancestor$Master.ID
  
  # Return relevant information including ancient ancestor
  return(list(
    ancestor_haplogroup = ancestor_haplogroup,
    ancient_ancestor = ancient_ancestor,
    country_of_origin = country_of_origin,
    longitude = longitude,
    latitude = latitude,
    date = date,
    name = name
  ))
}

# Run the application
shinyApp(ui = ui, server = server)