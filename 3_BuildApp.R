# Load necessary packages 
#install.packages("shiny")
library(shiny)
#install.packages("leaflet")
library(leaflet)
#install.packages("data.tree")
library(data.tree)
#install.packages("getwiki")
library(getwiki)
#install.packages("ggtree")
library(ggtree)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("ape")
library(ape)

# Load necessary input files 


# Define a list of vips a user can compare their haplogroup to (mtDNA vs y)
mt_famous_options <- vips_mtDNA$Individual
y_famous_options <- vips_yDNA$Individual

# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap")
  ),
  tags$style(
    HTML("
      body {
        background-color: #f0f7fc;
        font-family: 'Roboto', sans-serif;
      }
      .container {
        background-color: #ffffff;
        border-radius: 10px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        padding: 20px;
        margin: 20px;
      }
      .intro-text {
        font-size: 20px;
        color: #333;
        margin-bottom: 20px;
      }
      .btn-primary {
        background-color: #007bff;
        border-color: #007bff;
        color: #fff;
        font-weight: bold;
      }
      .btn-primary:hover {
        background-color: #0056b3;
        border-color: #0056b3;
      }
      .panel-title {
        font-family: 'Roboto', sans-serif; /* Change font of panel title to Roboto */
        font-size: 24px;
        font-weight: bold;
        color: #007bff;
        margin-bottom: 20px;
      }
      .tree-plot-container {
        background-color: #ffffff;
        padding: 20px;
        margin-bottom: 20px;
        border-radius: 10px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        max-width: 100%; /* Set maximum width to ensure the plot doesn't exceed the container */
        overflow-x: auto; /* Add horizontal scroll if necessary */
      }
    ")
  ),
  titlePanel("Welcome to NobleRoots!"),
  div(class = "container",
      sidebarLayout(
        sidebarPanel(
          selectInput("haplogroup_type", "Select haplogroup type:",
                      choices = c("(Select)", "mtDNA", "yDNA")),
          uiOutput("haplogroup_selector"), 
          textInput("user_haplogroup", "Enter your haplogroup (ISOGG format):", placeholder = "Type your haplogroup here"),
          actionButton("find_ancestor", "Find Common Ancestor", class = "btn-primary")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Results", 
                     uiOutput("ancestor_info"),
                     div(id = "tree_plot_container", class = "tree-plot-container", imageOutput("tree_plot", width = "100%"))
            ),
            tabPanel("Map", leafletOutput("map")),
            tabPanel("Biography", htmlOutput("famous_biography"))
          )
        )
      )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Define reactive expression for dynamically updating haplogroup selector
  output$haplogroup_selector <- renderUI({
    if (input$haplogroup_type == "mtDNA") {
      selectizeInput("vip", "Select a VIP:",
                     choices = c("(Select)", mt_famous_options),
                     options = list(
                       placeholder = "Start typing to search...",
                       onInitialize = I("function() { this.setValue(''); }")
                     ))
    } else if (input$haplogroup_type == "yDNA") {
      selectizeInput("vip", "Select a VIP:",
                     choices = c("(Select)", y_famous_options),
                     options = list(
                       placeholder = "Start typing to search...",
                       onInitialize = I("function() { this.setValue(''); }")
                     )) 
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
        HTML(paste("<h3>", famous_info, "</h3>",
                   "<p>Source: Wikipedia</p>"))
      }
    })
    # Render tree plot
    output$tree_plot <- renderImage({
      tree_plot <- common_ancestor$tree_plot
      outfile <- tempfile(fileext = '.png')
      ggsave(outfile, tree_plot, width = 10, height = 6, units = "in")
      list(
        src = outfile,
        contentType = 'image/png',
        width = '100%',  # Make the width responsive
        height = 'auto',
        alt = "Tree Plot"
      )
    }, deleteFile = TRUE)
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
    
    # Determine if the date is BCE or CE
    if (common_ancestor_details$details$date < 0) {
      date_text <- paste(abs(common_ancestor_details$details$date), "BCE", sep = " ")
    } else {
      date_text <- paste(common_ancestor_details$details$date, "CE", sep = " ")
    }
    
    # Render HTML with common ancestor information and additional details
    HTML(paste("<p style='font-size: x-large;'>You and ", input$vip, 
               " share a common ancestor <strong>", 
               common_ancestor_details$details$name, "</strong>. 
               This ancestral connection dates back to <strong>", 
               date_text, "</strong> <span style='font-weight: normal;'>and 
               originates in</span> <strong>", 
               common_ancestor_details$details$country_of_origin, 
               "</strong>.</p>", sep = ""))
    
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
  
  # Find all samples in the AADR set with the ancestral haplogroup
  all_ancestors <- subset(AADR_set, haplogroup == ancestor_haplogroup)
  
  # Determine the most ancient ancestor from all_ancestors 
  ancient_ancestor <- all_ancestors[which.min(all_ancestors$Date), ]
  
  # extract information from AADR table 
  country_of_origin <- ancient_ancestor$Origin
  longitude <- ancient_ancestor$Long.
  latitude <- ancient_ancestor$Lat.
  date <- ancient_ancestor$Date 
  name <- ancient_ancestor$Master.ID
  
  # Build a phylogenetic tree 
  # Define the tree structure
  tree_data <- data.frame(
    parent = c("Root", "Root"),
    child = c("A", "B"),
    stringsAsFactors = FALSE
  )
  
  # Create a tree object
  tree <- as.phylo(tree_data)
  
  # Plot the tree with the root at the top and without the scale
  tree_plot<- tree_plot <- ggtree(tree, layout = "dendrogram") +
    geom_label(aes(x=-1.2, y=1.5, label=ancient_ancestor$Master.ID), size = 8, fontface = "bold") +
    geom_segment(aes(x = -1, y = 1.5, xend = -1.15, yend = 1.5), color = "red", size = 1.5) + # Line leading to ancestral haplogroup 
    geom_segment(aes(x = -1, y = 1.5, xend = -1, yend = 2), color = "red", size = 1.5) + # Line connecting ancestral haplogroup to vip
    geom_segment(aes(x = -1, y = 2, xend = 0, yend = 2), color = "red", size = 1.5) + # Line leading to vip
    geom_label(aes(x=0 , y=1, label="You"), size = 8, fontface = "bold") +
    geom_label(aes(x=0 , y=2, label=vip), size = 8, color = "red", fontface = "bold") +
    theme_tree2() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none")
  
  # Return relevant information including ancient ancestor
  return(list(
    ancestor_haplogroup = ancestor_haplogroup,
    ancient_ancestor = ancient_ancestor,
    country_of_origin = country_of_origin,
    longitude = longitude,
    latitude = latitude,
    date = date,
    name = name,
    tree_plot = tree_plot  # Pass the tree plot to the output
  ))
}

# Run the application
shinyApp(ui = ui, server = server)