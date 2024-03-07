library(shiny)
library(leaflet)
library(wikipediatrend)
library(dplyr)
library(data.tree)

# Load VIP datasets 
vips_mtDNA <- read.csv("~/Desktop/vipsMtDNA.csv")
colnames(vips_mtDNA)[2] <- "mtDNA.haplogroup" 
vips_yDNA <- read.csv("~/Desktop/vipsYDNA.csv")[, -c(4, 5)]
colnames(vips_yDNA)[2] <- "Y.haplogroup"

# Load AADR dataset 
AADR <- read.csv("~/Desktop/AADR_Annotation.csv")[, -c(1,4,5,6,7,8,10,11,12,18,19,20,
                                                       21,22,23,24,25,26,28,30,31,32,
                                                       33,34,35,36)]

# Fix column names 
colnames(AADR)[3] = "Date"
colnames(AADR)[9] = "Y.haplogroup"
colnames(AADR)[10] = "mtDNA.haplogroup"

# Fix date column to reflect true year (if it's negative it's BCE, if it's positive it's CE)
AADR$Date <- 1950 - AADR$Date

# Split into 2 AADR tables: one with mtDNA and one with yDNA 
AADR_mtDNA <- data.table::copy(AADR)
AADR_mtDNA <- AADR_mtDNA %>%
  select(-Y.haplogroup) %>%
  # Keep only entries with mtDNA 
  filter(grepl("^[A-Za-z0-9]+$", mtDNA.haplogroup)) 
colnames(AADR_mtDNA)[6] <- "Origin"

AADR_yDNA <- AADR %>%
  select(-mtDNA.haplogroup) %>%
  # Keep only entries with Y 
  filter(grepl("^[A-Za-z0-9]+$", Y.haplogroup))
colnames(AADR_yDNA)[6] <- "Origin"

# For ease of joining datasets later, rename all haplogroup columns to "haplogroup"
colnames(vips_mtDNA)[2] <- "haplogroup"
colnames(vips_yDNA)[2] <- "haplogroup"
colnames(AADR_mtDNA)[9] <- "haplogroup"
colnames(AADR_yDNA)[9] <- "haplogroup"

# Load Parent-Leaf Pairs 
mtPhylo <- read.csv("~/Desktop/mt_phyloTree.csv", quote = "")
colnames(mtPhylo)[1] <- "Child"
colnames(mtPhylo)[2] <- "Parent"
yPhylo <- read.csv("~/Desktop/y_phyloTree.csv")
colnames(yPhylo)[1] <- "Child"
colnames(yPhylo)[2] <- "Parent"

# Remove vips whose haplogroup was not found in the y tree
# IF I HAVE TIME: I can add them back in by determining their branch
merged_y_tree <- left_join(vips_yDNA, yPhylo, by = "haplogroup")
vips_yDNA <- merged_y_tree %>%
  filter(Parent != "NA") %>%
  select(-Parent)

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
        tabPanel("Results", tableOutput("result_table")),
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Biography", htmlOutput("famous_person_info"))
      )
    )
  )
)

# Define server logic
# THIS NEEDS WORK 
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
  })
  
  # Render result table
  output$result_table <- renderTable({
    # Check if common ancestor details are available
    if (is.null(common_ancestor_details$details)) {
      return(NULL)
    }
    # Return the common ancestor details as a table
    common_ancestor_details$details
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
      setView(lng = common_ancestor_details$details$longitude,
              lat = common_ancestor_details$details$latitude, zoom = 4) %>%
      addMarkers(lng = common_ancestor_details$details$longitude,
                 lat = common_ancestor_details$details$latitude,
                 popup = common_ancestor_details$details$name)
  })
  
  # Render famous person information
  output$famous_person_info <- renderUI({
    # Check if common ancestor details are available
    if (is.null(common_ancestor_details$details)) {
      return(NULL)
    }
    # Render HTML with common ancestor information and additional details
    HTML(paste("<h3>", common_ancestor_details$details$name, "</h3>",
               "<p>Country of Origin: ", common_ancestor_details$details$country,
               "</p>", "<p>Period: ", common_ancestor_details$details$date,
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
  print(paste("The ancestral haplogroup shared between you and", vip, "is:", ancestor_haplogroup))
  
  # Find all samples in the AADR set with the ancestral haplogroup
  all_ancestors <- subset(AADR_set, haplogroup == ancestor_haplogroup)
  
  # Determine the most ancient ancestor from all_ancestors 
  ancient_ancestor <- all_ancestors[which.min(all_ancestors$Date), ]
  print(paste("The most ancient shared ancestor between you and", vip, "is:", ancient_ancestor[2]))
  
  # extract information from AADR table 
  country_of_origin <- ancient_ancestor$Origin
  longitude <- ancient_ancestor$Long.
  latitude <- ancient_ancestor$Lat.
  date <- ancient_ancestor$Date 
}  

# Function to get famous person info from Wikipedia
# THIS NEEDS WORK 
get_famous_person_info <- function(name) {
  # Use wikipediatrend package to fetch information from Wikipedia
  person_info <- wikide(trend = name, language = "en")
  if (is.null(person_info)) {
    return("Information not available.")
  }
  return(person_info)
}

# Run the application
shinyApp(ui = ui, server = server)