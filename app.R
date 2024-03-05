library(shiny)
library(leaflet)
library(wikipediatrend)
library(dplyr)

# Load all necessary data 
# Load VIP datasets 
vips_mtDNA <- read.csv("~/Desktop/vipsMtDNA.csv")
colnames(vips_mtDNA)[2] <- "mtDNA.haplogroup" 
vips_yDNA <- read.csv("~/Desktop/vipsYDNA.csv")[, -c(4, 5)]
colnames(vips_yDNA)[2] = "Y.haplogroup"

# Load AADR dataset 
AADR <- read.csv("~/Desktop/AADR_Annotation.csv")[, -c(1,4,5,6,7,8,9,10,12,18,19,20,21,22,23,24,25,26,28,30,31,32,33,34,35,36)]
colnames(AADR)[3] = "Full.Date"
colnames(AADR)[9] = "Y.haplogroup"
colnames(AADR)[10] = "mtDNA.haplogroup"

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

# Load Parent-Leaf Pairs 
mtPhylo <- read.csv("~/Desktop/mt_phyloTree.csv", quote = "")[-c(3477,3478),]
colnames(mtPhylo)[1] <- "Leaf"
colnames(mtPhylo)[2] <- "Parent"
yPhylo <- read.csv("~/Desktop/y_phyloTree.csv")
colnames(yPhylo)[1] <- "Leaf"
colnames(yPhylo)[2] <- "Parent"

# Define UI
ui <- fluidPage(
  titlePanel("DNA Common Ancestor Finder"),
  sidebarLayout(
    sidebarPanel(
      selectInput("haplogroup_type", "Select haplogroup type:",
                  choices = c("mtDNA", "yDNA")),
      textInput("haplogroup", "Enter your haplogroup:"),
      actionButton("find_ancestor", "Find Common Ancestor")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Results", tableOutput("result_table")),
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Famous Person", uiOutput("famous_person_info"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Store common ancestor details
  common_ancestor_details <- reactiveValues()
  
  # Find common ancestor on button click
  observeEvent(input$find_ancestor, {
    common_ancestor <- find_common_ancestor(input$haplogroup, 
                                            input$haplogroup_type)
    common_ancestor_details$details <- common_ancestor
  })
  
  output$result_table <- renderTable({
    if (is.null(common_ancestor_details$details)) {
      return(NULL)
    }
    common_ancestor_details$details
  })
  
  output$map <- renderLeaflet({
    if (is.null(common_ancestor_details$details)) {
      return(NULL)
    }
    leaflet() %>%
      addTiles() %>%
      setView(lng = common_ancestor_details$details$Long.,
              lat = common_ancestor_details$details$Lat., zoom = 4) %>%
      addMarkers(lng = common_ancestor_details$details$Long.,
                 lat = common_ancestor_details$details$Lat.,
                 popup = common_ancestor_details$details$location)
  })
  
  output$famous_person_info <- renderUI({
    if (is.null(common_ancestor_details$details)) {
      return(NULL)
    }
    person_info <- get_famous_person_info(common_ancestor_details$details$name)
    HTML(paste("<h3>", common_ancestor_details$details$name, "</h3>",
               "<p>Country of Origin: ", common_ancestor_details$details$country,
               "</p>", "<p>Period: ", common_ancestor_details$details$period,
               "</p>", "<p>More about ", common_ancestor_details$details$name,
               ": ", person_info, "</p>", sep = ""))
  })
}


# Find common ancestor
# Function to check if two nodes are identical
nodes_identical <- function(node1, node2) {
  return(identical(node1[1:2], node2[1:2]))
}

find_common_ancestor <- function(user_haplogroup, haplogroup_type) {
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
  
  # Merge the datasets to get the full phylogenetic tree
  full_tree <- merge(phylo_set, vip_set, by.x="haplogroup", by.y="haplogroup")
  
  # Function to find common ancestor
  find_ancestor <- function(haplogroup) {
    # Find the row corresponding to the haplogroup
    node <- full_tree[full_tree$haplogroup == haplogroup,]
    # If haplogroup not found, return NULL
    if (nrow(node) == 0) {
      return(NULL)
    }
    # If parent haplogroup is NA, it means it's the root
    if (is.na(node$parent_haplogroup)) {
      return(node)
    }
    # Recursively find parent haplogroup
    parent <- find_ancestor(node$parent_haplogroup)
    return(parent)
  }
  
  # Find ancestors for user's haplogroup
  user_ancestor <- find_ancestor(user_haplogroup)
  
  # If user haplogroup does not exist in the tree, return NULL
  if (is.null(user_ancestor)) {
    return(NULL)
  }
  
  # Allow user to interactively explore common ancestors with other famous individuals
  explore_common_ancestors <- function(famous_person) {
    # Find ancestors for famous person's haplogroup
    famous_ancestor <- find_ancestor(famous_person)
    
    # If famous person haplogroup does not exist in the tree, return NULL
    if (is.null(famous_ancestor)) {
      return(NULL)
    }
    
    # Find common ancestor
    while (!nodes_identical(user_ancestor, famous_ancestor)) {
      # If either user or famous ancestor is root, return NULL (no common ancestor)
      if (is.na(user_ancestor$parent_haplogroup) || is.na(famous_ancestor$parent_haplogroup)) {
        return(NULL)
      }
      # Move both ancestors up the tree
      user_ancestor <- find_ancestor(user_ancestor$parent_haplogroup)
      famous_ancestor <- find_ancestor(famous_ancestor$parent_haplogroup)
    }
    
    # Retrieve latitude, longitude, and date from AADR dataset for common ancestor
    common_ancestor_info <- AADR_set[AADR_set$haplogroup == famous_ancestor$haplogroup, ]
    
    # Return the common ancestor with its details
    return(common_ancestor_info)
  }
  
  return(explore_common_ancestors)
}

# Function to get famous person info from Wikipedia
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

