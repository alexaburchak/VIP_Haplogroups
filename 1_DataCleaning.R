library(dplyr)

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
colnames(mtPhylo)[1] <- "haplogroup"
colnames(mtPhylo)[2] <- "Parent"
yPhylo <- read.csv("~/Desktop/y_phyloTree.csv")
colnames(yPhylo)[1] <- "haplogroup" # needs to be named haplogroup for now for the left_join
colnames(yPhylo)[2] <- "Parent"

# Remove vips whose haplogroup was not found in the y tree
# IF I HAVE TIME: I can add them back in by determining their branch manually
merged_y_tree <- left_join(vips_yDNA, yPhylo, by = "haplogroup")
vips_yDNA <- merged_y_tree %>%
  filter(Parent != "NA") %>%
  select(-Parent)

# Rename column 1 to "Child" in the phylo files
# They will need to be named this when I use the data.tree package later as this is the
# only way the program will recognize these values as branches to build a tree 
colnames(mtPhylo)[1] <- "Child"
colnames(yPhylo)[1] <- "Child"
