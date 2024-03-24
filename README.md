# README.md

## Overview
This repository contains three R scripts designed for data cleaning and visualization related to haplogroup analysis. 
These scripts are intended to be used in conjunction with the provided raw data stored in the "Raw_Data" folder.

### Scripts
1. **1_DataCleaning.R**: This script is responsible for cleaning and processing the raw data files to prepare them for 
further analysis. It loads the VIP datasets (`vips_mtDNA` and `vips_yDNA`) and the AADR dataset (`AADR`). It performs 
tasks such as fixing column names, adjusting date formats, splitting the AADR dataset into two separate tables based on 
haplogroup type, and filtering out irrelevant entries. Additionally, it loads the parent-leaf pairs for both 
mitochondrial DNA and Y-DNA trees, and removes VIPs whose haplogroups are not found in the Y-DNA tree.

2. **2_RenameRoyals.R**: This script renames the VIPs in the mitochondrial DNA and Y-DNA datasets to their 
corresponding historical figures or famous individuals. It provides a comprehensive list of name replacements for 
better identification and clarity.

3. **3_BuildApp.R**: This script builds a Shiny web application for interactive haplogroup analysis. It utilizes 
various R packages such as `shiny`, `leaflet`, `data.tree`, `getwiki`, `ggtree`, `ggplot2`, and `ape`. Users can select 
the type of haplogroup (mtDNA or Y-DNA), enter their haplogroup in ISOGG format, and select a VIP from the provided 
list. The application then finds the common ancestor between the user's haplogroup and the selected VIP, displays 
relevant information, and visualizes the ancestral tree.

## Usage
To use these scripts, follow these steps:
1. Ensure that R (V 4.3.2) and the required packages are installed on your system.
2. Clone or download this repository to your local machine.
```sh
mkdir ~/VIP_Haplogroups
cd ~/VIP_Haplogroups
git clone git@github.com:alexaburchak/VIP_Haplogroups.git
```
3. Unzip the raw data files from the "Raw_Data" folder within the repository.
```sh
cd ~/VIP_Haplogroups
unzip Raw_Data
```
4. Execute the scripts in the following order:
   - `1_DataCleaning.R`
   - `2_RenameRoyals.R`
   - `3_BuildApp.R`
5. Interact with the Shiny web application generated by `3_BuildApp.R` to analyze haplogroup data.

### Example input and output
Input: After running 3_BuildApp.R, select 'mtDNA' for your haplogroup type, 'Cnut' for your VIP and enter "C4a'b'c" as 
your haplogroup. 

Output: Upon clicking "Find Common Ancestor", you should receive information about BRZ001, a common ancestor that dates
back to 3870 BCE and originated in Czech Republic. You will also be able to visualize where BRZ001 came from on a map
and learn more about Cnut from his Wikipedia page.  

## Raw Data
All raw data required for analysis can be found in the "Raw_Data" folder within this repository. Ensure that the 
file paths specified in the scripts point to the correct locations of these raw data files.

For any questions or issues, please contact @alexaburchak.

