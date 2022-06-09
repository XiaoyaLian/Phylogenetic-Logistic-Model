rm(list=ls())
install.packages("BiocManager")
BiocManager::install("ggtreeExtra")


# Load packages.
# used in Practical 2
library(stringr)
library(dplyr)
library(ggtree)
library(ggtreeExtra)
library(ggplot2)
# used in Completeness code
library(caper)
library(magrittr)
# library(janitor)
library(ape)
library(phytools)


# Load trait data
jetz_traits <- read.csv("C:/Users/13988/Desktop/FinalStatisticNoSO.csv")
colnames(jetz_traits)
# Add in underscore. 
jetz_traits$Jetz_Name <- gsub(" ", "_", jetz_traits$Jetz_Name)

# Summarise for families.  
family_scores <- jetz_traits %>% 
  group_by(Family) %>% 
  summarise(jetz_name = first(Jetz_Name), # Get the name for tree plotting.
            mean_score = mean(na.omit(New_score)), # Calculate mean SS score for each family
            sqrt_score = sqrt(mean_score),
            dominant_trophic_level = names(which.max(table(Trophic_level))) # pull out the dominant trophic level for each family
  )


# Read in tree and merge/prepare data.
jetz_tree <- read.tree("C:/Users/13988/Desktop/Final Project/BirdzillaHackett1.tre")[[101]]
## Return the object.
jetz_tree
## Get the structure of the object.
str(jetz_tree)

# Create a family tree.
family_tree <- drop.tip(jetz_tree, setdiff(jetz_tree$tip.label, family_scores$jetz_name))

# Add node data.
family_scores$node <-  nodeid(family_tree, family_scores$jetz_name)

# Add a blank score for the back.
family_scores$blank <- 100
# Create a label with family size.
family_scores$plot_label <- paste0(family_scores$sqrt_score)

# Merge trees and dataframes.
family_plot_data <-  full_join(family_tree, family_scores, by = "node")


# Open a new plotting window.
dev.off() 
dev.new(noRStudioGD = TRUE)
# Plot the tree.
# library(ggtreeExtra)
(try_plot <- ggtree(family_plot_data, layout="fan", size = 0.95, aes(colour=dominant_trophic_level)) +  # size= is used to change the width of branch
    # geom_tiplab(size = 1.5) +
    scale_color_manual(values = c("#E69F00", "#56B4E9"), breaks=c("Herbivore", "Carnivore")) +
    
    # Geom fruit allows us to specify the ggplot geom we want (show mean SS score at the end)
    geom_fruit(geom=geom_bar,
               mapping=aes(y=node, x=sqrt_score),
               pwidth=0.24,
               orientation="y", 
               stat="identity", fill="navy", colour="navy", width=0.2) +   # width= is used to change the width of bars
    
    # We can remove some of the white space around our plot by setting the margins to negative values.
    theme(plot.margin=margin(-80,-80,-80,-80)))
