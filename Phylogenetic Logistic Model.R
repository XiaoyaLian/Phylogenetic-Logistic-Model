# Load the bird diet, latitude and Sexual Selection data.
jetz_traits <- read.csv("C:/Users/13988/Desktop/FinalDataNoSO.csv", header = TRUE)
# Check it's been imported.
str(jetz_traits)
head(jetz_traits)
# Add in underscore. 
jetz_traits$Jetz_Name <- gsub(" ", "_", jetz_traits$Jetz_Name)
# Remove any NAs in the data (make sure to check you're not loosing too much data!)
jetz_traits <- na.omit(jetz_traits)

# Convert character trophic level variable to factor
jetz_traits$Trophic_level <- as.factor(jetz_traits$Trophic_level)
class(jetz_traits$Trophic_level)
levels(jetz_traits$Trophic_level)

# Transform sexual score into a binary variable. Grouping 0-2 and 3-4 together
jetz_traits$New_score<-ifelse(jetz_traits$New_score>2,1,0)
table(jetz_traits$New_score)

# The abs function takes absolute value.
jetz_traits$Latitude <- as.numeric(jetz_traits$Latitude)
jetz_traits$abs_latitude <- abs(jetz_traits$Latitude)
# Scale variables.
jetz_traits$abs_latitude_scaled <- scale(jetz_traits$abs_latitude)



# Load packages.
library(ape)
library(phylolm)
library(phytools)

# Read in the tree.
jetz_tree <- read.tree("C:/Users/13988/Desktop/Final Project/BirdzillaHackett1.tre")[[101]]
# Return the object.
jetz_tree
# Get the structure of the object.
str(jetz_tree)
# Lets see the first 10 species in the tree.
head(jetz_tree$tip.label, 10)

# Drop the tips on the tree. Set diff will find all the tips that don't match, and then drop.tip will remove them
pruned_birdtree <- drop.tip(jetz_tree, setdiff(jetz_tree$tip.label, jetz_traits$Jetz_Name))
str(pruned_birdtree)
# Plot the smaller tree.
# plotTree(pruned_birdtree, ftype="i")

# Make a covariance matrix.
model_covar <- vcv.phylo(pruned_birdtree)
# Make sure the covariance matrix and data are in the same order.
row.names(jetz_traits) <- jetz_traits$Jetz_Name
jetz_traits <- jetz_traits[row.names(model_covar),]

# Run the model.
phylogenetic_logistic_model <- phyloglm(New_score ~ Trophic_level*abs_latitude_scaled, data=jetz_traits, phy=pruned_birdtree, 
                                        method = c("logistic_IG10"))
summary(phylogenetic_logistic_model)
