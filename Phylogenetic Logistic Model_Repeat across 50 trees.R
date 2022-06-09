# Load the bird diet and SS data.
jetz_traits <- read.csv("C:/Users/13988/Desktop/FinalDataNoSONoUncer.csv", header = TRUE)
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


# Read in the 50 random tree.
random_tree <- read.tree("C:/Users/13988/Desktop/Final Project/dichromatism_trees.tre")
# Return the object.
random_tree
# Get the structure of the object.
str(random_tree)


# Use for loop to repeat the Phylogenetic Logistic Model across 50 random trees. Using all data
# Creates an empty list to store 
slist = list()
alist = list()
i=0
for(r_tree in random_tree){
  pruned_birdtree <- drop.tip(r_tree, setdiff(r_tree$tip.label, jetz_traits$Jetz_Name))
  # str(pruned_birdtree)
  # Make a covariance matrix.
  model_covar <- vcv.phylo(pruned_birdtree)
  # Make sure the covariance matrix and data are in the same order.
  row.names(jetz_traits) <- jetz_traits$Jetz_Name
  jetz_traits <- jetz_traits[row.names(model_covar),]
  # Run the model.
  phylogenetic_logistic_model <- phyloglm(New_score ~ Trophic_level*abs_latitude_scaled, data=jetz_traits, phy=pruned_birdtree, 
                                          method = c("logistic_IG10"))
  # Extract the coeffcients from each iteration from the model
  sum_data <- summary(phylogenetic_logistic_model)
  # sum_data$coefficients
  # Store the results of everytime the loop repeats
  i = i+1
  slist[[i]] <-sum_data$coefficients
  alist[[i]] <-sum_data$alpha
  # print(slist)
}

# Average the model coefficients after repeating our analysis across 50 random trees
Reduce("+",slist)/length(slist)
Reduce("+",alist)/length(alist)
