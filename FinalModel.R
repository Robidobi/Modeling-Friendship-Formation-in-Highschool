library(statnet)
library(intergraph)

# ========= 1. SETTING UP DATA ===========

# read in the edge list from our github
edgelist <-  read.csv("Edges_11thGradeSNA_2.csv", stringsAsFactors = F)
attributes <- read.csv("UpdatedNodes3.csv", stringsAsFactors = F) 

# Indexing data so that you only put in certain columns
el_no_weight <- edgelist[,1:2] # We will ignore the ranking variable for now.
el_no_weight <- as.matrix(el_no_weight) # igraph requires a matrix

# convert ids to characters so they are preserved as names
el_no_weight[,1] <- as.character(el_no_weight[,1])
el_no_weight[,2] <- as.character(el_no_weight[,2])

# ========= 2. CREATING NETWORK ===========
library(igraph)

#Cleaning Data
respondents <- unique(edgelist[,1]) 
attributes$surveyed <- ifelse(attributes$Id %in% respondents, 1, 0)

# Replace NA in Race with "Unknown"
attributes$Race.Origin[is.na(attributes$Race.Origin)] <- "Unknown"
# Replace NA in Sex with a placeholder
attributes$Sex[is.na(attributes$Sex)] <- 99


#Creating Network
set.seed(1234)
net <- graph_from_data_frame(edgelist, directed = TRUE, vertices = attributes)
statnet <- asNetwork(net)



# ========= 3. RUNNING MODEL ===========

#Model 8: The Final Model
model_final <- ergm(statnet ~ edges + 
                      mutual+
                      gwesp(0.1, fixed = TRUE) +
                      nodematch("Sex") + 
                      nodematch("Years.at.School") + 
                      nodematch("Home.Language")+
                      nodematch("Soccer") +
                      nodematch("Hogar.Ortigosa")+
                      nodematch("Race.Origin", diff = TRUE, keep = c(1,2,3,5)),
                    constraints = ~bd(maxout = 10), # Telling the model about the 10-friend limit
                    control = control.ergm(MCMC.samplesize = 2000, 
                                           MCMC.interval = 2000,
                                           # This helps with those 'numerical instability' errors
                                           CD.nsteps = 10)
                    
)
summary(model_final)

mcmc.diagnostics(model_final)
# Define the grid (2 rows, 2 columns)
par(mfrow = c(2, 3))
# Run the GOF on your best model
gof_results <- gof(model_final)
# Plot the results to see the comparison
plot(gof_results)
#Finding probabilites
exp(coef(model_final))
 




