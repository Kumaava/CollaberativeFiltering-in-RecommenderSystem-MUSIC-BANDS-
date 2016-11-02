
data.germany <- read.csv(file="lastfm-matrix-germany.csv")

# Drop any column named "user"
data.germany.ibs <- (data.germany[,!(names(data.germany) %in% c("user"))])

#Create a helper function to calculate the cosine between two vectors
getCosine <- function(x,y) 
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}
# Create a placeholder dataframe listing item vs. item
holder <- matrix(NA, nrow=ncol(data.germany.ibs),ncol=ncol(data.germany.ibs),dimnames=list(colnames(data.germany.ibs),colnames(data.germany.ibs)))
data.germany.ibs.similarity <- as.data.frame(holder)

# Lets fill in those empty spaces with cosine similarities                                                                     
for(i in 1:ncol(data.germany.ibs)) {
  # Loop through the columns for each column
  for(j in 1:ncol(data.germany.ibs)) {
    # Fill in placeholder with cosine similarities
    data.germany.ibs.similarity[i,j] <- getCosine(as.matrix(data.germany.ibs[i]),as.matrix(data.germany.ibs[j]))
  }
}

# Output similarity results to a file
write.csv(data.germany.ibs.similarity,file="final-germany-similarity.csv")


# Get the top 10 neighbours for each
data.germany.neighbours <- matrix(NA, nrow=ncol(data.germany.ibs.similarity),ncol=11,dimnames=list(colnames(data.germany.ibs.similarity)))
for(i in 1:ncol(data.germany.ibs)) 
{
  data.germany.neighbours[i,] <- (t(head(n=11,rownames(data.germany.ibs.similarity[order(data.germany.ibs.similarity[,i],decreasing=TRUE),][i]))))
}

# Output neighbour results to a file  
write.csv(file="final-germany-item-neighbours.csv",x=data.germany.neighbours[,-1])
