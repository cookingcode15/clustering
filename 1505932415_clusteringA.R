# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))
install.packages(c("cluster", "rattle.data","NbClust"))
# set working directory

#load libraries
library=("cluster")
library=("rattle.data")
lbrary=("NbClust")
# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
df <- scale(wine[-1]) 
df
# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df)

# Exercise 2:
#   * How many clusters does this method suggest? 3
#   * Why does this method work? What's the intuition behind it? 
# That is where the 'bend' is located on the plot
#   * Look at the code for wssplot() and figure out how it works

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")



# Exercise 3: How many clusters does this method suggest?
## 3 clusters


# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km
#first set seed
set.seed(1234)
#then output result calling kmeans function with fit.km as the variable
# fit.km <- kmeans( ... )
fit.km <- kmeans(df, 3, nstart=25)  




# Now we want to evaluate how well this clustering does.
fit.km$size

fit.km$centers  

aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
ct.km <- table(wine$Type, fit.km$cluster)
ct.km  
plot(fit.km$cluster)

#each cluster is balanced and the centroid (or mean is obvious to compare in groups)
install.packages("flexclust")
install.packages("grid")
install.packages("lattice")
install.packages("modeltools")
install.packages("stats54")
library("flexclust")
library("modeltools")
library("lattice")

randIndex(ct.km)

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
# Yes because the variance within clusters is not high and the 3 groups are visually similar in attributes
install.packages(cluster)
library(cluster)
# k-means 
k.means.fit <- kmeans(df, 3)
k.means.fit
#attributes
attributes(k.means.fit)

#size fit
k.means.fit$size

#clusplot( ... ) for 2D representation

library(cluster)

clusplot(df, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
# this builds a confusion matrix
table(wine[,1],k.means.fit$cluster)


