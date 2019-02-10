# The MIT License (MIT) 
# 
# Copyright (c) 2019 Georg Unterholzner
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy 
# of this software and associated documentation files (the "Software"), to deal 
# in the Software without restriction, including without limitation the rights 
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
# copies of the Software, and to permit persons to whom the Software is 
# furnished to do so, subject to the following conditions: 
#  
# The above copyright notice and this permission notice shall be included in all 
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
# SOFTWARE. 


# Calculate the euclidian distance to a specific centroid
calculate_euclidian_distance <- function(centroid, data_row) {
  return( sqrt(sum((data_row - centroid)^2)) )
}


# Calculate the euclidian distances to all centroids for a specific row
calculate_distance_for_row <- function(data_row, centroids) {
  return( apply(centroids, 1, calculate_euclidian_distance, data_row) ) 
}


# For each row, get the id of the nearest cluster
calculate_cluster_association <- function(data, centroids) {
  distances <- t(apply(data, 1, calculate_distance_for_row, centroids))
  return( apply(distances, 1, which.min) )
}


# Calculate the new centroids for all clusters
calculate_new_centroids <- function(data, cluster_associations) {
  new_centroids <- matrix(nrow=0, ncol=dim(data)[2])
  for(cluster_id in unique(cluster_associations)) {
    new_centroids <- rbind(new_centroids, colMeans(data[which(cluster_associations == cluster_id), ]))
  }
  return(new_centroids)
}


# K-Means function
k_means <- function(data, k, iterations) {
  # Initialize random centroids (Forgy Partition)
  centroids <- matrix(data[sample(k),], k, dim(data)[2])
  
  # 1. Assign nearest centroid to each observation
  # 2. Calculate the new centroids of each cluster
  # 3. Repeat step 1-2 until number of iterations is reached
  for(i in 1:iterations) {
    centroids <- calculate_new_centroids(data, 
                                         calculate_cluster_association(data, centroids))
  }
  
  # Calculate final cluster associations
  cluster_associations <- calculate_cluster_association(data, centroids)
  
  # Return named list
  return(
    list(
      centroids = centroids,
      cluster = cluster_associations
    )
  )
}
