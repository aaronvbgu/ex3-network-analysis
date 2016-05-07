## Load package
library(igraph)

#### Part 1 ####

## Grey's Anatomy Network of Sexual Relations
ga.data <- read.csv('ga_edgelist.csv', header=TRUE)
g <- graph.data.frame(ga.data, directed=FALSE)

# get vertexes values
V(g)$Closeness = closeness(g)
V(g)$Betweenness = betweenness(g)
V(g)$Eigenvector = evcent(g)$vector

# get max value
maxBetweenness <- V(g)$name[which(V(g)$Betweenness == max(betweenness(g)))]
maxCloseness <- V(g)$name[which(V(g)$Closeness == max(closeness(g)))]
maxEigenvector <- V(g)$name[which(V(g)$Eigenvector == max(evcent(g)$vector))] 

# print
cat("Max Betweenness belongs to: ", maxBetweenness  , ", value: ", max(betweenness(g)))
cat("Max closeness belongs to: ", maxCloseness, ", value: ", max(closeness(g)))
cat("Max Eigenvector belongs to: ", maxEigenvector  , ", value: ", max(evcent(g)$vector))

# graph properties
g <- simplify(g)
set.seed(100)
lay <- layout.kamada.kawai(g)

# community strucure via short random walks
cat("Short Random Walks:")
fc <- walktrap.community(g)
cat("Modularity: ", modularity(fc))
cat("Sizes: ", sizes(fc))

# show graph
V(g)$label <- NA
V(g)$color <- fc$membership
V(g)$size <- 5
plot(fc, g)

# Girvan-Newman community detection algorithm
cat("Girvan-Newman:")
fc <-  edge.betweenness.community(g)
cat("Modularity: ", modularity(fc))
cat("Sizes: ", sizes(fc))

# show graph
V(g)$label <- NA
V(g)$color <- fc$membership
V(g)$size <- 5
plot(fc, g)

#### Part 2 ####

## Grey's Anatomy Network of Sexual Relations
ga.data <- read.csv('ga_edgelist.csv', header=TRUE)
g <- graph.data.frame(ga.data, directed=FALSE)

# get vertexes values
V(g)$Closeness = closeness(g)
V(g)$Betweenness = betweenness(g)
V(g)$Eigenvector = evcent(g)$vector

# get max value
maxBetweenness <- V(g)$name[which(V(g)$Betweenness == max(betweenness(g)))]
maxCloseness <- V(g)$name[which(V(g)$Closeness == max(closeness(g)))]
maxEigenvector <- V(g)$name[which(V(g)$Eigenvector == max(evcent(g)$vector))] 

# print
cat("Max Betweenness belongs to: ", maxBetweenness  , ", value: ", max(betweenness(g)))
cat("Max closeness belongs to: ", maxCloseness, ", value: ", max(closeness(g)))
cat("Max Eigenvector belongs to: ", maxEigenvector  , ", value: ", max(evcent(g)$vector))

# graph properties
g <- simplify(g)
set.seed(100)
lay <- layout.kamada.kawai(g)

# community strucure via short random walks
cat("Short Random Walks:")
fc <- walktrap.community(g)
cat("Modularity: ", modularity(fc))
cat("Sizes: ", sizes(fc))

# show graph
V(g)$label <- NA
V(g)$color <- fc$membership
V(g)$size <- 5
plot(fc, g)

# Girvan-Newman community detection algorithm
cat("Girvan-Newman:")
fc <-  edge.betweenness.community(g)
cat("Modularity: ", modularity(fc))
cat("Sizes: ", sizes(fc))

# show graph
V(g)$label <- NA
V(g)$color <- fc$membership
V(g)$size <- 5
plot(fc, g)
