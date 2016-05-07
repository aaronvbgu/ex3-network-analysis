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


library(igraph)
library("XML")
require(reshape2)

#### Part 2 ####

## Imdb top movies

movies <- c("tt0111161", # The Shawshank Redemption
            "tt1853728", # Django Unchained
            "tt2820852", # Furious Seven
            "tt0405159", # Million Dollar Baby
            "tt2637276", # Ted 2
            "tt1483013", # Oblivion
            "tt1905041", # Furious 6
            "tt1013752", # Fast & Furious
            "tt0463985", # The Fast and the Furious: Tokyo Drift
            "tt0110912", # Pulp Fiction
            "tt0361748", # Inglourious Basterds
            "tt0993846"  # The Wolf of Wall Street
)

top_movies_data <- data.frame(matrix(NA, nrow=0, ncol=2))
colnames(top_movies_data) <- c("Movie", "Cast")

# crawling information from imdb
for (i in 1:length(movies)) {
  
  ID <- movies[i]
  URL <- paste0("http://www.imdb.com/title/", ID)
  parsed.html <- htmlParse(URL)
  Film <- xpathSApply(parsed.html, "//h1[@class='header']/span[@class='itemprop']", xmlValue)
  if (is.null(Film)) 
    Film =  xpathSApply(parsed.html, "//h1[@itemprop='name']", xmlValue)
  parsed.html <- htmlParse(paste0(URL,"/fullcredits"))
  Full_Cast <- xpathSApply(parsed.html, "//span[@itemprop='name']", xmlValue)
  TopFiveCast <- head(Full_Cast, 5)
  melt(TopFiveCast)
  movie_data <- data.frame(Film = Film, TopFiveCast = TopFiveCast)
  
  # bind all movie data to one dataset
  top_movies_data <- rbind(top_movies_data, movie_data) 
}

initial_g <- graph.data.frame(top_movies_data)
# top_movies_data is the data frame with combined data of all comedy movies
V(initial_g)$type <- bipartite.mapping(initial_g)$type
g <- bipartite.projection(initial_g, which="true")
g <- simplify(g)

# get vertexes values
V(g)$Closeness = closeness(g)
V(g)$Betweenness = betweenness(g)
V(g)$Eigenvector = evcent(g)$vector
V(g)$Eigenvector

# get max value
maxBetweenness <- V(g)$name[which(V(g)$Betweenness == max(betweenness(g)))]
maxCloseness <- V(g)$name[which(V(g)$Closeness == max(closeness(g)))]
maxEigenvector <- V(g)$name[which(V(g)$Eigenvector == max(evcent(g)$vector))] 
maxEigenvector

# print
cat("Max Betweenness belongs to: ", maxBetweenness  , ", value: ", max(betweenness(g)))
cat("Max closeness belongs to: ", maxCloseness, ", value: ", max(closeness(g)))
cat("Max Eigenvector belongs to: ", maxEigenvector  , ", value: ", max(evcent(g)$vector))

# graph properties
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

# community structure via greedy optimization of modularity
cat("Greedy Optimization:")
fc <-  fastgreedy.community(g)
cat("Modularity: ", modularity(fc))
cat("Sizes: ", sizes(fc))

# show graph
V(g)$label <- NA
V(g)$color <- fc$membership
V(g)$size <- 5
plot(fc, g)
