library(jsonlite)
library(igraph)

d <- fromJSON("train.json")
d <- d[lapply(d$ingredients, length) >= 2 , ] # I only care about recipes with at least two ingredients
#head(d)

d$ingredients[1] # take a peek at the data

#universe.of.ingredients <- sort(unique(unlist(d$ingredients)))

# Need to scan through each recipe, create all the pairs of ingredients, set appropriate weights, and combine into one giant matrix
d$pairs <- lapply(d$ingredients, simplify = T, combn, 2)

# Initialize then populate the matrix of pairwise ingredient combinations
relations.w.weight <- t(  rbind(d$pairs[[1]] , 1/dim(d$pairs[[1]])[2])  )
for (recipe.num in 2:10) { #dim(d)[1] ultimately i'll include every recipe (i think), but while developing, a small subset is sufficient
 
   new.relations <- t(  rbind(d$pairs[[recipe.num]] , 1/dim(d$pairs[[recipe.num]])[2])  )
  
   relations.w.weight <- rbind(relations.w.weight, new.relations)
   
}

relations.df <- data.frame(relations.w.weight, stringsAsFactors = FALSE)
names(relations.df) <- c("Ing1", "Ing2", "Weight") # helpful names are...helpful
relations.df$Weight <- as.numeric(relations.df$Weight)

# Create the adjacency matrix
#adj_matrix <- as.matrix(as_adjacency_matrix(graph_from_data_frame(relations.df), attr = "Weight"))
adj_matrix <- as.matrix(as_adjacency_matrix(graph_from_data_frame(relations.df)))

#g <- graph.adjacency(as.matrix(adj_matrix),mode="undirected",weighted="Weight",diag=FALSE)
#plot.igraph(g) #, vertex.color = s+ 18)

# Then apply the community identification algorithm

# define a few functions for later use
norm <- function(z)
{
  return (sqrt(sum(z^2)))
}

power.iter <- function(a, x, k){
  
  for (i in 1:k){
    
    u <- as.matrix(x/norm(x))
    x <- a%*%u
    lam <- t(u)%*%a%*%u
    
  }
  
  u <- x/norm(x)
  
  return(u)
  
}

Find.S <- function(elements.to.subset = 1:dim(B)[1]) {
  
  new.B <- B[elements.to.subset, elements.to.subset]
  dominant_u <-  power.iter(new.B, rep(1,nrow(new.B)), 100)
  s <- dominant_u/abs(dominant_u)
  s[is.nan(s)] <- 1 # temp fix
  
  return(s)

}

# identify the key statistics for the network

K <- rowSums(adj_matrix)
kk_m <- (K%*%t(K))/sum(K)
B <- adj_matrix-kk_m

Recur.Finding.S.Rounds <- function(num.rounds) { # WIP
  return(1)
}

s1 <- Find.S()

s2 <- rbind(Find.S(s1==1)
          , Find.S(s1 == -1))

s3 <- rbind(Find.S(s1==1 & s2==1), Find.S(s1 == -1 & s2==1)
          , Find.S(s1==1 & s2==-1), Find.S(s1 == -1 & s2==-1))

s4 <- rbind(Find.S(s1==1 & s2==1 & s3 ==1 ), Find.S(s1==1 & s2==1 & s3 == -1 )
          , Find.S(s1 == -1 & s2==1 & s3 ==1 ), Find.S(s1 == -1 & s2==1 & s3 == -1)
          , Find.S(s1==1 & s2==-1 & s3 ==1 ) , Find.S(s1==1 & s2==-1 & s3 == -1 )
          , Find.S(s1 == -1 & s2==-1 & s3 ==1 ) , Find.S(s1 == -1 & s2==-1 & s3 == -1 ))

results <- data.frame(s1,s2,s3,s4)
results$group_id <- paste(s1,s2,s3,s4)

