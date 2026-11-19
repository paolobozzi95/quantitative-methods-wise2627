

library(tidyverse)

#install.packages("tibble")
library(statnet) 

#install package to read rda file
library(foreign)



### Let's create our own network ###

#use a matrix
df <- rbind( c(3, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1),
             c(0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 1),
             c(1, 0, 4, 0, 0, 1, 0, 0, 0, 0, 1),
             c(0, 0, 0, 0, 3, 4, 0, 0, 0, 0, 0),
             c(0, 0, 0, 0, 4, 6, 0, 0, 0, 0, 0),
             c(0, 0, 1, 0, 0, 6, 0, 0, 0, 2, 2),
             c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
             c(0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0),
             c(0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0),
             c(1, 0, 1, 1, 0, 0, 2, 0, 0, 4, 2),
             c(1, 1, 1, 0, 0, 1, 0, 0, 0, 2, 7))

rownames(df) <- c("Robert","Mamie","David","Nicolina","Elisabeth", "Luca",
                  "William", "Kimi", "Jesse", "Anastasiia", "Ilaha")

colnames(df) <- c("Robert","Mamie","David","Nicolina","Elisabeth", "Luca",
                  "William", "Kimi", "Jesse", "Anastasiia", "Ilaha")

#Convert to network
df_net <- network(df,matrix.type="adjacency",
                  directed=FALSE) #it makes sure connections take place in both directions

summary(df_net)

set.vertex.attribute(df_net, "programme", 
                     c("MA", "MA", "MA", "BA", "BA", "MA", "MA", "BA",
                       "MA", "MA", "MA"))

programme <- get.vertex.attribute(df_net, "programme")

vertex_colors <- ifelse(programme == "BA", "blue", "red")


# I can do the same using an edge list


# Edge list as pairs: c(from, to)
el <- rbind(c( 1,  3),
            c( 1, 10),
            c( 1, 11),
            c( 2, 11),
            c( 3,  1),
            c( 3,  6),
            c( 3, 11),
            c( 4,  5),
            c( 4,  6),
            c( 5,  6),
            c( 6,  3),
            c( 6, 10),
            c( 6, 11),
            c(10,  1),
            c(10,  3),
            c(10,  4),
            c(10,  7),
            c(10, 11),
            c(11,  1),
            c(11,  2),
            c(11,  3),
            c(11,  6),
            c(11, 10))

# Tie strength, in the same order as the pairs
w <- c(1, 1, 1, 1, 1, 1, 1, 3, 4, 6, 1, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2)

# Build the network: 11 nodes, so 8 and 9 stay in as isolates
n <- network.initialize(11, directed = TRUE)
add.edges(n, tail = el[, 1], head = el[, 2])
n %e% "weight" <- w

n %v% "diag_value" <- c(3, 3, 4, 0, 4, 6, 1, 2, 3, 4, 7)

set.seed(42)
plot(n,
     displaylabels = TRUE,
     label.pos     = 5,
     vertex.col    = "steelblue",
     vertex.cex    = 1 + (n %v% "diag_value") * 0.3,
     edge.lwd      = n %e% "weight",
     arrowhead.cex = 1.2)










#plot according to attributes
gplot(df_net,
      gmode = "graph",
      vertex.col = vertex_colors,
      vertex.cex = 1.4,
      edge.lwd = 2,
      edge.col = "grey75",
      edge.lty = 1,
      mode = "fruchtermanreingold",
      displaylabels = TRUE)

#different sizes for ties

edge_lwd <- get.edge.value(df_net, "na")  # default edge values are missing
# So instead, get the matrix directly:
edge_lwd <- df[as.matrix(as.edgelist(df_net))]

gplot(df_net,
      gmode = "graph",
      vertex.col = vertex_colors,
      vertex.cex = 1.4,
      edge.lwd = edge_lwd,  # Use actual weights
      edge.col = "grey75",
      edge.lty = 1,
      mode = "fruchtermanreingold",
      displaylabels = TRUE)


#kamadakawai
#fruchtermanreingold
#spring
#circle




#visualize the data
#View(Moreno)


#load .rda file
load("C:/Users/paolo/Downloads/Moreno.rda")

summary(Moreno, print.adj=TRUE)


plot(Moreno, vertex.col = "blue",
     vertex.cex = 2, edge.lwd = 2) #this is just aesthetics

#metrics

#size
network.size(Moreno)

#density
network.density(Moreno)

#components
components(Moreno)

#diameter
lgc <- component.largest(Moreno,result="graph") 

gd <- geodist(lgc) 

max(gd$gdist)

#Clustering coefficient: transitivity
gtrans(Moreno, mode = "graph")


