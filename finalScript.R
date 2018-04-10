#Remember to set working directory to /DATA421_LightningNetwork/datafiles
## The R version used for this project was R version 3.4.4 aka "Someone to Lean On" 
##      released March 15 2018
install.packages("memoise")
install.packages("sqldf")
install.packages("igraph")
install.packages("network")
install.packages("visNetwork")
install.packages("networkD3")

library(memoise)
library(sqldf)
library(igraph)
library(network)
library(visNetwork)
library(networkD3)

nodes <- read.csv("nodesWithLoc.csv", header = TRUE)

colnames(nodes)


#Here is a sample view of the data;
head(nodes)

summary(nodes)


channels <- read.csv("channelsR.csv", header = TRUE)

sourceC <- sqldf(
  "SELECT *
   FROM channels, nodes
   WHERE nodes.NodeID = channels.source"
)

colnames(sourceC)[5] <- "alias.S"
colnames(sourceC)[6] <- "ip.S"
colnames(sourceC)[7] <- "continent.S"
colnames(sourceC)[8] <- "country.S"
colnames(sourceC)[9] <- "region"
colnames(sourceC)[10] <- "city.S"
colnames(sourceC)[11] <- "Lat.S"
colnames(sourceC)[12] <- "Lng.S"

destinationC <- sqldf(
  "SELECT *
   FROM channels, nodes
   WHERE nodes.NodeID = channels.destination"
)

colnames(destinationC)[5] <- "alias.D"
colnames(destinationC)[6] <- "ip.D"
colnames(destinationC)[7] <- "continent.D"
colnames(destinationC)[8] <- "country.D"
colnames(destinationC)[9] <- "region.D"
colnames(destinationC)[10] <- "city.D"
colnames(destinationC)[11] <- "Lat.S"
colnames(destinationC)[12] <- "Lng.s"

final <- sqldf(
  "SELECT *
   FROM sourceC, destinationC
   WHERE sourceC.source = destinationC.source AND 
  sourceC.destination = destinationC.destination AND
  sourceC.weight = destinationC.weight"
)

#file included in deliverables, not required to run
write.csv(final, file = "final.csv")


channelsR <- read.csv("newChanneldata.csv", header = TRUE)

edges <- channelsR
colnames(edges)[2] <- "target"

net <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
net


#Simply plotting the graph
plot(net)

#Making the graph a bit better
plot(net, vertex.label = NA, vertex.size = 5, edge.color = "white", 
     edge.lty = 0, axes = TRUE)

#The graph displays the simple graph of the network showing the connections
# from one node to another node. 
plot(net, layout = layout_randomly, vertex.label = NA, vertex.size = 5, 
     edge.lty = 1, edge.color = "brown", axes = TRUE, edge.arrow.mode = 0, 
     frame = TRUE, main = "Simple graph")


#Let's try a circle formatted network
l <- layout_in_circle(net)
plot(net, layout = l, vertex.size = 1, edge.lty = 1, edge.color = "brown", 
     vertex.color = "blue", axes = TRUE, edge.arrow.mode = 0, frame = TRUE, 
     main = "Circle Graph", vertex.label = NA)


#Trying a sphere formatted network
l2 <- layout_on_sphere(net)
plot(net, layout = l2, vertex.label = NA, vertex.size = 5, edge.lty = 1, 
     edge.color = "brown", vertex.color = "blue", axes = TRUE, 
     edge.arrow.mode = 0, frame = TRUE, main = "Sphere Graph")



#Plotting with Fruchterman-Reingold algorithm
l3 <- layout_with_fr(net)
plot(net, layout = l3, vertex.label = NA)

#Plotting with an actual GUI (opens in new window),
## more expandability upon graphs and their data
tkid <- tkplot(net, vertex.label = NA)
plot(tkid)



#simple graph using visNetwork package
edges = read.csv("newChanneldata.csv", header = TRUE)
nodes = read.csv("nodesWithLoc.csv", header = TRUE)

colnames(edges)[1] <- "Source"
colnames(edges)[2] <- "Target"

df1 <- sqldf("SELECT *
             FROM edges
             INNER JOIN nodes
             ON edges.Source = nodes.NodeID")

colnames(df1)[1] <- "Source"
colnames(df1)[5] <- "SourceAlias"


colnames(df1)[2] <- "nodeid"

colnames(df1)[4] <- "placeholder"
colnames(df1)[5] <- "SourceAlias"

df2 <- sqldf("SELECT *
             FROM df1
             INNER JOIN nodes
             on df1.nodeid = nodes.nodeid")

colnames(df2)[2] <- "Target"
colnames(df2)[7] <- "TargetAlias"

columnsRemain <- c("Source", "Target", "weight", "SourceAlias", "TargetAlias")
df2 <- df2[columnsRemain]

g = graph.data.frame(df2, directed = TRUE)

visIgraph(g)


#Degrees, coefficients, centrality, etc
edges = read.csv("newChanneldata.csv", header = TRUE)
nodes = read.csv("nodesWithLoc.csv", header = TRUE)

colnames(edges)
colnames(edges)[1] <- "Source"
colnames(edges)[2] <- "Target"
colnames(edges)

colnames(nodes)
colToRemain <- c("NodeID", "alias")
nodes = nodes[colToRemain]

colnames(nodes)[1] <- "NodeID"
colnames(nodes)[2] <- "Alias"
colnames(nodes)

df1 <- sqldf("SELECT *
             FROM edges
             INNER JOIN nodes
             ON edges.Source = nodes.NodeID")
colnames(df1)
colnames(df1)[5] <- "SourceAlias"
colnames(df1)

colnames(edges)
colnames(nodes)

edges$weight <- edges[, 3] + 1

netGraph <- graph_from_data_frame(d = edges, 
                                  vertices = nodes, directed = T)
netGraph

#Betweenness calculation
betweenness(netGraph, v = V(netGraph), directed = TRUE, 
            weights = E(netGraph)$weight,
            nobigint = TRUE, normalized = FALSE)

#Edge-Betweenness calculation
edge_betweenness(netGraph, e = E(netGraph), directed = TRUE, 
                 weights =  E(netGraph)$weight)

#Closeness centrality
closeness(netGraph, vids = V(netGraph), mode = ("total"),
          weights = E(netGraph)$weight, normalized = FALSE)

#Degree of a vertex
degree(netGraph, v = V(netGraph), mode = c( "all"), 
       loops = FALSE, normalized = FALSE)

# Clustering coefficient (transitivity of a graph) 
transitivity(netGraph, type = ( "global"), vids = V(netGraph),
             weights = E(netGraph)$weight, isolates = c("NaN", "zero"))
