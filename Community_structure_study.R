getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <- "/Users/prayash/Downloads"
setwd(dir_path)
# clear everything out of memory
rm(list=ls())  

# Load primary school data, contact data
infile_edges<-"Edges_sp_data_school_day_2.csv"
infile_nodes<-"Nodes_sp_data_school_day_2.csv"

## Load package
library(igraph)
edge_frame=read.csv(infile_edges, header = TRUE, sep = ",")
node_frame=read.csv(infile_nodes, header = TRUE, sep = ",")

g_primschool=graph.data.frame(edge_frame, directed = FALSE, vertices= node_frame)

# Edges
ecount(g_primschool)
## Vertices
vcount(g_primschool)
is.weighted(g_primschool)

V(g_primschool)$name
E(g_primschool)$weight
V(g_primschool)$gender
V(g_primschool)[V(g_primschool)$classname=="1B"]

is.simple(g_primschool)
is.connected(g_primschool)

# http://igraph.wikidot.com/community-detection-in-r
# "The following code snippet performs a Wilcoxon rank-sum test on the "internal" and "external"
# degrees of a community in order to quantify its significance. Let us call the edges within a 
# community "internal" and the edges connecting the vertices of a community with the rest of the graph "external".
# The null hypothesis of the test is that there is no difference between the number of "internal" and "external" edges 
# incident to a vertex of the community. More internal than external edges show that the community is significant; less 
# internal than external edges show that the community is in fact an "anti-community". The p-value of the test performed by 
# this function will be close to zero in both cases; the value of the test statistic tells us whether we have a community or an anti-community."
community.significance.test <- function(graph, vs, ...) {
  if (is.directed(graph)) stop("This method requires an undirected graph")
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  # Total degree among nodes in the vs list, minus the degree within the subgraph 
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}

stud.class <- get.vertex.attribute(g_primschool, "classname")
stud.gender<- get.vertex.attribute(g_primschool, "gender")
# Does edge weight make any difference here?

# Community detection using the Fast Greedy Algorithm
school_comm_fast <- fastgreedy.community(g_primschool, weights=E(g_primschool)$weight)
c.m <- membership(school_comm_fast)
# Assignment to communities, based on class section or teacher status. This analysis can be extended to gender (see below).
table(c.m, stud.class, useNA = c("no"))

# Here, we are testing community significance for just two of the communities. Students will complete tests for the remainder of communities for each algorithm. 
v_comp1 <- V(g_primschool)[c.m==1]
v_comp2 <- V(g_primschool)[c.m==2]
v_comp3 <- V(g_primschool)[c.m==3]
v_comp4 <- V(g_primschool)[c.m==4]
v_comp5 <- V(g_primschool)[c.m==5]
v_comp6 <- V(g_primschool)[c.m==6]
v_comp7 <- V(g_primschool)[c.m==7]
community.significance.test(g_primschool, v_comp1)
community.significance.test(g_primschool, v_comp2)
community.significance.test(g_primschool, v_comp3)
community.significance.test(g_primschool, v_comp4)
community.significance.test(g_primschool, v_comp5)
community.significance.test(g_primschool, v_comp6)
community.significance.test(g_primschool, v_comp7)

# Community detection using the Walktrap Algorithm
school_comm_walk <- walktrap.community(g_primschool, weights=E(g_primschool)$weight)
c.m <- membership(school_comm_walk)
# Assignment to communities, based on class section or teacher status. This analysis can be extended to gender (see below).
table(c.m, stud.class, useNA = c("no"))

# Here, we are testing community significance for Walktrap algorithm
v_comp1 <- V(g_primschool)[c.m==1]
v_comp2 <- V(g_primschool)[c.m==2]
v_comp3 <- V(g_primschool)[c.m==3]
v_comp4 <- V(g_primschool)[c.m==4]
v_comp5 <- V(g_primschool)[c.m==5]
v_comp6 <- V(g_primschool)[c.m==6]
v_comp7 <- V(g_primschool)[c.m==7]
v_comp8 <- V(g_primschool)[c.m==8]
v_comp9 <- V(g_primschool)[c.m==9]
community.significance.test(g_primschool, v_comp1)
community.significance.test(g_primschool, v_comp2)
community.significance.test(g_primschool, v_comp3)
community.significance.test(g_primschool, v_comp4)
community.significance.test(g_primschool, v_comp5)
community.significance.test(g_primschool, v_comp6)
community.significance.test(g_primschool, v_comp7)
community.significance.test(g_primschool, v_comp8)
community.significance.test(g_primschool, v_comp9)

# Community detection using the Spinglass Algorithm
school_comm_sping <- spinglass.community(g_primschool, weights=E(g_primschool)$weight)
c.m <- membership(school_comm_sping)
# Assignment to communities, based on class section or teacher status. This analysis can be extended to gender (see below).
table(c.m, stud.class, useNA = c("no"))

# Here, we are testing community significance for Spinglass Algorithm
v_comp1 <- V(g_primschool)[c.m==1]
v_comp2 <- V(g_primschool)[c.m==2]
v_comp3 <- V(g_primschool)[c.m==3]
v_comp4 <- V(g_primschool)[c.m==4]
v_comp5 <- V(g_primschool)[c.m==5]
v_comp6 <- V(g_primschool)[c.m==6]
v_comp7 <- V(g_primschool)[c.m==7]
v_comp8 <- V(g_primschool)[c.m==8]
v_comp9 <- V(g_primschool)[c.m==9]
v_comp10 <- V(g_primschool)[c.m==10]
v_comp11 <- V(g_primschool)[c.m==11]
community.significance.test(g_primschool, v_comp1)
community.significance.test(g_primschool, v_comp2)
community.significance.test(g_primschool, v_comp3)
community.significance.test(g_primschool, v_comp4)
community.significance.test(g_primschool, v_comp5)
community.significance.test(g_primschool, v_comp6)
community.significance.test(g_primschool, v_comp7)
community.significance.test(g_primschool, v_comp8)
community.significance.test(g_primschool, v_comp9)
community.significance.test(g_primschool, v_comp10)
community.significance.test(g_primschool, v_comp11)


# Community detection using the Label Propagation Algorithm
school_comm_label <- label.propagation.community(g_primschool, weights=E(g_primschool)$weight)
c.m <- membership(school_comm_label)
# Assignment to communities, based on class section or teacher status. This analysis can be extended to gender (see below).
table(c.m, stud.class, useNA = c("no"))

# Here, we are testing community significance for Label Propagation Algorithm
v_comp1 <- V(g_primschool)[c.m==1]
v_comp2 <- V(g_primschool)[c.m==2]
v_comp3 <- V(g_primschool)[c.m==3]
v_comp4 <- V(g_primschool)[c.m==4]
v_comp5 <- V(g_primschool)[c.m==5]
v_comp6 <- V(g_primschool)[c.m==6]
v_comp7 <- V(g_primschool)[c.m==7]
v_comp8 <- V(g_primschool)[c.m==8]
v_comp9 <- V(g_primschool)[c.m==9]
v_comp10 <- V(g_primschool)[c.m==10]
v_comp11 <- V(g_primschool)[c.m==11]
v_comp12 <- V(g_primschool)[c.m==12]
community.significance.test(g_primschool, v_comp1)
community.significance.test(g_primschool, v_comp2)
community.significance.test(g_primschool, v_comp3)
community.significance.test(g_primschool, v_comp4)
community.significance.test(g_primschool, v_comp5)
community.significance.test(g_primschool, v_comp6)
community.significance.test(g_primschool, v_comp7)
community.significance.test(g_primschool, v_comp8)
community.significance.test(g_primschool, v_comp9)
community.significance.test(g_primschool, v_comp10)
community.significance.test(g_primschool, v_comp11)
community.significance.test(g_primschool, v_comp12)


# Students will produce similar plots for the walktrap, spinglass, and label propagation algorithms for community detection
plot(school_comm_fast,g_primschool, vertex.label= NA, vertex.size=2)
plot(school_comm_walk,g_primschool, vertex.label= NA, vertex.size=2)
plot(school_comm_sping,g_primschool, vertex.label= NA, vertex.size=2)
plot(school_comm_label,g_primschool, vertex.label= NA, vertex.size=2)

# Community detection using the Girvan-Newman Algorithm
school_comm_girvan <- edge.betweenness.community(g_primschool, weights=E(g_primschool)$weight)
c.m <- membership(school_comm_girvan)
# Assignment to communities, based on class section or teacher status. This analysis can be extended to gender (see below).
table(c.m, stud.class, useNA = c("no"))

#We see that there are a lot of communities to do significance test

# Consider students in first grade and 5th grade. To what extent does community structure indicate that students segregate by gender in these two grades?
# Use the Fast Greedy algorithm for analysis.

v_grade1students<-V(g_primschool)[V(g_primschool)$classname=="1B" | V(g_primschool)$classname=="1A"]
v_grade5students<-V(g_primschool)[V(g_primschool)$classname=="5B" | V(g_primschool)$classname=="5A"]

subgraph_grade1<-induced_subgraph(g_primschool, v_grade1students)
subgraph_grade5<-induced_subgraph(g_primschool, v_grade5students)

school_comm_fast1 <- fastgreedy.community(subgraph_grade1, weights=E(subgraph_grade1)$weight)
plot(school_comm_fast1,subgraph_grade1, vertex.label= stud.gender, vertex.size=2)

school_comm_fast5 <- fastgreedy.community(subgraph_grade5, weights=E(subgraph_grade5)$weight)
plot(school_comm_fast5,subgraph_grade5, vertex.label= stud.gender, vertex.size=2)

