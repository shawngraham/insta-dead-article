options(java.parameters = "-Xmx6144m")
# install devtools
install.packages("devtools")
install.packages("igraph")
# load devtools
library(devtools)

# install arcdiagram
install_github('arcdiagram', username='gastonstat')

# load arcdiagram
library(igraph)
library(dplyr)
library(arcdiagram)
# location of 'gml' file
net_file = "accounts-openly-selling-connected-by-followers-in-common.graphml"
# read 'graphml' file
bone_graph = read.graph(net_file, format="graphml")

# get edgelist
edgelist = get.edgelist(bone_graph)

# get vertex labels
vlabels = get.vertex.attribute(bone_graph, "label")



# get vertex groups
vgroups = get.vertex.attribute(bone_graph, "Modularity Class")

# get vertex fill color
vfill = get.vertex.attribute(bone_graph, "color")

# get vertex border color
vborders = get.vertex.attribute(bone_graph, "out-degree")

# get vertex degree
#degrees = degree(bone_graph)

interestingthing = get.vertex.attribute(bone_graph, "pagerank")

# get edges value
values = get.edge.attribute(bone_graph, "weight")

# load reshape
library(reshape)

# data frame with vgroups, degree, vlabels and ind
#x = data.frame(vgroups, degrees, vlabels, ind=1:vcount(bone_graph))
x = data.frame(vgroups, interestingthing, vlabels, ind=1:vcount(bone_graph))

x$vlabels <-as.character(x$vlabels)


# arranging by vgroups and degrees
#y = arrange(x, desc(vgroups), desc(degrees))
#ok, how should things be arranged?
y = arrange(x, (vgroups), desc(interestingthing))

# get ordering 'ind'
new_ord = y$ind

# plot arc diagram
dpi=600    #pixels per square inch
png("figure.png", width=8*dpi, height=8*dpi, res=dpi)

arcplot(edgelist, ordering=new_ord, labels=vlabels, cex.labels=0.8,
        show.nodes=TRUE, col.nodes=vfill, bg.nodes=vfill,
        #cex.nodes = log(degrees)+0.5, pch.nodes=21,
        lwd.nodes = 2, line=-0.5,
        col.arcs = hsv(0, 0, 0.2, 0.25),lwd.arcs =  0.005 * values)
dev.off()
get

