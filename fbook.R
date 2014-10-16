library(Rfacebook)
library(Rook)
library(igraph)
library(gtools)

fb_oauth <- ""
            # El token de arriba es temporal, para obtener uno ir a:
            # ir a https://developers.facebook.com/tools/explorer
            # Seleccionar "Unversioned" en API Version
            # Click Get_Access_Token
            # Seleccionar casillas de data que se quiere
            # Aceptar y aceptar.
            # Copiar Access_Token arriba

download_friends <- function(fb_oauth){
      
      my_friends <- getFriends(token=fb_oauth, simplify=TRUE)

      #Too many friends in one group gets rejected by FB API. 
      #Break into groups of 80.
      rq <- ceiling(nrow(my_friends)/80)
      
      my_friends_info <- getUsers(my_friends[1:80,]$id, token=fb_oauth, private_info=TRUE)
      
      for(i in 1:rq-2){
            
            a <- i*80+1
            b <- (1+i)*80
            
            split_my_friends <-  my_friends[a:b,]
            friends_info <- getUsers(split_my_friends$id, token=fb_oauth, private_info=TRUE)
            my_friends_info <- rbind(my_friends_info, friends_info)
      }
      
      a <- (rq-1)*80+1
      b <- nrow(my_friends)
      
      split_my_friends <-  my_friends[a:b,] 
      friends_info <- getUsers(split_my_friends$id, token=fb_oauth, private_info=TRUE)
      
      return(rbind(my_friends_info, friends_info))
}

#GRAPH

simple_graph <- function(my_network){
      #my_network <- getNetwork(fb_oauth, format="adj.matrix")
      #save(my_network, file="my_network")
      #OR
      #load(my_network, file="my_network")

      singletons <- rowSums(my_network)==0 
      
      # remove singletons
      my_graph <- graph.adjacency(my_network[!singletons,!singletons])
      
      # make connections one way
      return(simplify(my_graph, remove.multiple = TRUE, remove.loops = TRUE))
}

plot_drl<-function(my_graph_simple){
      layout.drl <- layout.drl(my_graph_simple,options=list(simmer.attraction=0))      
      
      E(my_graph_simple)$color <- rgb(.5, .5, 0, 0.15)
      E(my_graph_simple)$width <- 0.0001
      
      # Let's colour based on connectedness.
      #this is the number of "shortest paths" going 
      #through a particular individual
      hc4 <- heat.colors(10,alpha=0.9)
      betweenness <- betweenness(my_graph_simple)
      #vcolors <- factor(cut(betweenness, quantile(betweenness), include.lowest = TRUE))
      vcolors <- quantcut(betweenness, q=seq(0,1,by=0.1))
      vcolors2 <- hc4[vcolors]
      
      return(plot(my_graph_simple, vertex.size=2, vertex.color=vcolors2,
                  vertex.label=NA, 
                  vertex.label.cex=2,
                  edge.arrow.size=0, edge.curved=TRUE,layout=layout.drl))
}

plot_fr<-function(my_graph_simple){
      layout.fr <- layout.fruchterman.reingold(my_graph_simple, options=list(simmer.attraction=0))
      
      E(my_graph_simple)$color <- rgb(.5, .5, 0, 0.15)
      E(my_graph_simple)$width <- 0.0001
      
      # Let's colour based on connectedness.
      #this is the number of "shortest paths" going 
      #through a particular individual
      hc4 <- heat.colors(10,alpha=0.9)
      betweenness <- betweenness(my_graph_simple)
      #vcolors <- factor(cut(betweenness, quantile(betweenness), include.lowest = TRUE))
      vcolors <- quantcut(betweenness, q=seq(0,1,by=0.1))
      vcolors2 <- hc4[vcolors]
      return(plot(my_graph_simple, vertex.size=2, vertex.color=vcolors2,
                  vertex.label=NA, 
                  vertex.label.cex=2,
                  edge.arrow.size=0, edge.curved=TRUE,layout=layout.fr))
}

plot_kk<-function(my_graph_simple){
      layout.kk <- layout.kamada.kawai(my_graph_simple,options=list(simmer.attraction=0))
      
      E(my_graph_simple)$color <- rgb(.5, .5, 0, 0.15)
      E(my_graph_simple)$width <- 0.0001
      
      # Let's colour based on connectedness.
      #this is the number of "shortest paths" going 
      #through a particular individual
      hc4 <- heat.colors(10,alpha=0.9)
      betweenness <- betweenness(my_graph_simple)
      #vcolors <- factor(cut(betweenness, quantile(betweenness), include.lowest = TRUE))
      vcolors <- quantcut(betweenness, q=seq(0,1,by=0.1))
      vcolors2 <- hc4[vcolors]
      
      return(plot(my_graph_simple, vertex.size=2, vertex.color=vcolors2,
                  vertex.label=NA, 
                  vertex.label.cex=2,
                  edge.arrow.size=0, edge.curved=TRUE,layout=layout.kk))
}