https://www.kaggle.com/mtoconnor/network-mapping-hollywood-actor-overlap


data=read.csv("movie.csv",stringsAsFactors = F)

library(reshape2)
library(network)
library(sna)
library(ggplot2)
library(GGally)
library(readr)



getIMDBGraph<-function(data, 
                       firstyear =1, 
                       lastyear = 3000, 
                       genre = FALSE, 
                       minranking = 0, 
                       maxranking = 10){
  if(is.character(genre)){
    data<-data[grep(genre, data$genres),]}
  
  data<-subset(data, 
               data$title_year >= firstyear & 
                 data$title_year <= lastyear &
                 data$imdb_score <= maxranking &
                 data$imdb_score >= minranking)
  
  dataset<- data.frame(data$movie_title, 
                  data$actor_1_name, 
                  data$actor_2_name, 
                  data$actor_3_name)
  
  dataset<- melt(dataset, id.vars = 'data.movie_title')
  names(dataset)<-c('title', 
               'actornum', 
               'actor')
  dataset<-dataset[,c(1,3)]
  edges<-merge(x = dataset, 
               y = dataset, 
               by = 'title')
  
  edges<-subset(edges,edges$actor.x != edges$actor.y)
  edgelist<-as.matrix(edges[,c(2:3)])
  graph<-network(edgelist)
  return(graph)}

graph <- getIMDBGraph(data, firstyear = 2000, lastyear = 2008)

p <- ggnet2(graph, 
            size = 'degree',  # feature by which the nodes are scaled
            size.min = 40,  # lower bound of the nodes to be plotted
            label = T,  #plot labels?
            # mode = 'kamadakawai', # plotting algo for node placement (defaults to FR)
            label.size = 2.5, 
            node.size = 7, 
            node.color = 'grey70',
            node.alpha = 0.7,
            edge.alpha = 0.2,
            edge.size = 0.3,
            legend.size = FALSE, # I don't want a legend 
            legend.position = 'None') 
# Now add a title and subtitle
p <- p + ggtitle('Network of actor connections', 
                 subtitle = '2000\'s')

# And format the title
p <- p + theme(plot.title = element_text(hjust = 0.5), 
               plot.subtitle = element_text(hjust = 0.5))
p
