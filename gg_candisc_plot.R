gg.candisc.plot <- function(candisc.object){

  # based on: https://gist.github.com/low-decarie/7449296  
  
  # Plot with ellipses using ggplot2 ####
  
  library(grid)
  library(ggplot2)
  library(devtools)
  library(digest)
  library(ggrepel)
  
  #For info : http://stackoverflow.com/questions/2397097/how-can-a-data-ellipse-be-superimposed-on-a-ggplot2-scatterplot
  #source_url("https://raw.github.com/low-decarie/FAAV/master/r/stat-ellipse.R") 
  #source("StatEllipse.R")
  
  #Nicer theme
  #theme_set(theme_bw())
  
  #Basic score plot
  labels <- names(candisc.object$scores)[1]
  can.plot <- qplot(data=candisc.object$scores,
                    x = Can1,
                    y = Can2,
                    colour=get(labels)) +
    labs(x = paste0("Can1 ", round(candisc.object$pct[1], digits = 2), " %"),
         y = paste0("Can2 ", round(candisc.object$pct[2], digits = 2), " %")) +
    stat_ellipse() +  #Add ellipses
    coord_equal()
  
  
  #Add arrows
  
  #Create arrow coordinates
  #arrow.coord <- data.frame(x=0,y=0,
  #                          xend=candisc.object$structure[,1],yend=candisc.object$structure[,2],
  #                          variable=row.names(candisc.object$structure))
  
  #Enlarge arrows
  #enlarge <- 3
  #arrow.coord$xend <- enlarge*arrow.coord$xend
  #arrow.coord$yend <- enlarge*arrow.coord$yend
  
  
  #Plot arrows
  #can.plot <- can.plot+geom_segment(data=arrow.coord,
  #                                  colour=I("black"),
  #                                  aes(x=x,
  #                                      y=y,
  #                                      xend=xend,
  #                                      yend=yend),
  #                                  arrow = arrow(length = unit(0.5,"cm")))
  
  #Add arrow labels
  #can.plot <- can.plot+geom_text(data=arrow.coord,
  #                               colour=I("black"),
  #                               aes(x=xend,
  #                                   y=yend,
  #                                   label=variable,
  #                                   hjust=0.5,
  #                                   vjust=0.5))
  
  #Add labels
  can.plot <- can.plot + theme(legend.position = 'none') + 
    geom_label_repel(aes(label = get(labels), colour = get(labels)),
                                                              box.padding   = 0.35, 
                                                              point.padding = 0.5)
  
  
  print(can.plot)
}
