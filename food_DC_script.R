## rict-1 NK rescue#
food_DC<-read.csv(file.choose(), header=TRUE)
food_DC$type<-interaction(food_DC$food_conc, food_DC$food_type)
 
 par(mar=c(8,5,1,1))
 
 library(ggplot2)
 p<-ggplot(food_DC, aes(x=food_conc, y=pct))
 
 p + geom_line(aes(colour = food_type)) +
 geom_jitter(aes(colour = food_type), position = position_jitter(width = .5, height = .2)) + theme_bw() + theme(axis.text.x=element_text(angle=45, hjust=1, size=12)) + facet_grid(. ~ n_eggs)
 ylab("% dauer") +
 theme(panel.grid.minor.y=element_blank(),panel.grid.major.x=element_blank())
