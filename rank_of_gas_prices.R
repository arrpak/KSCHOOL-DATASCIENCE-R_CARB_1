library(reshape2)
library(plyr)

carb <- read.csv('carburantes_20050222.csv', sep='\t', dec=',', header=TRUE )
#only stay with Provincia, Direccion and prices of all gas types
carb <- carb[,c(1,5,9:17)]
carb.long <- melt(carb, id.vars = c('Provincia','Dirección'))
carb.long <- carb.long[!is.na(carb.long$value),] 

##mean, max and min value of all types of gas by state
res <- ddply(carb.long, .(Provincia, variable), summarize, mean = mean(value), max = max(value), min = min(value))
res[res$Provincia=='MADRID',]

##rank of min prices
carb_min <- ddply(carb.long, .(Provincia, variable), transform, min_rank=rank(value, ties='random'))

#min price of every type of gas in Madrid
carb_rank <- carb_min[carb_min$min_rank==1,]
carb_rank[carb_rank$Provincia=='MADRID',1:4]

#10 lowest prices of gas 98 in Madrid
carb_rank <- carb_min[carb_min$min_rank<=10,]
carb_rank <- carb_rank[carb_rank$Provincia=='MADRID' & carb_rank$variable=='Precio.Gasolina..98',]
carb_rank[order(carb_rank$min_rank),]

##rank of max prices
carb_max <- ddply(carb.long, .(Provincia, variable), transform, max_rank=rank(-value, ties='random'))

##max price of every type of gas in Madrid
carb_rank <- carb_max[carb_max$max_rank==1,]
carb_rank[carb_rank$Provincia=='MADRID',1:4]

##10 highest prices of gas 98 in Madrid
carb_rank <- carb_max[carb_max$max_rank<=10,]
carb_rank <- carb_rank[carb_rank$Provincia=='MADRID' & carb_rank$variable=='Precio.Gasolina..98',]
carb_rank[order(carb_rank$max_rank),]