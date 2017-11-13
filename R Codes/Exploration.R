library(ggplot2)
library(gridExtra)
library(data.table)
load('RawData.rda')

## Visualize

XPlot<-function(n){
  column = colnames(Raw_Train)[n]
  value = Raw_Train[,eval(parse(text = column))]
  plot <- ggplot(Raw_Train, aes(x = eval(parse(text = column)), y = V_price_doc))
  
  if(is.numeric(value)) {
    if (length(unique(value))>500) {plot <- plot + geom_smooth()}
    else{
      plot <- plot + 
        geom_smooth() +
        geom_errorbar(stat="summary", color="grey40", width=0.4)+
        geom_line(stat = "summary", color="red", size=1.5)+
        geom_point(size = 5, color="red", fill="white", shape = 21, stat="summary", stroke=1.5)
    }
  } else {plot <- plot + geom_bar(stat="summary",fun.y="mean")}
  
  plot <- plot + xlab(Dict[column,Meaning])+ylab("Sale Price") + 
    theme(plot.title = element_text(size = 50),  
          axis.title.x = element_text(size = 20),  
          axis.title.y = element_text(size = 20),
          text = element_text(size=20))  
  plot
}

NCOL = ncol(Raw_Train)
pdf('Data Exploration.pdf', width=21, height=27)
plotL = lapply(1:NCOL, XPlot) 
i = 1
while(i<=NCOL){
  if (i %% 9 == 0) print (do.call(grid.arrange,  plotL[(i-8):i]))
  if (i %% 9 != 0 & i == NCOL) print (do.call(grid.arrange,  plotL[(NCOL - NCOL %% 9 + 1):NCOL]))
  i = i + 1
}
dev.off()

#####################################################################
##### Copied from online discussion for self-learning purpose #######
#####################################################################

library(tidyverse)
dtrain <- fread("./Raw Data/train.csv")
dtrain[,month:=month(timestamp)]
dtrain[,year:=year(timestamp)]

ggplot(dtrain, aes(x = month, y = price_doc))+
      geom_errorbar(stat="summary", color="grey40", width=0.4)+
      geom_line(stat = "summary", color="red", size=1.5)+
      facet_grid(~year)


ggplot(aes(x=price_doc), data = dtrain)+
  #geom_histogram(aes(x=full_all),bins=1000, fill="darkblue")+
  geom_density(fill='red', color='red') + 
  geom_rug(colour="grey60")+
  facet_grid(~product_type) +
  theme_bw()+
  theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  scale_x_continuous(trans='log')+
  xlab("Price")+ylab("Density")

dtrain %>% 
  filter(build_year > 1691 & build_year < 2018) %>% 
  ggplot(aes(x=build_year)) + 
  geom_histogram(fill='red') + 
  ggtitle('Distribution of build year')

miss_pct <- map_dbl(dtrain, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })

miss_pct <- miss_pct[miss_pct > 0]

data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) + 
  geom_bar(stat='identity', fill='red') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))

library(corrplot)
internal_chars <- c('full_sq', 'life_sq', 'floor', 'max_floor', 'build_year', 'num_room', 
                    'kitch_sq', 'state' ,'price_doc')

corrplot(cor(dtrain[, ..internal_chars], use="complete.obs"))

demo_vars <- c('area_m', 'raion_popul', 'full_all', 'male_f', 'female_f', 'young_all', 
               'young_female', 'work_all', 'work_male', 'work_female', 'price_doc')

corrplot(cor(dtrain[, ..demo_vars], use='complete.obs'))

table(dtrain$num_room)
ggplot(aes(x=num_room), data=dtrain) + 
  geom_histogram(fill='red', bins=20) + 
  ggtitle('Distribution of room count')

library(caret)

#Get complete cases of dtrain
completes <- complete.cases(dtrain)

# Set training control so that we only 1 run forest on the entire set of complete cases
trControl <- trainControl(method='none')

# Run random forest on complete cases of dtrain. Exclude incineration_raion since it
# only has 1 factor level
rfmod <- train(price_doc ~ . - id - timestamp - incineration_raion,
               method='rf',
               data=dtrain[completes, ],
               trControl=trControl,
               tuneLength=1,
               importance=TRUE)

varImp(rfmod)


