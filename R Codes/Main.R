library(data.table)
library(dplyr)

Raw_Train <- fread("./Raw Data/train.csv")
Raw_Test <- fread("./Raw Data/test.csv")
Raw_macro <- fread("./Raw Data/macro.csv")
Dict <- fread("./Dictionary/data dictionary.csv")
setkey(Dict,Variable)

# Outlier for Y， 10std

Ub <- mean(Raw_Train$price_doc) + 10 * sd(Raw_Train$price_doc)
Lb <- mean(Raw_Train$price_doc) - 10 * sd(Raw_Train$price_doc)
df_Train = Raw_Train[price_doc < Ub & price_doc > Lb]

# Outlier for price per sq feet

#Ub <- 600000
#Lb <- 10000
#df_Train = df_Train[price_doc/full_sq >= Lb & price_doc/full_sq <= Ub]

# Bind Train and Test for pre-processing

df_Train[,TestTrain := "Train"] # Raw train  add a label, column name is "TestTrain"
df_Test = Raw_Test[,price_doc := -1]
df_Test [,TestTrain := "Test"]
df_All = rbind(df_Train, df_Test)

# Numeric columns
#Numeric_Cols = ""
#for(col in names(df_All)){
#  if(col == "id" | col == "timestamp" | col == "price_doc") next
#  if(class(df_All[[col]]) == "character") next
#    else Numeric_Cols = c(Numeric_Cols,col)
#} 
#Numeric_Cols = Numeric_Cols[-1]

# Remove some columns

#ColumnsInDict = colnames(df_All)[(colnames(df_All) %in% Dict[,Variable])] # columns in dict
#demo_vars <- c('area_m', 'raion_popul','railroad_km','cafe_count_5000',
#               'metro_km_avto','mosque_km','indust_part') # other cols might be useful
#Columns = c(ColumnsInDict,demo_vars,"TestTrain")

# Fix Obvious data entry errors

df_All$state[df_All$state == 33] <- which.max(table(df_All$state))
df_All$build_year[df_All$build_year == 20052009] <- 2007

# Remove outliers for X
outlier_ids = df_All [(full_sq > 2000 | life_sq > 2000 | full_sq == 0 | life_sq == 0 |
                       build_year < 1900 | build_year > 2025 |
                       floor < 0 | floor > 60 | 
                       max_floor < 0 | max_floor > 80 | floor > max_floor |
                       state < 1 | state > 4 |
                       kitch_sq > 500 | 
                       full_all > 500000) & TestTrain == "Train", id]
df_All = df_All[!(id %in% outlier_ids)]


#miss_pct <- map_dbl(df_All, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })

# Join with Macro-eco data
#sig_macro_columns=c("timestamp",'oil_urals', 'gdp_quart_growth', 'cpi', 'usdrub',
#                    'salary_growth', 'unemployment', 'mortgage_rate', 
#                    'deposits_rate','rent_price_3room_bus')
#df_All = data.table(merge(df_All, Raw_macro[,..sig_macro_columns], by = "timestamp", all.x = TRUE))
df_All = data.table(merge(df_All, Raw_macro, by = "timestamp", all.x = TRUE))

# Impute NA with most frequently occursing cases, can be excluded if using xgboost
#for(col in names(df_All)){
#  df_col = df_All[TestTrain == "Train",..col]
#  if(col == "id" | col == "timestamp") next
#  Mode = names(table(df_col)[which.max(table(df_col))]) # most frequent factor
#  Mode = if(class(df_All[[col]]) == "character") Mode else median(df_All[[col]],na.rm=TRUE)
#  df_All[, (col) := apply(.SD, 1, function(x) if (is.na(x)) Mode else x), .SDcols = col]
#}  


#Lon, Lat

Location<-fread('./Raw Data/Longitud_Latitud.csv')
Density_by_area = df_All[,.(CountByArea = .N),by = sub_area]
df_All<-df_All %>% 
  left_join(Location, by = "sub_area") %>%
  left_join(Density_by_area, by = "sub_area") %>%
  data.table()

# Time 
# Add Month-Year and Week-Year

df_All[,month:=month(timestamp)]
df_All[,year:=year(timestamp)]
df_All[,week:=week(timestamp)]
df_All[,dom:=mday(timestamp)]
df_All[,dow:=wday(timestamp)]
df_All[,month_year := month + year * 100]
df_All[,week_year := week + year * 100]
month_year_cnt = df_All[,.(month_year_cnt = .N), by = month_year]
week_year_cnt = df_All[,.(week_year_cnt = .N), by = week_year]
df_All<-df_All %>%
  left_join(month_year_cnt, by = "month_year") %>%
  left_join(week_year_cnt, by = "week_year") %>%
  select(-c(timestamp,week_year,month_year) )%>%
  data.table()


# Advance Built?

df_All[,Advance:= ifelse(build_year > year, "YES", "NO")]

# Others

df_All[,Percent_Floor:=ifelse(max_floor == 0, 0, floor/max_floor)]
df_All[,Area_per_Room:=full_sq/num_room]
df_All[,Percent_Kitchen:=ifelse(full_sq == 0, 0,kitch_sq/full_sq)]
df_All[,Percent_Living:=ifelse(full_sq == 0, 0,life_sq/full_sq)]

# Factorize selected variables
FactCol=vector()
for(col in names(df_All)){
  if(col == "id" | col == "timestamp") next
  if(class(df_All[[col]]) == "character")  FactCol=c(FactCol, col)
}  
#FactCol = c("material", "product_type","Advance","month","dom","dow")
df_All[, (FactCol) := lapply(.SD, as.factor), .SDcols = FactCol]


Test_ID = df_All[TestTrain == "Test",id]
df_All<-df_All%>%
  select(-c(id,sub_area))
save(df_All, file = "./RawData.rda")


### ---------------------------------------------------------------------------------------

library(corrplot)
library(ggplot2)

ggplot(df_All[TestTrain == "Train"], aes(x = Test_1, y = price_doc))+
  geom_bar(stat="summary",fun.y="mean") +
  facet_grid(~year)

internal_chars <- c('full_sq', 'life_sq', 'floor', 'max_floor', 'build_year', 'num_room', 'Percent_Floor','Percent_Living',
                    'kitch_sq', 'state' ,'CountByArea','area_m', 'raion_popul','full_all','price_doc')
Numeric_Cols_ = c(Numeric_Cols[1:25],"price_doc")
corrplot(cor(df_All[TestTrain == "Train",..Numeric_Cols_], use="complete.obs"))
corrplot(cor(df_All[TestTrain == "Train", ..internal_chars], use="complete.obs"))
corrplot(cor(df_All[TestTrain == "Train", ..sig_macro_columns][,-"timestamp"], use='complete.obs'))
