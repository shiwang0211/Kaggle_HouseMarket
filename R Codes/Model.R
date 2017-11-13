library(xgboost)
library(Matrix)
library(data.table)
load('RawData.rda')
#set.seed(1234)
#r2 <- function(pred.y, true.y)
#{ 1 - length(true.y)*mse(pred.y, true.y)/((length(true.y)-1)*var(true.y)) }

## Build Boosting Tree Model
colnames(df_All)<-paste("V_",colnames(df_All),sep="")
ModelColumns = c("V_price_doc","V_full_sq","V_raion_popul",
                 "V_build_year","V_cpi","V_full_all","V_area_m","V_product_type","V_usdrub","V_floor",
                 "V_state","V_kitch_sq","V_rent_price_3room_bus","V_mortgage_rate","V_railroad_km"
                 ,"V_longitude","V_latitude","V_CountByArea","V_metro_km_avto","V_mosque_km","V_indust_part"
                 ,"V_month_year_cnt"
                 ,"V_week_year_cnt"
                 ,"V_Percent_Floor"
                 ,"V_Percent_Living"
                 ,"V_Percent_Kitchen"
                 ,"V_num_room"
                 ,"V_material"
                 ,"V_oil_urals"
                 ,"V_gdp_quart_growth"
                 ,"V_salary_growth"
                 ,"V_unemployment"
                 ,"V_deposits_rate"
                 ,"V_Advance")
colnames(df_All)[which(colnames(df_All) == "V_build_count_1921-1945")] = "V_build_count_1921_1945"
colnames(df_All)[which(colnames(df_All) == "V_build_count_1946-1970")] = "V_build_count_1946_1970"
colnames(df_All)[which(colnames(df_All) == "V_build_count_1971-1995")] = "V_build_count_1971_1995"
colnames(df_All)[which(colnames(df_All) == "V_rent_price_4+room_bus")] = "V_rent_price_4_room_bus"

ModelColumns = c(colnames(df_All)[1:150], colnames(df_All)[259:404])
df_All = df_All[,..ModelColumns]
#sparse_matrix_train<-Matrix(as.matrix(sapply(df_All[V_TestTrain == "Train",-c("V_id","V_timestamp","V_price_doc")],as.numeric)),sparse=TRUE)
#sparse_matrix_train <- sparse.model.matrix(V_price_doc~.-1, data = df_All[V_TestTrain == "Train", ..ModelColumns])
options(na.action='na.pass')
sparse_matrix_train <- sparse.model.matrix(V_price_doc~.-1, data = df_All[V_TestTrain == "Train"])
output_vector = log(df_All[V_TestTrain == "Train",V_price_doc])

cv.res <- xgb.cv(data = sparse_matrix_train, label = output_vector, nfold = 10, max_depth = 5,
                   eta = 0.05, nthread = 8, nrounds = 300, subsample = 0.7, colsample_bytree = 0.7)

bst <- xgboost(data = sparse_matrix_train, label = output_vector, max_depth = 5,eta = 0.05,
               nthread = 8, nrounds = 106)

# Loss function, log
importance <- xgb.importance(feature_names = colnames(sparse_matrix_train), model = bst)
xgb.plot.importance(importance_matrix = importance,top_n=20)

## Use for prediction
options(na.action='na.pass')
#sparse_matrix_test<-Matrix(as.matrix(sapply(df_All[V_TestTrain == "Test",-c("V_id","V_timestamp","V_price_doc")],as.numeric)),sparse=TRUE)
#sparse_matrix_test <- sparse.model.matrix(V_price_doc~.-1, data =  df_All[V_TestTrain == "Test", ..ModelColumns])\
sparse_matrix_test <- sparse.model.matrix(V_price_doc~.-1, data =  df_All[V_TestTrain == "Test"])
pred <- exp(predict(bst, sparse_matrix_test))
Result = data.frame(id = Test_ID, price_doc = pred)

write.csv(Result, file = "./Result/Result.csv",row.names = FALSE)
