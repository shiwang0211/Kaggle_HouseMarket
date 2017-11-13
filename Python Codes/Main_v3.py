# -*- coding: utf-8 -*-
"""
Created on Sun May 14 10:05:38 2017

@author: shiwa
"""

import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
from numpy.polynomial.chebyshev import *
import matplotlib.pyplot as plt
import seaborn as sns

pd.options.mode.chained_assignment = None  
pd.set_option('display.max_columns', 500)

read_columns= ['timestamp', 'oil_urals', 'gdp_quart_growth', 'cpi', 'usdrub', \
                'salary_growth', 'unemployment', 'average_provision_of_build_contract_moscow', 'mortgage_rate', \
                 'deposits_rate','deposits_growth','rent_price_3room_eco',\
                 'rent_price_3room_bus']
train_df = pd.read_csv("../Raw Data/train.csv",usecols=['timestamp','price_doc','full_sq'])
macro_df = pd.read_csv("../Raw Data/macro.csv",usecols=read_columns)


def condition_train(value, col):
    vals = (macro_df[macro_df['mo_ye'] == value])
    ret = vals[col].asobject
    ret = ret[0]
    return ret

def condition_test(value, col):
    vals = (macro[macro['mo_ye'] == value])

    ret = vals[col].asobject

    ret = ret[0]

    return ret

def condition(value,col):
    vals = (macro_df[macro_df['timestamp'] == value])
    ret=vals[col].asobject
    ret=ret[0]

    return ret

def init_anlz_file():

    anlz_df = train_df
    for clmn in read_columns:
        if clmn == 'timestamp':
            continue
        anlz_df[clmn] = np.nan
        anlz_df[clmn] = anlz_df['timestamp'].apply(condition, col=clmn)
        print(clmn)
    return anlz_df

anlz_df=init_anlz_file() # Merge

methods=['pearson', 'kendall', 'spearman']
def plot_grouped_trends(df,feat1,feat2,corr_df):
   
    fig, ax = plt.subplots(1, 2, figsize=(10, 5))
    x=df.index.values
    ch=chebfit(x,df[feat1].values,7)
    trendf1=chebval(x,ch)
    ax[0].plot(x,df[feat1].values,x,trendf1)
    ax[0].set_ylabel(feat1)
    ax[0].set_title('Chart '+feat1+' vs trend' )
    ax[0].set_xlabel('months count')
    ch2=chebfit(x,df[feat2].values,7)
    trendf2=chebval(x,ch2)
    ax[1].plot(x,df[feat2].values,x,trendf2)
    ax[1].set_ylabel(feat2)
    ax[1].set_title('Chart '+feat2+' vs trend' )
    ax[1].set_xlabel('months count')
    ##### do here two charts density distribition
    
    ls=[feat2]
    for method in methods:
        corr=df[[feat1,feat2]].corr(method=method)
        ls.append(corr[feat1][1])
    corr_df.loc[len(corr_df)]=ls

anlz_df['timestamp']=pd.to_datetime(anlz_df['timestamp'])
anlz_df['mo_ye']=anlz_df['timestamp'].apply(lambda x: x.strftime('%m-%Y'))
anlz_df['price_per_sqm']=anlz_df['price_doc']/anlz_df['full_sq']


macro_columns = ['price_doc','price_per_sqm','full_sq','oil_urals', 'gdp_quart_growth', 'cpi', 'usdrub', \
                'salary_growth', 'unemployment', 'average_provision_of_build_contract_moscow', 'mortgage_rate', \
                 'deposits_rate','deposits_growth','rent_price_3room_eco',\
                 'rent_price_3room_bus']
macro_df=pd.DataFrame(anlz_df.groupby('mo_ye')[macro_columns].mean())
macro_df.reset_index(inplace=True)


macro_df['mo_ye']=pd.to_datetime(macro_df['mo_ye'])
macro_df=macro_df.sort_values(by='mo_ye')


macro_df.reset_index(inplace=True)
macro_df.drop(['index'],axis=1,inplace=True)


corr_df=pd.DataFrame(columns=['feature','pearson', 'kendall', 'spearman'])
corr=macro_df[macro_columns].corr(method='spearman')
fig, ax = plt.subplots(figsize=(10,10))         # Sample figsize in inches
sns.heatmap(corr, annot=True, linewidths=.5, ax=ax)

for feat in macro_columns:
    if (feat=='price_doc'):
        continue
    plot_grouped_trends(macro_df,'price_doc',feat,corr_df)


sig_macro_columns=['oil_urals', 'gdp_quart_growth', 'cpi', 'usdrub', \
                'salary_growth', 'unemployment', 'mortgage_rate', \
                 'deposits_rate','rent_price_3room_bus']






train = pd.read_csv("../Raw Data/train.csv", parse_dates=['timestamp'])
train['mo_ye']=train['timestamp'].apply(lambda x: x.strftime('%m-%Y'))
#macro_df['mo_ye']=macro_df['mo_ye'].apply(lambda x: x.strftime('%m-%Y'))
test_df = pd.read_csv("../Raw Data/test.csv",parse_dates=['timestamp'])
test_df['mo_ye']=test_df['timestamp'].apply(lambda x: x.strftime('%m-%Y'))
macro=pd.read_csv("../Raw Data/macro.csv", parse_dates=['timestamp'])
macro['mo_ye'] = macro['timestamp'].apply(lambda x: x.strftime('%m-%Y'))

for clmn in sig_macro_columns:
    train[clmn] = train['mo_ye'].apply(condition_train, col=clmn)
    test_df[clmn] = test_df['mo_ye'].apply(condition_test, col=clmn)
  
train=train.drop(['timestamp'],1) 


#free memory
del(train_df)
del(macro_df)
del(anlz_df)


from sklearn import model_selection, preprocessing

x_train = train


##  Encode categirical varaibles exclude 'mo_ye'
for c in x_train.columns:
    if c=='mo_ye':
        continue
    if x_train[c].dtype == 'object':
        lbl = preprocessing.LabelEncoder()
        lbl.fit(list(x_train[c].values))
        x_train[c] = lbl.transform(list(train[c].values))
x_train['mo_ye']=x_train['mo_ye'].apply(lambda x:  100*pd.to_datetime(x).year+pd.to_datetime(x).month)
x_train['price_doc']=np.log1p(x_train['price_doc'])

from sklearn.base import TransformerMixin
class DataFrameImputer(TransformerMixin):
    def fit(self, X, y=None):
        self.fill = pd.Series([X[c].value_counts().index[0]
        if X[c].dtype == np.dtype('O') else X[c].median() for c in X],
        index=X.columns)
        return self
    def transform(self, X, y=None):
        return X.fillna(self.fill)
x_train = DataFrameImputer().fit_transform(x_train)

import operator
import xgboost as xgb
target = 'price_doc'
IDcol = 'id'

predictors = [x for x in x_train.columns if x not in [target, IDcol]]
xgb_params = {
    'eta': 0.05,
    'max_depth': 8,
    'subsample': 0.7,
    'colsample_bytree': 0.7,
    'objective': 'reg:linear',
    'eval_metric': 'rmse',
    'silent': 1
}
train_matrix= xgb.DMatrix(x_train[predictors], x_train[target].values, feature_names=x_train[predictors].columns.values)
model = xgb.train(dict(xgb_params, silent=1), train_matrix, num_boost_round=100)
# plot the important features #
importance = model.get_fscore()
importance = sorted(importance.items(), key=operator.itemgetter(1))
df = pd.DataFrame(importance, columns=['feature', 'fscore'])
df['fscore'] =100* df['fscore'] / df['fscore'].max()
df=df.sort_values(by="fscore",ascending=False)
df.head(50).plot(kind='barh', x='feature', y='fscore', legend=False, figsize=(10, 10))

plt.title('XGBoost Feature Importance( 50 significant)')

#plt.annotate()
plt.xlabel('relative importance')
plt.show()






