"""
Build an estimating model for vehicle fuel consumption
"""



import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn import linear_model
from sklearn import svm

car = pd.read_csv("C:\\...")
car.head()

df = car.loc[:, ['FC_L1', 'Veh_Speed.m.s.', 'Acceleration']]
df.columns = ['FC_L1', 'S', 'A']

df['S2'] = df['S']**2
df['S3'] = df['S']**3
df['A2'] = df['A']**2
df['A3'] = df['A']**3
df['SA'] = df['S']*df['A']
df['S2A'] = df['S']**2*df['A']
df['S3A'] = df['S']**3*df['A']
df['SA2'] = df['S']*df['A']**2
df['S2A2'] = df['S']**2*df['A']**2
df['S3A2'] = df['S']**3*df['A']**2
df['SA3'] = df['S']*df['A']**3
df['S2A3'] = df['S']**2*df['A']**3
df['S3A3'] = df['S']**3*df['A']**3
  
df_acc = df.loc[df['A']>=0]
df_acc = df_acc.reset_index(drop=True)
df_dec = df.loc[df['A']<0]
df_dec = df_dec.reset_index(drop=True)

df_acc_X_train = df_acc.drop('FC_L1', 1)[:-109095]
df_acc_X_test = df_acc.drop('FC_L1', 1)[-109095:]
df_acc_y_train = np.log(df_acc.FC_L1[:-109095])
df_acc_y_test = np.log(df_acc.FC_L1[-109095:])

df_dec_X_train = df_dec.drop('FC_L1', 1)[:-109095]
df_dec_X_test = df_dec.drop('FC_L1', 1)[-109095:]
df_dec_y_train = np.log(df_dec.FC_L1[:-41051])
df_dec_y_test = np.log(df_dec.FC_L1[-41051:])
  
#fit regression model
regr_acc = linear_model.LinearRegression()
regr_acc.fit(df_acc_X_train, df_acc_y_train)
#coefficients
print('Coefficients: \n', regr_acc.coef_)

#mean squared error
print("Mean squared error: %.2f"
      % np.mean((regr_acc.predict(df_acc_X_test) - df_acc_y_test) ** 2))
#explained variance score: 1 is perfect prediction
print('Variance score: %.2f' % regr_acc.score(df_acc_X_test, df_acc_y_test))

#SVM
clf = svm.SVR(kernel='linear', C=1e3)
clf.fit(df_acc_X_train, df_acc_y_train)
np.mean((clf.predict(df_acc_X_test) - df_acc_y_test) ** 2)
clf.score(df_acc_X_test, df_acc_y_test)
cross_val_score(clf, df_acc_X_test, df_acc_y_test, scoring='neg_log_loss')

#decision tree
from sklearn import tree
from sklearn.model_selection import cross_val_score
tlf = tree.DecisionTreeRegressor()
tlf = tlf.fit(df_acc_X_train, df_acc_y_train)
np.mean((tlf.predict(df_acc_X_test) - df_acc_y_test) ** 2)
tlf.score(df_acc_X_train, df_acc_y_train)
#cross_val_score(tlf, df_acc_X_test, df_acc_y_test, scoring='neg_log_loss')


