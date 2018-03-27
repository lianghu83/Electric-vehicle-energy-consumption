"""
Use the EV energy consumption model into real world.
"""

import pandas as pd
import numpy as np
import random

#set parameters
m = 1621 #vehicle mass, kg
g = 9.8066 #m/s2
rho_air = 1.2256 #air density, kg/m3
Af = 2.3316 #front area, m2
Cd = 0.28 #car drag coefficient
f = 0.01 #rolling resistance
boundary = 72/3.6 #city/hwy boundary, m/s; 45mph

#regression parameters in lists
#ECR (electricity consumption rate) = Constant + a*VSP + b*P_aux
#low speed, high speed, threshold=45mph
#VSP>0, =0, <0
city_p = [3215.396567, 1160.132663, 2.149907]
city_i = [609.84947, 1.18721]
city_n = [719.66540, 557.56682, 2.09965]
hwy_p = [8428.308370, 757.207729, 2.599005]
hwy_n = [8119.872, 594.226, 2.574]

#read driving cycle file
dc = pd.read_csv("C:\\...")

#rename speed column as S, acceleration column as A
#m/s, m/s2
dc = dc[['S', 'A']]

#VSP, W/kg
dc['VSP'] = dc['S']*(1.1*dc['A'] + g*f) + 0.5*rho_air*Cd*Af/m*dc['S']**3
  
#temperature celsius, set to constant in 20~30 degrees, or random
dc['Temperature'] = 25
#dc['Temperature'] = [random.uniform(20, 30) for x in range(len(dc))]

#Auxiliary load, W
dc['P_aux'] = np.exp(-0.089400*dc['Temperature'] + 6.711928)
  
#Calculate ECR
for i in range(len(dc)):
    if dc.loc[i, 'S'] < boundary and dc.loc[i, 'VSP'] > 0.0:
        dc.loc[i, 'ECR'] = city_p[0] + city_p[1]*dc.loc[i, 'VSP'] + city_p[2]*dc.loc[i, 'P_aux']
    if dc.loc[i, 'S'] < boundary and dc.loc[i, 'VSP'] == 0.0:
        dc.loc[i, 'ECR'] = city_i[0] + city_i[1]*dc.loc[i, 'P_aux']
    if dc.loc[i, 'S'] < boundary and dc.loc[i, 'VSP'] < 0.0:
        dc.loc[i, 'ECR'] = city_n[0] + city_n[1]*dc.loc[i, 'VSP'] + city_n[2]*dc.loc[i, 'P_aux']
    if dc.loc[i, 'S'] >= boundary and dc.loc[i, 'VSP'] > 0.0:
        dc.loc[i, 'ECR'] = hwy_p[0] + hwy_p[1]*dc.loc[i, 'VSP'] + hwy_p[2]*dc.loc[i, 'P_aux']
    if dc.loc[i, 'S'] >= boundary and dc.loc[i, 'VSP'] < 0.0:
        dc.loc[i, 'ECR'] = hwy_n[0] + hwy_n[1]*dc.loc[i, 'VSP'] + hwy_n[2]*dc.loc[i, 'P_aux']
    
#total energy consumption, kWh
print(sum(dc['ECR'])/1000/3600)
        
        
