"""
Calibrate a EV electricity consumption model
that considers regenerative braking system.
The RBS model
"""

import pandas as pd

#set parameters
Ur = 318.0 #V
Q = 60.0 #Ah
E = Ur*Q*1*3600 #J

coef = pd.read_csv("C:\\...")
coef = coef.drop('Acceleration', 1)

#four modes: acc, dec, cruise, idle
a = coef.loc[0]
d = coef.loc[1]
c = coef.loc[2]
idle = coef.loc[3]

#read driving cycle file
dc = pd.read_csv("C:\\...")

#rename speed column as S, acceleration column as A
dc = dc[['S', 'A']]

dc['SOC'] = ""
dc['BS'] = ""
dc['BA'] = ""
dc['BSOC'] = ""
dc['ECR'] = ""

SOC_Start = 1.0
dc.loc[0, 'SOC':'ECR'] = [SOC_Start, 0., 0., SOC_Start, idle[0]]

for i in range(1, len(dc)):
    
    dc.loc[i, 'SOC'] = dc.loc[(i-1), 'SOC'] - dc.loc[(i-1), 'ECR']/Ur/Q/3600
    dc.loc[i, 'BS'] = dc.loc[(i-1), 'S']
    dc.loc[i, 'BA'] = dc.loc[(i-1), 'A']
    dc.loc[i, 'BSOC'] = dc.loc[(i-1), 'SOC']
    
    #calculate ECR
    th = 0.25
    if dc.loc[i, 'A'] > th:
        e = a
    if dc.loc[i, 'A'] < -th:  
        e = d
    if dc.loc[i, 'A'] >= -th and dc.loc[i, 'A'] <= th and dc.loc[i, 'S'] != 0.:
        e = c
    if dc.loc[i, 'A'] >= -th and dc.loc[i, 'A'] <= th and dc.loc[i, 'S'] == 0.:
        e = idle
        
    dc.loc[i, 'ECR'] = (e[0] + 
                e[1]*dc.loc[i, 'S'] + 
                e[2]*dc.loc[i, 'S']**2 +
                e[3]*dc.loc[i, 'S']**3 + 
                e[4]*dc.loc[i, 'S']*dc.loc[i, 'A'] +
                e[5]*dc.loc[i, 'S']**2*dc.loc[i, 'A'] +
                e[6]*dc.loc[i, 'BS']**4 + 
                e[7]*dc.loc[i, 'BA']**2 + 
                e[8]*dc.loc[i, 'BA']**3 + 
                e[9]*dc.loc[i, 'BA']**4 + 
                e[10]*dc.loc[i, 'BS']**2*dc.loc[i, 'BA'] + 
                e[11]*dc.loc[i, 'BS']**2*dc.loc[i, 'BA']**2 + 
                e[12]*dc.loc[i, 'BSOC'] + 
                e[13]*dc.loc[i, 'BSOC']*dc.loc[i, 'BS']**3 +
                e[14]*dc.loc[i, 'BSOC']*dc.loc[i, 'BS']*dc.loc[i, 'BA']**3 +
                e[15]*dc.loc[i, 'BSOC']*dc.loc[i, 'BS']**3*dc.loc[i, 'BA'] + 
                e[16]*dc.loc[i, 'BSOC']**2*dc.loc[i, 'BA'] + 
                e[17]*dc.loc[i, 'BSOC']**3*dc.loc[i, 'BS'] +
                e[18]*dc.loc[i, 'BSOC']**3*dc.loc[i, 'BS']**3 +
                e[19]*dc.loc[i, 'BSOC']**3*dc.loc[i, 'S']**3 +
                e[20]*dc.loc[i, 'BSOC']**3*dc.loc[i, 'BS']*dc.loc[i, 'BA']**2 +
                e[21]*dc.loc[i, 'BSOC']**4*dc.loc[i, 'BS']**2 + 
                e[22]*dc.loc[i, 'BSOC']**4*dc.loc[i, 'BS']**4 +
                e[23]*dc.loc[i, 'BSOC']**4*dc.loc[i, 'BS']**2*dc.loc[i, 'BA']**2
                )
                     
#total energy consumption
print(sum(dc['ECR'])/1000/3600) #unit is kWh   
        
        
        