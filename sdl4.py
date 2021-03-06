# -*- coding: utf-8 -*-
"""sdl4

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1Us7pPl_N2PvcXFxDf1Kp7jHN18W5hQbz
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from google.colab import files
uploaded=files.upload()

data=pd.read_csv("universities.csv")
data.iloc[:11,:]  #use of multi axes indexing method

#detecting missing values 
print("detecting missing values in column of central university")

print(data['Central University'].isnull())

# replace "NaN" with "0".
print("replacing nan with 0")
d1=data.fillna(0)
print(d1)

# pad - fill method forward
print (data.fillna(method='pad'))

#rows get excluded which contained "NAN"
print (data.dropna())