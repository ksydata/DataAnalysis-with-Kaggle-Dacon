# -*- coding: utf-8 -*-
"""DataAnalysis&Utilization.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/163oh4-2H5f3beaq7V1EZs3GR5ggkCA-o

# **데이터 분석 및 활용 Australian Housing Prices 데이터 분석 과제**

The dataset contains information on 1000 properties in Australia, including location, size, price, and other details

```
1. Kaggle notebook의 GCS(Google Cloud Storage) PATH를 Collab에서 경로 지정

!pip install kaggle --upgrade
!pip install gcsfs
    # 구글 클라우드 스토리지 경로를 불러오기 위해 설치
from kaggle_datasets import KaggleDatasets
GCS_DS_PATH = KaggleDatasets().get_gcs_path("RealEstateAU_1000_Samples")
print(GCS_DS_PATH)
    # Kaggle notebook의 GCS(Google Cloud Storage) PATH를 Collab에서 불러옴
GCS_DS_PATH = "gs://'''Kaggle에서 복사한 경로 붙여넣기'''
train = pd.read_csv(GCS_DS_PATH + "/train.csv")
```

```
2. kaggle.json 위치변경 및 실행

!pip install -q kaggle
    # Kaggle API 설치
!mkdir -p ~/.kaggle
    # Kaggle directory 만들기
from google.colab import files
    # google collab API key 불러오기
uploaded = files.upload
    # 다운로드한 kaggle.json file 업로드

! cp kaggle.json ~/.kaggle
    # Kaggle directory의 API key 복사 
!chmod 600 ~/.kaggle/kaggle.json 
    # disable API key 
    # {"username":"sooyeonknunsong","key":"6f920cdd3a94110643504ef606a7af8a"}
!kaggle datasets download -d thedevastator/australian-housing-data-1000-properties-sampled
    # 데이터 셋 다운로드
!unzip -q /content/australian-housing-data-1000-properties-sampled.zip
    # 파일 압축 풀기
```
"""

import pandas as pd
import missingno as msno
import re
import numpy as np

from sklearn import *
from sklearn.metrics import *
from sklearn.model_selection import train_test_split

from matplotlib.pyplot import figure
import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import OneHotEncoder
from sklearn.impute import KNNImputer

HP = pd.read_csv("/content/drive/MyDrive/Colab Notebooks/RealEstateAU_1000_Samples.csv", index_col = 0,  na_values = ["NaN"])
    # Australian Housing Price pandas.DataFrame

CodeBook = pd.DataFrame({"features" :
                         ["breadcrumb", "category_name", "property_type", "building_size", "land_size", "preferred_size","open_date",
                         "listing_agency", "price", "location_number", "location_type",
                         "location_name", "address", "address_1", "city", "state", "zip_code", "phone",
                         "latitude", "longitude", "product_depth", "bedroom_count", "bathroom_count", "parking_count", "RunDate"],
                         
                         "descripts" : 
                         ["A breadcrumb is a text trail that shows the user's location within a website. (String)", 
                         "The name of the category that the listing belongs to. (String)",
                         "The type of property being listed. (String)",
                         "The size of the property's building, in square meters. (Numeric)",
                         "The size of the property's land, in square meters. (Numeric)",
                         "The preferred size of the property, in square meters. (Numeric)",
                         "The date that the property was first listed for sale. (Date)",
                         "The agency that is listing the property. (String)",
                         "The listing price of the property. (Numeric)",
                         "The number that corresponds to the property's location. (Numeric)",
                         "The type of location that the property is in. (String)",
                         "The name of the location that the property is in. (String)",
                         "The property's address. (String)",
                         "The first line of the property's address. (String)",
                         "The city that the property is located in. (String)",
                         "The state that the property is located in. (String)",
                         "The zip code that the property is located in. (String)",
                         "The listing agent's phone number. (String)",
                         "The property's latitude. (Numeric)",
                         "The property's longitude. (Numeric)",
                         "The depth of the product. (Numeric)",
                         "The number of bedrooms in the property. (Numeric)",
                         "The number of bathrooms in the property. (Numeric)",
                         "The number of parking spaces in the property. (Numeric)",
                         "The date that the listing was last updated. (Date)"]
})

pd.set_option("display.max_columns", 10)
pd.set_option("display.max_row", 10)
CodeBook

HP.set_index("TID", inplace=True)
    # 인덱스(행 번호)를 TID 열로 지정

HP.info()

msno.bar(HP)

"""문자열의 $ 제거 및 문자형에서 연속형으로 자료형 변환

* building size
* land_size
* preferred_size
* price
* product_depth


결측값 대체하는 보간법으로 MICE(다중대체법), KNN imputer(k-최근접 이웃 평균값), 다항식 다중선형회귀 예측값 중 선택
* building size
* land_size
* preferred_size

* bedroom_count
* bathroom_count
* parking_count

결측값 대체가 어려운 변수의 행(관측값) 제거
* address
* address_1

결측값 대체가 어려운 변수의 열 제거
* open_date
* latitude
* longitude

날짜형 변수인 RunDate의 시계열 데이터 결측값을 대체하는 보간법
"""

pd.set_option("display.max_columns", None)
HP

"""#### 결측값 대체가 어려운 변수의 열 제거
* open_date
* latitude
* longitude
"""

HP.drop(["open_date", "latitude", "longitude"], axis = 1, inplace = True)
    # 결측값 대체가 어려운 변수 제거



"""### 문자열의 $ 제거 및 문자형에서 연속형으로 자료형 변환

* building size
* land_size
* preferred_size
* price
* product_depth
"""

HP.building_size.unique

pd.set_option("display.max_row", 10)
HP.building_size.str.extract(r"(ha)") == True
  # HP 데이터프레임의 building_size 열의 r"(ha)" 헥타르가 포함된 행 출력 (True False)
HP["building_size"] = HP["building_size"].str.replace("m²", "")
HP["building_size"] = HP["building_size"].str.replace(",", "")
  # 헥타르가 포함된 행 없으므로 (False) m²단위 제거하고 float로 자료형 변환
HP["building_size"] = HP["building_size"].astype("float")

HP["land_size"] = HP["land_size"].str.replace("m²", "")

HP["land_size"] = HP["land_size"].str.replace(",", "")

HP.land_size.str.contains(r"(m²)")

HP["preferred_size"] = HP["preferred_size"].str.replace("m²", "")

HP["preferred_size"] = HP["preferred_size"].str.replace(",", "")

pd.set_option("display.max_row", None)
HP[HP.land_size.str.contains(r"(ha)") == True].index.values
    # ha와 m²의 단위 차이
    # 헥타르(hectare, ㏊)는 10,000 제곱미터(100m×100m)
    # 헥타르 단위의 land_size 또는 preferred_size를 이상치로 판정하고 결측값 NaN 처리

land_size_ha = HP[HP.land_size.str.contains(r"(ha)") == True].index.values

len(land_size_ha)

HP[HP.preferred_size.str.contains(r"(ha)") == True].index.values

preferred_size_ha = HP[HP.preferred_size.str.contains(r"(ha)") == True].index.values

len(preferred_size_ha)

land_size_ha_list = []

for i in range(0, 45, 1):
    land_size_ha_list.append(land_size_ha[i])
    if i == 44: 
      print(type(land_size_ha_list))
      print(land_size_ha_list)
    else: pass

preferred_size_ha_list = []

for i in range(0, 44, 1):
    preferred_size_ha_list.append(preferred_size_ha[i])
    if i == 43: 
      print(type(preferred_size_ha_list))
      print(preferred_size_ha_list)
    else: pass

# [x for y in land_size_ha_list for x in preferred_size_ha_list if y in x]  
    # 두 리스트의 공통된 행 번호 추출
    # if str.find("") != m²
    # preprocessed_text = re.sub('[^a-zA-Z]', ' ', text)

HP.at[land_size_ha_list, "land_size"] = "NaN"
    # 헥타르 단위의 land_size를 이상치로 판정하고 결측값 NaN 처리

HP.at[preferred_size_ha_list, "preferred_size"] = "NaN"
    # 헥타르 단위의 preferred_size를 이상치로 판정하고 결측값 NaN 처리

HP = HP.astype({
    "land_size":"float",
    "preferred_size":"float"
})

HP.info()

pd.set_option("display.max_columns", None)
pd.set_option("display.max_row", 10)
HP
   # location_name 변수의 관측값(변량)과 price 변수의 관측값이 동일한 문제

HP.drop("location_name", axis = 1, inplace = True)
   # location_name이 잘못 저장되었다고 판단 하에 location_name 변수 열 제거

HP[HP.price.str.contains(r"(2 Residence)") == True].index.values



# Trial & Error

# HP["price"] = HP["price"].str.replace("369 000", "369000")
'''
HP["price"] = HP["price"].str.strip()
HP["price"] = HP["price"].str.lstrip()
    # 좌우 공백 제거
HP["price"] = HP["price"].str.replace("$", "")
HP["price"] = HP["price"].str.replace(",", "")
HP["price"] = HP["price"].str.replace("+", "")
    # price 변수의 문자열 $ 및 , 화폐 단위 제거
HP["price"] = HP["price"].str.replace("Offersover", "")
HP["price"] = HP["price"].str.replace("Offers over ", "")
HP["price"] = HP["price"].str.replace("Offers Over ", "")
HP["price"] = HP["price"].str.replace("OversOver", "")
HP["price"] = HP["price"].str.replace("Overs Over", "")
HP["price"] = HP["price"].str.replace("Considered", "")
HP["price"] = HP["price"].str.replace("PRICEGUIDE", "")
HP["price"] = HP["price"].str.replace("PRICE GUIDE", "")
HP["price"] = HP["price"].str.replace("FASTRAK", "NaN")
HP["price"] = HP["price"].str.replace("UNDERCONTRACT", "NaN")
HP["price"] = HP["price"].str.replace("UNDER CONTRACT", "NaN")
HP["price"] = HP["price"].str.replace("2Residence", "NaN")
HP["price"] = HP["price"].str.replace("OpennNegotiation", "NaN")
HP["price"] = HP["price"].str.replace("Openn Negotiation", "NaN")
    # Offers over 문자열 및 2 Residence 집 가격과 무관한 바 제거
'''

# HP[HP.price.isnull()]
    # [ValueError] invalid literal for int() with base 10: 'NaN' 

# HP[HP.price.str.contains(r"(NaN)") == True].index.values
# price_NaN = HP[HP.price.str.contains(r"(NaN)") == True].index.values
# len(price_NaN)
'''
price_NaN_list = []

for i in range(0, 15, 1):
    price_NaN_list.append(price_NaN[i])
    if i == 43: 
      print(type(price_NaN_list))
      print(price_NaN_list)
    else: pass
'''
# HP.at[price_NaN_list, "price"] = "NaN"

# HP["price"] = HP["price"].str.replace(pat = r'.',repl = r' ', regex=True)
# HP["price"] = re.sub(r"[^0-9]", "", HP["price"])

HP.price = HP.price.str.extract('(\d+)')

HP["price"] = HP["price"].astype("float")

HP.product_depth = HP.product_depth.str.extract('(\d+)')

HP["product_depth"] = HP["product_depth"].astype("float")
    # product_depth는 문자 제거 후 숫자만 남기면 모두 NaN으로 결측인바 변수 열 제거

HP.drop(["product_depth"], axis = 1, inplace = True)

"""### 자료형 변환(문자형에서 범주형)"""

HP["RunDate"] = pd.to_datetime(HP["RunDate"])

HP = HP.sort_values(by = "RunDate")

HP["breadcrumb"].unique()
    # category
    # 웹사이트 내에서 사용자의 위치를 보여주는 텍스트 흔적

HP["category_name"].unique()
    # category & TARGET variable

HP["property_type"].unique()
    # category

HP["location_type"].unique()
   # 유형이 Buy 하나인바 변수 열 제거
HP.drop("location_type", axis = 1, inplace = True)

# HP["listing_agency"].unique()
    # object

HP["city"].unique()
    # category

HP["state"].unique()
    # 유형이 Buy 하나인바 변수 열 제거
HP.drop("state", axis = 1, inplace = True)

msno.bar(HP)

HP = HP.astype({
    "breadcrumb":"category",
    "category_name":"category",
    "property_type":"category",
    "city":"category"
})

HP.info()

"""### 연속형 변수 상관분석 후 KNN을 이용한 결측값 예측하여 보간/대체"""

corr = HP.corr()
mask = np.zeros_like(HP.corr(), dtype = np.bool)
mask[np.triu_indices_from(mask)] = True
    # 상관분석 히트맵 삼각형 마스크(위 쪽 삼각형에 True, 아래 삼각형에 False)
sns.heatmap(data = corr, annot = True, fmt = ".1f", cmap = "YlGnBu", mask = mask)

"""

```
* 상관분석 결과 building_size와의 상관계수가 0.4 이상인 prefered를 이용하여 결측값 대체
* 상관분석 결과 land_size, preferred_size와의 상관계수가 0.4 이상인 parking_count, bedroom_count를 이용하여 결측값 대체
```

"""

NA_impute = KNNImputer()
    # NA값의 최근접 k개의 평균으로 결측값을 대체하는 k-nearest neighbor 보간법

numeric_col = HP.select_dtypes(include = ["int64", "float64"]).columns.values

numeric_col_list = []
for i in range(0, 9, 1):
  numeric_col_list.append(numeric_col[i])

numeric_col_list.pop(5)

print(numeric_col_list, end = ",")

NA_impute.fit(HP.loc[:, numeric_col_list])

for col_name in numeric_col_list:
  HP[col_name] = NA_impute.transform(HP.loc[:, numeric_col_list])

HP.isnull().sum()

HP = HP.dropna()
    # address와 address_1 변수의 결측값 12개는 보간하기 어려운바 12개 행 제거

"""### Target 변수인 category_name을 예측하기 위한 이진분류기

해당 데이터는 호주의 주택가격을 예측하는 회귀의 문제로 풀 수 있으나

데이터 분석 및 활용, 본 수업에서는 비선형 회귀분석을 다루지 않은바
분류의 문제로 예측함
"""

x, y = 0, 0

x = HP["category_name"]
y = HP.drop("category_name", axis = 1)

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size = 0.3, random_state = 2023)

