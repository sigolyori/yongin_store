### 라이브러리 호출

library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(RColorBrewer)

### 데이터 호출

yi_1  <- read_csv("1.용인시_상권_정보.csv", col_names = TRUE, col_types = NULL)

# 칼럼 이름 재정의

yi_1  <- reshape::rename(yi_1,
                         c(위도 = "latitude",
                           경도 = "longitude"))

# 데이터프레임을 sf (simple features) 형태로 바꿈

yi_1_sf  <- st_as_sf(yi_1, 
                     coords = c("longitude", "latitude"), 
                     crs = 4326, dim = "XY" )

# 대분류코드명 조인

yi_2  <- read_csv("2.용인시_상권_업종코드.csv",
                  col_names = TRUE, col_types = NULL)

# 대분류코드, 대분류코드명 추출

yi_2_class  <- yi_2  %>%
  distinct(대분류코드, .keep_all = TRUE)  %>%
  select(c(1:2))

# 1번 데이터와 2번 데이터 조인

yi_1  <- left_join(yi_1, yi_2_class, by = "대분류코드")

yi_8 <- read_sf(dsn = "8.용인시_도로명주소_건물.geojson")

# 읍면동 리스트 생성

yi_13 <- read_sf(dsn = "13.용인시_행정경계(읍면동).geojson")

yi_13 <-  yi_13 %>% tibble() %>% 
  select(ADM_DR_CD, ADM_DR_NM)

for (i in 1:nrow(yi_13)){
  emd_list <-  tibble(EMD_CD = str_sub(yi_13$ADM_DR_CD, 
                                       start = 5L),
                      EMD_NM = yi_13$ADM_DR_NM)
}

yi_16 <- read_sf(dsn = "16.용인시_소상공인_매출정보.geojson")

yi_17  <- read_csv("17.용인시_소상공인_매출정보.csv", col_names = TRUE,
                   col_types = NULL)

### 데이터 전처리

# 16번 데이터의 칼럼을 선택하는 yi_select 함수 설정
yi_select  <- function(df, column){
  yi_16_tbl  <- df  %>% 
    tibble()  %>% 
    select(gid, starts_with(column), geometry)  %>% 
    pivot_longer(starts_with(column), names_to = column, values_to = "count")  %>% # 여러 칼럼을 하나의 칼럼으로 합치기
    filter(is.na(count) == FALSE) 
  
  yi_16_tbl  <- st_as_sf(yi_16_tbl)
  
  yi_16_tbl_list <- split(yi_16_tbl, yi_16_tbl[[column]])  # 칼럼에 따라 리스트로 분류
  
  return (yi_16_tbl_list)
}









