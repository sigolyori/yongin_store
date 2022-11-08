library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(RColorBrewer)
library(reshape)

#지표 생성을 위한 코드, 전처리

Maechool <-read.csv('17.용인시_소상공인_매출정보.csv')
#사업체수 5 미만인 행 제거
Mae2 <- subset(Maechool, Maechool$ws_cnt >= 5)

#격자당 매출을 모두 합하여 저장, 20년도 데이터가 3분기까지밖에 없으므로 다른 연도도 3분기까지 통일
sale_17 <- Mae2$sales_est_amt_201703 + Mae2$sales_est_amt_201706 + Mae2$sales_est_amt_201709
sale_18 <- Mae2$sales_est_amt_201803 + Mae2$sales_est_amt_201806 + Mae2$sales_est_amt_201809
sale_19 <- Mae2$sales_est_amt_201903 + Mae2$sales_est_amt_201906 + Mae2$sales_est_amt_201909
sale_20 <- Mae2$sales_est_amt_202003 + Mae2$sales_est_amt_202006 + Mae2$sales_est_amt_202009

#빈 데이터프레임 생성
sales<-data.frame(x = 3, y = 1:807)

#각 열에 연도별 데이터 저장
sales[,1] <- sale_17
sales[,2] <- sale_18
sales[,3] <- sale_19
sales[,4] <- sale_20

#열 이름 지정
colnames(sales) <- c('sales_17','sales_18','sales_19','sales_20')

#증감여부 파악을 위하여 인덱스 지정(각 행별로 전년대비 다음년도에 매출액이 증가하였으면 1 아니면 0)
idx <- ifelse(sales$sales_17 < sales$sales_18, 1, 0)
idx2 <- ifelse(sales$sales_18 < sales$sales_19, 1, 0)
idx3 <- ifelse(sales$sales_19 < sales$sales_20, 1, 0)

# 꾸준히 증가하였으면 3, 한 두번의 감소가 있었으면 2 혹은 1, 감소만 하였으면 0의 값을 가지는 열을 지정
sales[,5] <- idx + idx2 + idx3

#격자별 평균 업년을 구하기 위해 가중평균을 위한 변수들을 지정
repage2 <- Mae2$found_age_1*1 + Mae2$found_age_2*2 +Mae2$found_age_3*3 + Mae2$found_age_4*4 + Mae2$found_age_5*5 + Mae2$found_age_6*6
repage<- Mae2$found_age_1 + Mae2$found_age_2 +Mae2$found_age_3 + Mae2$found_age_4 + Mae2$found_age_5 + Mae2$found_age_6

#가중평균을 구한 뒤 열에 지정
sales[,6] <-repage2/repage

#열 이름 지정
colnames(sales)[5:6] <- c('Degree', 'Mean_woonyoung')

#첫째에서 반올림
sales[,6] <- round(sales[,6],1)

#매출액 증감여부가 3이면 1, 1이나 2이면 2, 0이면 3을 부여 
idx <- sales[,5] == 3
sales[idx,7] <- 1  
idx2 <- sales[,5] == 1 | sales[,5] == 2
sales[idx2,7] <- 2
idx3 <- sales[,5] == 0
sales[idx3,7] <- 3

#평균업년수가 2.85미만이면 1, 2.85이상 4.5미만이면 2, 4.5이상이면 3을 부여
idx <- sales[,6] < 2.85
sales[idx,8] <- 1  
idx2 <- sales[,6] >= 2.85 & sales[,6] < 4.5
sales[idx2,8] <- 2
idx3 <- sales[,6] >= 4.5
sales[idx3,8] <- 3

#위에서 부여한 값들을 곱하여 새로운 열에 지정
sales[,9] <- sales[,7] + sales[,8]

#평균업년수와 매출액 증감여부에서 도출한 값들이 4이하이면 1, 5이면 2, 6이면 3을 부여 
idx <- sales[,9] <= 4
sales[idx,10] <- 1
idx <- sales[,9] == 5
sales[idx,10] <- 2  
idx <- sales[,9] == 6
sales[idx,10] <- 3

#1 = 성장기, 2 = 성숙기, 3 = 쇠퇴기를 의미한다.

#맵을 그리기 위한 코드
#위에서 구한 생애주기에 gid를 갖다넣음
sales[,11] <- Mae2[,1]
#gid를 바탕으로 격자별 시각화를 위하여 geojosn값을 불러온다.
sales2 <- read_sf(dsn = "16.용인시_소상공인_매출정보.geojson")
sales3<-sales2[,c(1,67)]

#열이름 지정
colnames(sales)[11] <- 'gid'
#gid를 바탕으로 Multipolygon값을 얻기위해 병합
DF <- merge(sales3, sales, by = 'gid', all = TRUE)
#시각화에 필요한 값들만 추출
DF2<-DF[,c(1,11,12)]

#열이름 지정
colnames(DF2)[2] <- 'lifecycle'

df_1 <- DF2
#NA값들의 위치를 지정
idx <- is.na(df_1$lifecycle)
#NA값들이 있는 행은 제거
df2 <- df_1[idx==FALSE,]

# 변수 이름 바꾸어 지정

df_lifecycle  <- DF2

yi_13 <- read_sf(dsn = "13.용인시_행정경계(읍면동).geojson")

yi_16 <- read_sf(dsn = "16.용인시_소상공인_매출정보.geojson")

# sales 데이터 준비

yi_16_tbl  <- yi_16  %>% 
  tibble()  %>% 
  select(gid, starts_with("sales"), geometry)  %>% 
  pivot_longer(starts_with("sales"), names_to = "sales", values_to = "count")  %>% # 여러 칼럼을 하나의 칼럼으로 합치기
  filter(is.na(count) == FALSE)   

yi_16_tbl  <- yi_16_tbl  %>%  group_by(gid)  %>% 
  summarise(sum = sum(count, na.rm = TRUE))   %>% 
  ungroup()

# 유동인구 데이터 준비

yi_7  <- read_csv("7.용인시_유동인구.csv", col_names = TRUE, col_types = NULL)

# tmst 칼럼들을 행으로 합치기 
yi_7_tbl  <- pivot_longer(yi_7, starts_with("TMST"), names_to = "tmst", values_to = "count")

# 독립된 위, 경도 별로 유동인구 수 합치기
yi_7_count  <- yi_7_tbl  %>% 
  group_by(lon, lat)  %>% 
  summarise(mean = mean(count, na.rm = TRUE))  %>% 
  arrange(desc(mean))  %>% 
  ungroup()

# sf 형식으로 변환
yi_7_count  <- st_as_sf(yi_7_count, coords = c("lon", "lat"), crs = 4326)

# 두 sf 형식을 조인
lifecycle_yi_13  <- st_join(yi_13, df_lifecycle, left = TRUE)

# gid를 기준으로 lifecycle_yi_13와 yi_16_tbl을 left join
lifecycle_yi_13  <- left_join(lifecycle_yi_13, yi_16_tbl, by = "gid")

# NA 값은 필터링
lifecycle_yi_13  <- lifecycle_yi_13  %>% 
  filter(!is.na(lifecycle) == TRUE)

# 16번 데이터에서 격자와 geometry 정보만 추출

yi_16_gid  <- yi_16  %>% select(gid, geometry)  %>% distinct(gid)

# 격자 정보(Polygon)와 유동 인구 위, 경도(Point) 조인 
yi_7_count  <- st_join(yi_16_gid, yi_7_count, join = st_nearest_feature)

lifecycle_yi_13  <- lifecycle_yi_13  %>% as_tibble()

# gid로 두 데이터프레임 조인
lifecycle_yi_13  <- left_join(lifecycle_yi_13, yi_7_count, by = "gid")

# 법정동을 기준으로 유동인구, 매출 정보를 그룹화
lifecycle_table  <- lifecycle_yi_13  %>%
  select(gid, ADM_DR_NM, mean, sum)  %>%
  rename(.,c(mean = "population", sum = "sales"))  %>% 
  mutate(sum_each = population + sales)  %>% 
  group_by(ADM_DR_NM)  %>% 
  summarise(sum_pop = sum(population, na.rm = TRUE), 
            sum_sales = sum(sales, na.rm = TRUE),
            sum_all = sum(sum_each, na.rm = TRUE))  %>% 
  ungroup()

lifecycle_yi_13   <- lifecycle_yi_13  %>% 
  filter(!is.na(lifecycle) == TRUE)

# 읍면동 별 성장기, 성숙기, 쇠퇴기 구함
lifecycle_adm_count  <- lifecycle_yi_13  %>% 
  group_by(ADM_DR_NM, lifecycle)  %>% 
  summarise(n = n())

# 테이블 정리

# 칼럼 이름 재정의, NA 값은 0으로
lifecycle_adm_count  <- lifecycle_adm_count  %>% 
  pivot_wider(names_from = lifecycle, values_from = n)  %>% 
  rename(., c(`1` = "성장기", `2` = "성숙기", `3` = "쇠퇴기"))  %>% 
  ungroup()  %>% 
  replace_na(list(쇠퇴기 = 0))

# 조인

df_adm  <- left_join(lifecycle_table, lifecycle_adm_count, by = "ADM_DR_NM")

# 정규화

# 정규화, 칼럼 위치 조정
df_adm  <- df_adm  %>% 
  mutate(
    sum_pop_norm = (sum_pop - min(sum_pop)) / (max(sum_pop) - min(sum_pop)),
    sum_sales_norm = (sum_sales - min(sum_sales)) / (max(sum_sales) - min(sum_sales))
  )  %>% 
  relocate(성장기, 성숙기, 쇠퇴기, .after = last_col())

# df_adm.csv로 내보내기
write_csv(df_adm, "df_adm.csv")