# 읍면동 경계 데이터 불러오기
yi_13 <- read_sf(dsn = "13.용인시_행정경계(읍면동).geojson")

# 13번 데이터와 16번 데이터 조인
yi_16_yi_13  <- st_join(yi_13, yi_16, left = TRUE)

# 읍면동별 휴폐업률 계산 
yi_16_yi_13_runout  <- yi_16_yi_13  %>% 
  select(ADM_DR_NM, ws_cnt, runout_cnt, geometry)  %>% 
  mutate(runout_percentage = runout_cnt / (runout_cnt + ws_cnt))  %>% 
  group_by(ADM_DR_NM)  %>% 
  summarise(mean = mean(runout_percentage, na.rm = TRUE) * 100)  %>% 
  arrange(desc(mean))

### 지도 시각화
# 맵 생성
m  <- leaflet(yi_16_yi_13_runout)  %>% addProviderTiles(providers$CartoDB.DarkMatter)

# 색깔 palette 지정
pal  <- colorNumeric(
  palette = "Blues",
  domain = yi_16_yi_13_runout$mean) 

# 다각형 맵에 추가
m  <- m  %>% addPolygons(color = ~pal(mean),
                         popup = ~mean,
                         options = popupOptions(keepInView = TRUE),
                         label = ~ADM_DR_NM,
                         weight = 0.5,
                         fillOpacity = 0.8,
                         labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE,
                                                     style = list(
                                                       "color" = "black")))  %>% 
  addLegend(
    position = "bottomright",
    pal = pal,
    value = yi_16_yi_13_runout$mean,
    labFormat = labelFormat(prefix = "%"),
    title = "폐업률"
  )


# m_runout.html 파일로 저장
saveWidget(m, "m_runout.html", selfcontained = FALSE)
