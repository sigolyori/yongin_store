### 맵 시각화 - 칼럼 선택 후 대분류 분포 시각화 (m_filled.html)

# 시각화 하고자 하는 동 선택

yi_1_emd <- yi_1 %>% filter(행정동명 == "풍덕천1동") # 시각화 하고자 하는 동 선택

## 색깔 지정

code_factor <- yi_1_emd %>% 
  distinct(대분류명) %>% 
  select(대분류명) %>% 
  arrange()

code_factor <- as_vector(code_factor[["대분류명"]])

color_factor <- RColorBrewer::brewer.pal(8, "Set2")

color_factor <- as_vector(color_factor)

pal <- colorFactor(color_factor, domain = code_factor)

# 도로명 건물 데이터와 상권 정보 데이터 조인

yi_8 <- left_join(yi_8, emd_list, by = "EMD_CD")

# 칼럼 선택

yi_16_tbl_list  <- yi_select(df = yi_16, "found")

## -------------------------------------------------
# 업종대분류별 시각화
yi_8 <- 
  yi_8 %>% 
  filter(EMD_CD == "101") 

m  <- leaflet()  %>% setView(lng = 127.17810301349549, 
                             lat = 37.24206844684565, 
                             zoom = 10) 

names(yi_16_tbl_list)  %>% 
  walk( function(df){
    pal  <- colorNumeric(
      palette = "Reds",
      domain = yi_16_tbl_list[[df]]$count)
    
    
    m  <<- m  %>% addPolygons(data = yi_16_tbl_list[[df]],
                              group=df,
                              fillOpacity = 0.2,
                              weight = 1,
                              color = ~pal(count))
  })

m  <- m %>%
  addLayersControl(
    baseGroups = names(yi_16_tbl_list),
    options = layersControlOptions(collapsed = FALSE),
    position = "topright"
  )

m  <- m  %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolygons(data = yi_8,
              stroke = TRUE, fill = FALSE, weight = 1,
              color = "#444444") %>% # 건물 경계 추가
  addCircleMarkers(data = yi_1_emd,
                   lng = yi_1_emd$longitude,
                   lat = yi_1_emd$latitude,
                   radius = 0.5,
                   color = pal(yi_1_emd$대분류명),
                   label = htmlEscape(yi_1_emd$표준산업분류명)) %>% # 원 시각화
  addLegend(position = "bottomright",
            pal = pal,
            values = yi_1_emd$대분류명) # 범례 추가

saveWidget(m, "m_filled.html", selfcontained = FALSE)