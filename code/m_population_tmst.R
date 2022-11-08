### 맵 시각화 - 유동인구 데이터 (m_population.html, m_populaton_tmst.html)

yi_7  <- read_csv("7.용인시_유동인구.csv", col_names = TRUE, col_types = NULL)

# 읍면동 기준 유동인구 

yi_7_tbl  <- pivot_longer(yi_7, starts_with("TMST"), names_to = "tmst", values_to = "count")

yi_7_tbl_list  <- split(yi_7_tbl, yi_7_tbl$STD_YM)

# 시간대명 벡터로 추출 (0~24)

yi_7_tmst  <- yi_7  %>% 
  select(starts_with("TMST"))  %>% 
  colnames()

# 읍면동 기준 시각화 (m_population.html)

m  <- leaflet(yi_7_tbl_list) %>% addProviderTiles(providers$CartoDB.DarkMatter)

names(yi_7_tbl_list)  %>% 
  walk(function(df){
    m <<- m  %>% addCircles(data = yi_7_tbl_list[[df]],
                            lng = yi_7_tbl_list[[df]]$lon, lat = yi_7_tbl_list[[df]]$lat, 
                            radius = ~count,
                            label = ~count,
                            group = df)
  })

m  <-  m  %>% addLayersControl(
  baseGroups = names(yi_7_tbl_list),
  options = layersControlOptions(collapsed = FALSE),
  position = "topright"
)

saveWidget(m, file = "m_population.html", selfcontained = FALSE)

# 경도, 위도, 시간대별 데이터 별 총합 구함

yi_7_count  <- yi_7_tbl  %>% 
  group_by(lon, lat, tmst)  %>% 
  summarise(sum = sum(count, na.rm = TRUE))  %>% 
  arrange(desc(sum))

yi_7_count_sf  <- st_as_sf(x = yi_7_count, coords = c("lon", "lat"), crs = 4326)

# 16번 데이터에서 격자와 geometry 정보만 추출

yi_16_gid  <- yi_16  %>% select(gid, geometry)  %>% distinct(gid)

yi_16_yi_7  <- st_join(yi_16_gid, yi_7_count_sf)

# 격자별, 시간대별 유동인구 합 구함

df_table  <- yi_16_yi_7  %>% tibble()  %>% group_by(gid, tmst)   %>%  
  summarise(sum_col = sum(sum, na.rm = TRUE))  %>% ungroup()

# gid를 통해 데이터 조인

df_table   <- left_join(df_table, yi_16_gid, by = "gid")

# geometry를 sfc 형태로 변경

df_table$geometry  <- st_sfc(df_table$geometry, crs = 4326)

df_table  <- st_as_sf(df_table)

df_table_list  <- split(df_table, df_table$tmst)

yi_13 <- read_sf(dsn = "13.용인시_행정경계(읍면동).geojson")

# 7번 데이터에 독립된 위, 경도 Row를 추출

yi_7_coord  <- yi_7  %>% 
  distinct(lon, lat)

# 색 지정 위해서 구분 정함

bins  <- c(10, 50, 150, 300, 900, 2700, 10000, 25000)

# 블록별, 시간대별 유동인구 시각화 (m_population_tmst.html)

m  <- leaflet()  %>% 
  addProviderTiles(providers$CartoDB.DarkMatter)  

names(df_table_list)  %>% 
  walk(function(df){
    
    pal <- colorBin(
      "Blues", 
      domain = df_table_list[[df]]$sum_col,
      bins = bins)
    
    
    m <<- m  %>% addPolygons(
      data = df_table_list[[df]],
      fillColor = ~pal(sum_col),
      weight = 1,
      opacity = 1,
      fillOpacity = 0.7,
      label = ~sum_col,
      group = df,
      stroke = FALSE)
  })

m  <- m %>%
  addLayersControl(
    baseGroups = names(df_table_list),
    options = layersControlOptions(collapsed = FALSE),
    position = "topright"
  )

saveWidget(m, file = "m_population_tmst.html",
           selfcontained = FALSE)