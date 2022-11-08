### 맵 시각화 - 블록별 업종 개수

count_gid_total  <- read_csv("count_gid_total.csv")

count_gid_total  <- count_gid_total  %>% 
  rename(., count = total_x)  %>% 
  rename(., sales = total_y)

count_gid_total$geometry  <- st_as_sfc(count_gid_total$geometry, crs = 4326)

count_gid_total  <- st_as_sf(count_gid_total)

count_gid_total  <- count_gid_total  %>% 
  mutate(centroid = st_centroid(count_gid_total[["geometry"]])  %>%
           st_coordinates(.)  %>% 
           as_tibble())

# 대분류코드에 따라서 count_gid_total 데이터를 개별의 리스트로 나눔

count_gid_total_list  <- split(count_gid_total, count_gid_total$대분류코드)

m <- leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter)

names(count_gid_total_list) %>%
  purrr::walk( function(df) {
    
    pal  <<- colorNumeric(
      palette = "Blues",
      domain = count_gid_total_list[[df]]$sales) 
    
    m <<- m %>%
      addPolygons(data = count_gid_total_list[[df]],
                  group=df,
                  weight = 1,
                  stroke = TRUE)  %>% 
      addCircleMarkers(data = count_gid_total_list[[df]],
                       lng = ~centroid$X,
                       lat = ~centroid$Y,
                       radius = ~log2(count),
                       color = ~pal(sales),
                       group = df,
                       label = paste("사업체 수:", count_gid_total_list[[df]]$count,
                                     "\n", "매출액:", count_gid_total_list[[df]]$sales)
      )
  })

m  <- m %>%
  addLegend(position = "bottomright",
            pal = pal,
            values = count_gid_total$sales,
            title = "블록별 매출액"
  )   

m  <- m %>%
  addLayersControl(
    baseGroups = names(count_gid_total_list),
    options = layersControlOptions(collapsed = FALSE),
    position = "bottomright"
  )   


saveWidget(m, file = "m_category.html",
           selfcontained = FALSE)