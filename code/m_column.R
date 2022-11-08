### 맵 시각화 - 칼럼 선택 후 시각화 (m_column.html)

yi_16_tbl_list  <- yi_select(df = yi_16, column = "found") # found로 시작하는 칼럼 선택

l <- leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) # map 개체 생성

names(yi_16_tbl_list) %>% 
  purrr::walk( function(df) { # list 별로 반복
    
    pal  <- colorNumeric(
      palette = "Blues",
      domain = yi_16_tbl_list[[df]]$count) 
    
    l <<- l %>%
      addPolygons(data = yi_16_tbl_list[[df]],
                  label=~count,
                  group=df,
                  labelOptions = labelOptions(noHide = F,
                                              direction = 'auto'),
                  weight = 1,
                  color = ~pal(count),
                  stroke = FALSE)
  })

l  <- l %>%
  addLayersControl(
    baseGroups = names(yi_16_tbl_list),
    options = layersControlOptions(collapsed = FALSE),
    position = "bottomright"
  )   


saveWidget(l, file = "m_column.html",
           selfcontained = FALSE)