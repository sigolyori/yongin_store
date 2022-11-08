### 맵 시각화 - 업종별 칼럼 선택 후 시각화 (indcd_yn_html)

# indcd_alphabet_yn 칼럼 이름 만들기

letters_vec  <- letters[1:21]

# as_alphabet 함수 정의  
# 업종이 존재하는 격자 Row만 필터

as_alphabet  <- function(nm){
  yi_16_alphabet  <- yi_16  %>% tibble()  %>% 
    drop_na(any_of(nm))  %>% 
    st_as_sf()
  
  return (yi_16_alphabet)
}

indcd_col  <- function(x){
  indcd_yn  <- paste0("indcd_",x,"_yn")
  
  return (indcd_yn)
}

indcd_names  <- map_chr(letters_vec, indcd_col)

# 맵 시각화

m  <- leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter)
for (nm in indcd_names){ # 업종 존재하는 만큼 반복
  
  yi_16_alphabet  <- as_alphabet(nm)
  
  # 시각화 하고자 하는 column을 선택
  
  yi_16_tbl_list  <- yi_select(yi_16_alphabet, "age")
  
  if (length(yi_16_tbl_list)){
    names(yi_16_tbl_list)  %>% 
      walk( function(df){
        pal  <- colorNumeric(
          palette = "Blues",
          domain = yi_16_tbl_list[[df]]$count) # 색깔 정보 있는 pal 정의
        
        
        m  <<- m  %>% addPolygons(data = yi_16_tbl_list[[df]],
                                  label=~count,
                                  group=df,
                                  fillOpacity = 0.6,
                                  labelOptions = labelOptions(noHide = F,
                                                              direction = 'auto'),
                                  weight = 1,
                                  color = ~pal(count)) # Polygon 시각화
      })
    m  <<- m %>%
      addLayersControl(
        baseGroups = names(yi_16_tbl_list),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomright" # Layer control 추가
      )
    saveWidget(m, file = paste0(nm,".html"),
               selfcontained = FALSE) # html 파일로 저장
  } else {
    next
  }
}