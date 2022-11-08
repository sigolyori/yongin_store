library(showtext)
# 한글 위한 폰트 임포트
font_add_google('Noto Sans KR', 'notosanskr')

showtext_auto()

# 데이터 불러오기
yi_3 <- read_sf(dsn = "3.용인시_인구정보(총인구수)_격자.geojson")
yi_4 <- read_sf(dsn = "4.용인시_인구정보(고령)_격자.geojson")
yi_5 <- read_sf(dsn = "5.용인시_인구정보(생산가능)_격자.geojson")
yi_6  <-  read_sf(dsn = "6.용인시_인구정보(유소년)_격자.geojson")

# type 칼럼 만들기
type <- c("총인구수", "고령", "생산가능", "유소년")
name_type <- function(df, type_num){
  df["type"] <- type[type_num] 
  
  return(df)
}

yi_3 <- name_type(yi_3, 1)
yi_4 <- name_type(yi_4, 2)
yi_5 <- name_type(yi_5, 3)
yi_6 <- name_type(yi_6, 4)

# 데이터프레임 합치기
yi_pop <- bind_rows(yi_3, yi_4, yi_5, yi_6)

yi_12 <- read_sf(dsn = "12.용인시_법정경계(읍면동).geojson")

# 격자를 동별로 합치기
yi_12_yi_pop <- st_join(yi_12, yi_pop, 
                        left = TRUE)  

# NA값을 0으로 변경
yi_12_yi_pop <- yi_12_yi_pop %>% 
  replace_na(list(val = 0))

# 인구비율 계산
yi_pop_list  <- yi_12_yi_pop  %>% 
  tibble()  %>% 
  pivot_wider(names_from = type, values_from = val)  %>%
  filter(총인구수 != 0)  %>% 
  mutate(high = 고령/총인구수, mid = 생산가능/총인구수, low = 유소년/총인구수)  %>% 
  group_by(EMD_KOR_NM)  %>% 
  summarise(mean_high = mean(high),
            mean_mid = mean(mid),
            mean_low = mean(low))  %>% 
  gather(., "type", "val", 2:4)

yi_pop_list  <- left_join(yi_pop_list, yi_12)

# 시각화 준비
# 타입과 동별로 그룹
adm_pop <- yi_12_yi_pop %>%
  group_by(EMD_KOR_NM, type) %>% 
  summarise(sum = sum(val, na.rm = TRUE)) %>%
  arrange(desc(sum)) 

adm_pop_all <- yi_12_yi_pop %>%
  filter(type == "총인구수") %>% 
  group_by(EMD_KOR_NM, type) %>% 
  summarise(sum = sum(val, na.rm = TRUE)) %>%
  arrange(desc(sum)) 

adm_pop_all$EMD_KOR_NM <- as_factor(adm_pop_all$EMD_KOR_NM)

# Bar chart 생성
g_pop <- ggplot(data = adm_pop_all, aes(EMD_KOR_NM, 
                                        fct_reorder(EMD_KOR_NM, sum,
                                                    .desc = FALSE))) +
  geom_bar(stat = "identity", 
           fill = NA,
           color = "black") +
  scale_y_discrete(name = "인구 수",
                   breaks = c(10000, 20000,
                              30000, 40000,
                              50000, 60000),
                   labels = c(10000, 20000,
                              30000, 40000,
                              50000, 60000)) +
  geom_bar(stat = "identity", data = adm_pop, aes(EMD_KOR_NM, 
                                                  fct_reorder(EMD_KOR_NM, sum,
                                                              .desc = FALSE),
                                                  fill = type)) +
  theme(axis.text.x = element_text(
    angle = 90, vjust = 0.5, hjust = 1
  ))
g_pop

# ggsave("g_pop.png")

yi_pop_list  <- st_as_sf(yi_pop_list, crs = 4326)

# 맵 생성
yi_pop_list_split <- yi_pop_list  %>% 
  filter(type == "mean_mid")

l <- leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels) # map 개체 생성


pal  <- colorNumeric(
  palette = "Greens",
  domain = yi_pop_list_split$val) 

l <- l %>%
  addPolygons(data = yi_pop_list_split,
              label=~round(val, 2) * 100,
              group=df,
              labelOptions = labelOptions(noHide = F,
                                          direction = 'auto'),
              weight = 1,
              color = ~pal(val),
              fillOpacity = 0.8,
              stroke = FALSE)

l  <- l %>%
  addPolygons(data = yi_12,
              weight = 1,
              color = "white", 
              fill = FALSE,
              label = ~EMD_KOR_NM,
              labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE,
                                          style = list(
                                            "color" = "black")))  %>%
  addLegend(position = "bottomright",
            pal = pal,
            values = yi_pop_list_split$val)

saveWidget(l, file = "m_population_level.html",
           selfcontained = FALSE)