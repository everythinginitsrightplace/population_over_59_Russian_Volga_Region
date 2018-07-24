library(sf)
library(tidyverse)
library(ggplot2)
library(mapview)
library(rvest)
library(magrittr)
library(rgdal)
library(ggthemes)
library(hrbrthemes)

###Скачиваем данные по демографическому составу населения регионов РФ
shp  <- readOGR("C:/Users/user/Desktop/Population/Rossiya_Demogr.shp", "Rossiya_Demogr", stringsAsFactors=FALSE, encoding="UTF-8")
head(shp)

#Проверка
#plot(shp, axes=TRUE, border="gray")


# Переводим из SpatialPolygonsDataframe в формат dataframe
dfg <- as.data.frame(shp)
head(dfg)


# Загружаем shapefiles по регионам России

Russian_regions <- st_read("C:/Users/user/Desktop/all_shapefields/modern_russia_borders.shp")
head(Russian_regions)


# Отбираем регионы Приволжского федерального округа
Volga_Region_1 <- Russian_regions[Russian_regions$NL_NAME_1 == c("Татарстан", "Чувашия", "Ульяновская область", "Оренбургская область", "Башкортостан", "Саратовская область", "Самарская область", "Марий Эл",
                                                               "Пермский край", "Пензенская область", "Мордовия", "Кировская область", "Нижегородская область", "Удмуртия"),]

Volga_Region_1 <- Russian_regions[Russian_regions$NL_NAME_1 == "Татарстан",]
Volga_Region_1 <- Volga_Region_1[-(1:10), ]
Volga_Region_2 <- Russian_regions[Russian_regions$NL_NAME_1 == "Чувашия",]
Volga_Region_2 <- Volga_Region_2[-(1:10), ]
Volga_Region_3 <- Russian_regions[Russian_regions$NL_NAME_1 == "Ульяновская область",]
Volga_Region_3 <- Volga_Region_3[-(1:10), ]
Volga_Region_4 <- Russian_regions[Russian_regions$NL_NAME_1 == "Оренбургская область",]
Volga_Region_4 <- Volga_Region_4[-(1:10), ]
Volga_Region_5 <- Russian_regions[Russian_regions$NL_NAME_1 == "Башкортостан",]
Volga_Region_5 <- Volga_Region_5[-(1:10), ]
Volga_Region_6 <- Russian_regions[Russian_regions$NL_NAME_1 == "Саратовская область",]
Volga_Region_6 <- Volga_Region_6[-(1:10), ]
Volga_Region_7 <- Russian_regions[Russian_regions$NL_NAME_1 == "Самарская область",]
Volga_Region_7 <- Volga_Region_7[-(1:10), ]
Volga_Region_8 <- Russian_regions[Russian_regions$NL_NAME_1 == "Марий Эл",]
Volga_Region_8 <- Volga_Region_8[-(1:10), ]
Volga_Region_9 <- Russian_regions[Russian_regions$NL_NAME_1 == "Пермский край",]
Volga_Region_9 <- Volga_Region_9[-(1:10), ]
Volga_Region_10 <- Russian_regions[Russian_regions$NL_NAME_1 == "Пензенская область",]
Volga_Region_10 <- Volga_Region_10[-(1:10), ]
Volga_Region_11<- Russian_regions[Russian_regions$NL_NAME_1 == "Мордовия",]
Volga_Region_11 <- Volga_Region_11[-(1:10), ]
Volga_Region_12 <- Russian_regions[Russian_regions$NL_NAME_1 == "Кировская область",]
Volga_Region_12 <- Volga_Region_12[-(1:10), ]
Volga_Region_13 <- Russian_regions[Russian_regions$NL_NAME_1 == "Нижегородская область",]
Volga_Region_13 <- Volga_Region_13[-(1:10), ]
Volga_Region_14 <- Russian_regions[Russian_regions$NL_NAME_1 == "Удмуртия",]
Volga_Region_14 <- Volga_Region_14[-(1:10), ]

# Делаем одну таблицу со всеми 14 регионами ПФО
Volga_Region <- rbind(Volga_Region_1, Volga_Region_2, Volga_Region_3, Volga_Region_4, Volga_Region_5,
                      Volga_Region_6, Volga_Region_7, Volga_Region_8, Volga_Region_9, Volga_Region_10,
                      Volga_Region_11, Volga_Region_12, Volga_Region_13, Volga_Region_14)

# Проверяем посредством построения карты ПФО
ggplot(data = Volga_Region) + 
  geom_sf() + 
  scale_fill_viridis_c()

### Далее работаем с таблицей,в которой представлены данные по возрасту населения ПФО
dfg_volga <- dfg[dfg$FEDERAL_DI == "Приволжский федеральный округ",]
dfg_volga[3,1] <- "Удмуртия"
dfg_volga[4,1] <- "Марий Эл"
dfg_volga[6,1] <- "Башкортостан"
dfg_volga[7,1] <- "Мордовия"
dfg_volga[9,1] <- "Чувашия"
dfg_volga[10,1] <- "Татарстан"


# Объединяем два получившихся датафрейма
temp_1  <- merge(Volga_Region, dfg_volga, by.x = "NL_NAME_1", by.y = "NAME") 
names(temp_1)[27] <- "Жители_региона_старше_59"
colnames(temp_1)


picture <-  ggplot(data = temp_1, aes(fill = `Жители региона старше 59 лет, %`)) + 
  geom_sf() + 
  labs(title="Приволжский федеральный округ")+
  scale_fill_viridis_c(direction = -1) +
  #guide_legend("Жители региона старше 59 лет")+
  theme_modern_rc()
  


temp_1%>% 
  mapview(zcol = "Жители_региона_старше_59", label = .$NL_NAME_1, legend = TRUE, col.regions = mapviewGetOption("plainview.maxpixels"))


