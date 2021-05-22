citing = read.csv("/Users/gretacvega/Documents/GitHub/vizzWork/mate/citing.csv")
citing
#collab = read.table("/Users/gretacvega/Documents/GitHub/mate/collab.txt", h = T, sep ="\t")
collab = read.csv("/Users/gretacvega/Documents/GitHub/vizzWork/mate/collab.csv")
head(collab)


library("rnaturalearth")
library("rnaturalearthdata")
library(stringr)
library(dplyr)    
library(tidyr)          # data manipulation
library(purrr)          # data manipulation
#library(cartogram)      # cartograms creation
#library(tmap)           # maps creation
library(sf) 
library(ggplot2)
worldmap <- map_data("world")#, region = europeanUnion)

ggplot(collab %>% 
         mutate(region = str_to_title(collab)) %>% 
         right_join(worldmap, by = "region" ))+
  geom_polygon(aes(long,lat,group=group, fill=Nber), 
               size = 0.1, 
               colour= "#090D2A", 
               alpha=0.8)+
  scale_fill_viridis_c(na.value="white")+
  theme_void()+
  ggtitle("Countries of S. Matesanz's collaborators")

ggplot(citing %>% 
         mutate(region = str_to_title(citing)) %>% 
         right_join(worldmap, by = "region" ))+
  geom_polygon(aes(long,lat,group=group, fill=Nber), 
               size = 0.1, 
               colour= "#090D2A",#090D2A", 
               alpha=0.8)+
  scale_fill_viridis_c(na.value="white")+
  theme_void()+
  coord_fixed(1.3)+
  ggtitle("Countries where S. Matesanz has been cited")

collab = collab %>% 
  mutate(sovereignt = str_to_title(collab)) %>% 
  rename("collab_nber"= "Nber", "percent_collab"= "percent")

dd = citing %>% 
  mutate(sovereignt = str_to_title(citing)) %>% 
  rename("citing_nber" = "Nber" , "percent_citing" = "percent") %>% 
  full_join(collab, by = "sovereignt") %>% 
  select(sovereignt, collab_nber, percent_collab, citing_nber, percent_citing) %>% 
  replace_na(list(collab_nber=0, percent_collab=0, citing_nber=0, percent_citing=0)) %>% 
  mutate(sovereignt = replace(sovereignt, sovereignt == "Usa", "United States of America"))

head(dd)

world_map = ne_countries(returnclass = "sf")
world_map = world_map[,"sovereignt"]
world_data = world_map %>% 
  #select(sovereignt) %>% 
  filter(sovereignt != "Antarctica") %>% 
  st_transform(crs = '+proj=robin') %>% 
  left_join(dd, by = "sovereignt")
world_data2 = world_data %>% 
  select(sovereignt, percent_collab,percent_citing) %>% 
  gather(key = "type", value = "percent", "percent_collab":"percent_citing")


gg_dd = dd %>% mutate(sovereignt = replace(sovereignt, sovereignt == "United States of America", "United States of America"),
                      sovereignt = replace(sovereignt, sovereignt == "Peoples R China", "China"),
                      sovereignt = replace(sovereignt, sovereignt == "England", "United Kingdom"),
                      sovereignt = replace(sovereignt, sovereignt == "Scotland", "United Kingdom"),
                      sovereignt = replace(sovereignt, sovereignt == "Iran", "Iran (Islamic Republic of) "),
                      sovereignt = replace(sovereignt, sovereignt == "Wales", "United Kingdom"),
                      sovereignt = replace(sovereignt, sovereignt == "South Korea", "Korea, Republic of"),
                      sovereignt = replace(sovereignt, sovereignt == "Bosnia Herceg", "Bosnia and Herzegovina"),
                      sovereignt = replace(sovereignt, sovereignt == "Trinidad Tobago", "Trinidad and Tobago"),
                      sovereignt = replace(sovereignt, sovereignt == "Rep Congo", "Democratic Republic of the Congo"),
                      sovereignt = replace(sovereignt, sovereignt == "Tanzania", "United Republic of Tanzania"),
                      sovereignt = replace(sovereignt, sovereignt == "Cote Ivoire", "Cote d'Ivoire"),
                      sovereignt = replace(sovereignt, sovereignt == "Libya", "Libyan Arab Jamahiriya"),
                      sovereignt = replace(sovereignt, sovereignt == "North Ireland", "United Kingdom"),
                      sovereignt = replace(sovereignt, sovereignt == "Papua N Guinea", "Papua New Guinea"),
                      sovereignt = replace(sovereignt, sovereignt == "Vietnam", "Viet Nam")) %>% 
  group_by(sovereignt) %>% 
  summarise_each(sum)

world_data3 = world_map %>% 
  #select(sovereignt) %>% 
  filter(sovereignt != "Antarctica") %>% 
  st_transform(crs = '+proj=robin') %>% 
  left_join(gg_dd, by = "sovereignt")
world_data3 <- cbind(world_data3, st_coordinates(st_centroid(world_data3)))

ggplot(data= world_data3 %>% 
         mutate(clean_collab = ifelse(collab_nber == 0, NA, collab_nber)))+
  geom_sf(aes(fill = clean_collab),size = 0.1, 
          colour= "#090D2A",#090D2A", 
          alpha=0.99)+
  #scale_fill_viridis_c(na.value="white")+
  scale_fill_distiller(palette ="YlGn", na.value="white")+
  theme_void()  

ggplot(data= world_data3 %>% 
         mutate(clean_citing = ifelse(citing_nber == 0, NA, citing_nber)))+
  geom_sf(aes(fill = clean_citing),size = 0.1, 
          colour= "#090D2A",#090D2A", 
          alpha=0.99)+
  #scale_fill_viridis_c(na.value="white")+
  #scale_fill_distiller(palette ="YlGn", na.value="white")+
  scale_fill_distiller(palette ="YlOrRd", na.value="white")+
  theme_void()  

ggplot(data= world_data3 %>% 
         mutate(clean_citing = ifelse(citing_nber == 0, NA, citing_nber)))+
  #geom_sf(size = 0, colour= "transparent", alpha=0.99)+
  geom_sf(aes(fill = clean_citing),size = 0.1, colour= "#0B252E",alpha=0.99)+
  #geom_point(aes(x = X, y = Y, size = clean_citing, fill= clean_citing), colour= "#0B252E", shape = 21)+
  #scale_fill_viridis_c(na.value="white")+
  #scale_fill_distiller(palette ="YlOrBr", na.value="white", direction = 1)+
  #scale_fill_distiller(palette ="Blues", na.value="#3f4a43", direction = 1)+
  #scale_fill_gradient2(low = "#ffe396", mid =  "#f0a102", high = "#c49000",na.value="white")+
  #scale_fill_gradientn(colours = rev(c("#BA4E26","#CC552D", "#F8E4DD")), na.value="white")+
  #scale_fill_gradientn(colours = c("#F6D6CB","#F3C8B9","#E79273", "#E17751", "#CF4E23","#AE421E",  "#451B0C"), breaks = seq(0,500,50), na.value="#ebebeb", name = "Number of citations in country")+
  scale_fill_gradientn(colours = c("#E6CCF5","#C488E7","#B367E0", "#A345D9", "#902ACB","#7823A9",  "#3C1155"), breaks = seq(0,500,50), na.value="#ebebeb", name = "Número de citas por país")+
  #scale_fill_gradientn(colours = cc, breaks = seq(0,500,50), na.value="#ebebeb", name = "Número de citas por país")+
  #scale_fill_viridis_c(begin = 0.3, option = "D", na.value="#ebebeb", name = "Número de citas por país")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                                barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
    scale_size_continuous(range = c(3,10))+
  theme_void() +
  theme(legend.position="top",legend.text=element_text(color="white",size=12),legend.title = element_text(color="white",size=12),
        plot.background = element_rect(fill = "black",
                                       colour = "black",
                                       size = 0.5, linetype = "solid"),
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"))
ggsave("mate_cho_black_vio_esp.png",dpi=300, width = 29.7, height = 21, units = "cm")

cc = c(viridis::viridis(7,begin= 0.3, end = 0.8)[-7], "#f1a40f")

