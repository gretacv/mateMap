# refs
#https://www.r-graph-gallery.com/a-smooth-transition-between-chloropleth-and-cartogram.html
# https://rdrr.io/cran/tmap/man/tm_polygons.html
# https://rpubs.com/ials2un/hunger1
#citing = read.table("/Users/gretacvega/Documents/GitHub/mate/citing.txt", h = T, sep ="\t")
citing = read.csv("/Users/gretacvega/Documents/GitHub/vizzWork/mate/citing.csv")
citing
#collab = read.table("/Users/gretacvega/Documents/GitHub/mate/collab.txt", h = T, sep ="\t")
collab = read.csv("/Users/gretacvega/Documents/GitHub/vizzWork/mate/collab.csv")
head(collab)


library("rnaturalearth")
library("rnaturalearthdata")

worldmap <- map_data("world")#, region = europeanUnion)
class(worldmap)

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



library(dplyr)    
library(tidyr)          # data manipulation
library(purrr)          # data manipulation
library(cartogram)      # cartograms creation
library(tmap)           # maps creation
library(sf) 

#create df with collab and citing
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


world_map = ne_countries(returnclass = "sf")
world_map = world_map[,"sovereignt"]
world_data = world_map %>% 
  #select(sovereignt) %>% 
  filter(sovereignt != "Antarctica") %>% 
  st_transform(crs = '+proj=robin') %>% 
  left_join(dd, by = "sovereignt")# %>% 
  #na.omit()
  #replace_na(list(collab_nber=0, percent_collab=0, citing_nber=0, percent_citing=0)) 

world_data2 = world_data %>% 
  select(sovereignt, percent_collab,percent_citing) %>% 
  gather(key = "type", value = "percent", "percent_collab":"percent_citing")

sel_countries = world_data %>% 
  select(sovereignt, percent_collab,percent_citing) %>% 
  filter(percent_collab>0|percent_citing>0) %>% 
  select(sovereignt)
sel_countries_collab = world_data %>% 
  select(sovereignt, percent_collab,percent_citing) %>% 
  filter(percent_collab>0) %>% 
  select(sovereignt)

world_data_collab_trans = world_data %>% 
  select(sovereignt, percent_collab,percent_citing) %>% 
  #filter(percent_collab>0|percent_citing>0) %>% 
  filter(percent_collab>0) %>% 
  mutate(percent_collab.1 = percent_collab+0.1*(percent_citing-percent_collab),
         percent_collab.2 = percent_collab+(0.2*(percent_citing-percent_collab)),
         percent_collab.3 = percent_collab+(0.3*(percent_citing-percent_collab)),
         percent_collab.4 = percent_collab+(0.4*(percent_citing-percent_collab)),
         percent_collab.5 = percent_collab+(0.5*(percent_citing-percent_collab)),
         percent_collab.6 = percent_collab+(0.6*(percent_citing-percent_collab)),
         percent_collab.7 = percent_collab+(0.7*(percent_citing-percent_collab)),
         percent_collab.8 = percent_collab+(0.8*(percent_citing-percent_collab)),
         percent_collab.9 = percent_collab+(0.9*(percent_citing-percent_collab)),
         percent_collab.10 = percent_collab+1*(percent_citing-percent_collab)
         ) %>% 
  #gather(key = "type", value = "percent", "percent_collab.1":"percent_collab.5")
  gather(key = "type", value = "percent", "percent_collab.1":"percent_collab.10")

  
world_data_citing_trans = world_data %>% 
  select(sovereignt, percent_collab,percent_citing) %>% 
  filter(percent_collab>0|percent_citing>0) %>% 
  #filter(percent_collab>0) %>%
  mutate(percent_citing.1 = percent_collab+0.1*(percent_citing-percent_collab),
         percent_citing.2 = percent_collab+0.2*(percent_citing-percent_collab),
         percent_citing.3 = percent_collab+0.3*(percent_citing-percent_collab),
         percent_citing.4 = percent_collab+0.4*(percent_citing-percent_collab),
         percent_citing.5 = percent_collab+0.5*(percent_citing-percent_collab),
         percent_citing.6 = percent_collab+0.6*(percent_citing-percent_collab),
         #percent_citing.7 = percent_collab+0.7*(percent_citing-percent_collab),
         percent_citing.8 = percent_collab+0.8*(percent_citing-percent_collab),
         percent_citing.9 = percent_collab+0.9*(percent_citing-percent_collab)
         # percent_citing.91 = percent_collab+0.91*(percent_citing-percent_collab),
         # percent_citing.92 = percent_collab+0.92*(percent_citing-percent_collab),
         # percent_citing.93 = percent_collab+0.93*(percent_citing-percent_collab),
         # percent_citing.94 = percent_collab+0.94*(percent_citing-percent_collab),
         # percent_citing.95 = percent_collab+0.95*(percent_citing-percent_collab),
         # percent_citing.96 = percent_collab+0.96*(percent_citing-percent_collab),
         # percent_citing.97 = percent_collab+0.97*(percent_citing-percent_collab),
         # percent_citing.98 = percent_collab+0.98*(percent_citing-percent_collab),
         # percent_citing.99 = percent_collab+0.99*(percent_citing-percent_collab)
         ) %>% 
  gather(key = "type", value = "percent", "percent_citing.1":"percent_citing.9")
  #gather(key = "type", value = "percent", "percent_citing.5":"percent_citing.9")


# world_data_citing = world_data2 %>%
#   filter(type == "percent_citing", percent>0)
# world_data_citing
# world_carto_citing = cartogram_cont(world_data_citing, "percent", itermax=5)
# 
# ggplot(world_carto_citing)+
#   geom_sf(aes(fill = percent), size = 0.1, 
#           colour= "#090D2A",#090D2A", 
#           alpha=0.99)+
#   scale_fill_viridis_c(na.value="white",end = 0.24)+
#   theme_void()
# 
# 
# 
# world_data_collab = world_data2 %>%
#   filter(type == "percent_collab", percent>0)
# world_data_collab
# 
# world_carto_collab = cartogram_cont(world_data_collab, "percent", itermax=5)
# ggplot(world_carto_collab)+
#   geom_sf(aes(fill = percent), size = 0.1, 
#           colour= "#090D2A",#090D2A", 
#           alpha=0.99)+
#   scale_fill_viridis_c(na.value="white")+
#   theme_void()


###building data for animation
world_data_real_all = world_data2 %>% 
  #filter(percent>0) %>%
  mutate(type = replace(type, type == "percent_citing", "percent_citing_all"), type = replace(type, type == "percent_collab", "percent_collab_all")) %>% 
  split(.$type) %>%
  #map(cartogram_cont, "percent", maxSizeError = 1.5) %>% 
  do.call(rbind, .) 

world_data_real_sel = world_data2 %>% 
  #filter(percent>0) %>%
  filter(sovereignt %in% sel_countries$sovereignt) %>%
  mutate(type = replace(type, type == "percent_citing", "percent_citing_sel"), 
         type = replace(type, type == "percent_collab", "percent_collab_sel"),
         percent = replace(percent, percent == 0, NA)) %>%
  split(.$type) %>%
  #map(cartogram_cont, "percent", maxSizeError = 1.5) %>% 
  do.call(rbind, .) 

world_data_rea_sel_collab = world_data2 %>% 
  filter(sovereignt %in% sel_countries_collab$sovereignt ) %>% 
  mutate(type = replace(type, type == "percent_citing", "percent_citing_collab_sel"), 
         percent = replace(percent, percent == 0, NA)) %>%
  filter(type == "percent_citing_collab_sel") %>% 
  split(.$type) %>%
  #map(cartogram_cont, "percent", maxSizeError = 1.5) %>% 
  do.call(rbind, .) 

world_data_carto = world_data2 %>% 
  filter(percent>0) %>% 
  #filter(sovereignt %in% sel_countries$sovereignt) %>%
  split(.$type) %>%
  map(cartogram_cont, "percent", maxSizeError = 1.5) %>% 
  do.call(rbind, .) 


world_data_collab_trans_carto = world_data_collab_trans %>% 
  #filter(percent>0) %>%
  split(.$type) %>%
  map(cartogram_cont, "percent", maxSizeError = 1.5) %>% 
  do.call(rbind, .)

world_data_citing_trans_carto = world_data_citing_trans %>% 
  #filter(percent>0) %>%
  split(.$type) %>%
  map(cartogram_cont, "percent", maxSizeError = 1.5) %>% 
  do.call(rbind, .)


# carto_anim = tm_shape(world_data_carto) +
#   tm_polygons("percent",palette = "viridis", lwd = 0.1, border.col= "#090D2A") +
#   tm_facets(along = "type", free.coords = FALSE, drop.units = TRUE)+
#   tm_layout(legend.outside.position = "right", legend.outside = TRUE)
# 
# tmap_animation(carto_anim, filename = "mate.gif", delay = 150,
#                width = 1326, height = 942)
# 
# carto_anim = tm_shape(world_data_real_all) +
#   tm_polygons("percent",palette = "viridis", lwd = 0.1, border.col= "#090D2A") +
#   tm_facets(along = "type", free.coords = FALSE, drop.units = TRUE)+
#   tm_layout(legend.outside.position = "right", legend.outside = TRUE)
# 
# tmap_animation(carto_anim, filename = "mate_real.gif", delay = 150,
#                width = 1326, height = 942)
# 
# carto_anim = tm_shape(world_data_real_sel) +
#   tm_polygons("percent",palette = "viridis", lwd = 0.1, border.col= "#090D2A") +
#   tm_facets(along = "type", free.coords = FALSE, drop.units = TRUE)+
#   tm_layout(legend.outside.position = "right", legend.outside = TRUE)
# 
# tmap_animation(carto_anim, filename = "mate_real_sel.gif", delay = 150,
#                width = 1326, height = 942)

world_data_rbind = rbind(world_data_real_all,
                         world_data_real_sel,
                         world_data_carto, 
                         world_data_rea_sel_collab,
                         world_data_collab_trans_carto %>%
                           select(-percent_collab, -percent_citing)#,
                         # world_data_citing_trans_carto %>% 
                         #   select(-percent_collab, -percent_citing)
                         ) %>% 
  filter(type != "percent_citing")

unique(world_data_rbind$type)
world_data_rbind$type = factor(world_data_rbind$type, 
                               levels = c("percent_collab_all",
                                          "percent_collab_sel",
                                          "percent_collab",
                                          "percent_collab.1",
                                          "percent_collab.2",
                                          "percent_collab.3",
                                          # "percent_citing.1",
                                          # "percent_citing.2",
                                          # "percent_citing.3",
                                          # "percent_citing.4",
                                          "percent_collab.4",
                                          "percent_collab.5",
                                          "percent_collab.6",
                                          "percent_collab.7",
                                          "percent_collab.8",
                                          "percent_collab.9",
                                          "percent_collab.10",
                                          #"percent_citing.5",
                                          #"percent_citing.6",
                                          #"percent_citing.7",
                                          #"percent_citing.8",
                                          #"percent_citing.9",
                                          # "percent_citing.92",
                                          # "percent_citing.93",
                                          # "percent_citing.94",
                                          # "percent_citing.95",
                                          # "percent_citing.96",
                                          # "percent_citing.97",
                                          # "percent_citing.98",
                                          # "percent_citing.99",
                                          # "percent_citing", 
                                          "percent_citing_collab_sel",
                                          "percent_citing_sel",
                                          "percent_citing_all"))
ggplot(data= world_data_rbind)+
  geom_sf(aes(fill = percent),size = 0.1, 
           colour= "#090D2A",#090D2A", 
           alpha=0.99)+
   scale_fill_viridis_c(na.value="white")+
   theme_void()  +
  facet_wrap(~type)


carto_anim = tm_shape(world_data_rbind) +
  tm_polygons("percent", palette = "viridis", lwd = 0.1, border.col= "#090D2A", colorNA = "white") +
  tm_facets(along = "type", free.coords = FALSE, drop.units = TRUE)+
  tm_layout(legend.outside.position = "right", legend.outside = TRUE)

tmap_animation(carto_anim, filename = "mate_seq_start.gif", delay = 150,
               width = 1326, height = 942)

# to do: use only collab countries for cartogram transition, then add the only citing countries
# todo : verify country numbers, verify if collab %in% citing. yes



## using gganimate https://www.r-graph-gallery.com/a-smooth-transition-between-chloropleth-and-cartogram.html
gg_dd = dd %>% mutate(sovereignt = replace(sovereignt, sovereignt == "United States of America", "United States"),
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


data(wrld_simpl)
plot(wrld_simpl)
w_dd = wrld_simpl[,"NAME"]
w_dd = merge(wrld_simpl, gg_dd, by.x = "NAME", by.y = "sovereignt", all = TRUE)
w_dd_sf = as(w_dd, 'sf')

w_dd_sf_robin = w_dd_sf %>% st_transform( crs = '+proj=robin') %>% na.omit() %>% select(-FIPS,         -ISO2,-ISO3, -UN, -AREA, -POP2005, -REGION, -SUBREGION)
w_dd_robin = as(w_dd_sf_robin, 'Spatial')
w_dd_robin_collab = w_dd_robin[w_dd_robin$percent_collab>0,]
plot(w_dd_robin[,"percent_collab"])
#dd$sovereignt[which(!dd$sovereignt%in%w_dd$NAME)]
collab_carto = cartogram_cont(w_dd_robin_collab, "percent_collab", itermax=15)
plot(collab_carto)

collab_cartogram_df <- tidy(collab_carto, region = "NAME") %>% left_join(. , collab_carto@data, by=c("NAME"="NAME")) 
collab_df <- tidy(w_dd_robin_collab) %>% left_join(. , w_dd_robin_collab@data, by=c("NAME"="NAME")) 

collab_cartogram_df$id <- seq(1,nrow(collab_cartogram_df))
collab_df$id <- seq(1,nrow(collab_df))

data <- rbind(collab_df, collab_cartogram_df, collab_df)

data$ease <- "cubic-in-out"
data$time <- rep(c(1:3), each=nrow(collab_df))

# Calculate the transition between these 2 objects?
dt <- tween_elements(data, time='time', group='id', ease='ease', nframes = 30)

# check a few frame
ggplot() + 
  geom_polygon(data = dt %>% filter(.frame==0) %>% arrange(order), 
               aes(fill = POP2005, x = long, y = lat, group = group), size=0, alpha=0.9
  )
ggplot() + 
  geom_polygon(data = dt %>% filter(.frame==5) %>% arrange(order), 
               aes(fill = POP2005, x = long, y = lat, group = group) , size=0, alpha=0.9
  )
ggplot() + 
  geom_polygon(data = dt %>% filter(.frame==10) %>% arrange(order), 
               aes(fill = POP2005, x = long, y = lat, group = group) , size=0, alpha=0.9
  )

st_obj = as(world_data_rbind, 'Spatial')
xx = world_data_rbind %>% 
  tidy %>% 
  filter(type %in% c("percent_collab", "percent_collab.10", "percent_citing_collab_sel")) %>% 
  group_by(type) %>% 
  mutate(id = row_number(type), ease = "cubic-in-out") %>% 
  ungroup() 
st_obj = as(xx, 'Spatial')
xx$ease <- "cubic-in-out"
xx$time <- rep(c(1:3), each=26)
library(tweenr)
dt <- tween_elements(xx, time='time', group='id', ease='ease', nframes = 30)


ggplot() + 
  geom_sf(data = dt %>% filter(.frame==0),# %>% arrange(order), 
               aes(fill = percent), size=0, alpha=0.9
  )
ggplot() + 
  geom_polygon(data = dt %>% filter(.frame==5),# %>% arrange(order), 
               aes(fill = percent, x = long, y = lat, group = group) , size=0, alpha=0.9
  )
ggplot() + 
  geom_polygon(data = dt %>% filter(.frame==10),# %>% arrange(order), 
               aes(fill = percent, x = long, y = lat, group = group) , size=0, alpha=0.9
  )


