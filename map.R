library("ggplot2")
library("sf")
library("ggrepel")
library("ggspatial")
library("ggrepel")
library("dplyr")
library("ggforce")

# reading metadata and making a data frame

sites <- data.frame(read.csv("~/metadata_for_map.csv", sep = ";", dec = "."))

# setting min and max coordinates to focus the map

minx = 129.5
maxx = 131
miny = 32.6
maxy = 34

bbox <- st_bbox(c(xmin = minx, xmax = maxx, ymin = miny, ymax = maxy)) # focusing on Kyushu Island
 
# loading rivers data
rivers_json_polygons <- read_sf("~/hotosm_jpn_waterways_polygons.geojson")
rivers_json_polygons <- st_crop(rivers_json_polygons, bbox)
rivers_json_polygons_filtered <- rivers_json_polygons %>%
  filter(water == "river" | water == "canal")


# loading map data
japan_sf <- st_read("C:/Users/bogda/Documents/PhD/Fish/map/jpn_adm_2019_GDB.gdb") # read prefectures map
st_crs(japan_sf) <- 4326 # assign coordinates

# colors

pal1 <- c("#3e9896", "#51a16a", "#233341", "#efb440", "#987143", "#b60e10", "#d6828c", "#8b004b") 
borders <- "grey71"

rivers <- "#c9d7dd"

# full map plot
kuyshu_2 <- ggplot(japan_sf %>% filter(AdmLevel == 1 | AdmLevel == 99)) +
  geom_sf(data = rivers_json_polygons_filtered["geometry"], color = rivers, fill = rivers) +
  geom_sf(na.rm = TRUE, color = borders) +
  theme_bw() +
  geom_point(data = sites, aes(x = Longitude, y = Latitude, color = River_system, shape = Origin, size = n)) +
  coord_sf(xlim = c(minx, maxx), ylim = c(miny, maxy), expand = FALSE) +
  theme(
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    axis.title = element_text(size = 20), 
    axis.text.x = element_text(colour = "black", size=18), 
    axis.text.y = element_text(colour = "black", size=18),
    legend.position = "right",
    panel.grid = element_line(color = borders),
    panel.background = element_rect(fill = "#fffcea"),
    panel.grid.major.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_text_repel(data = sites,
                  aes(x = Longitude, y = Latitude, label = Location_ID, color = River_system),
                  size = 8, box.padding = 0.5, max.overlaps = 40, show.legend = FALSE) +
  scale_shape_manual(name = "Origin", values = c(17, 16)) +
  scale_color_manual(name = "River system", values = pal1) +
  scale_size(name = "N of samples") +
  guides(color = guide_legend(override.aes = list(size=6))) +
  guides(shape = guide_legend(override.aes = list(size=6))) +
  annotate(geom = "text", x = 130.6, y = 33.55, label = "FUKUOKA\n福岡市", color = borders, size = 5) +
  annotate(geom = "text", x = 130.1, y = 33.35, label = "SAGA\n佐賀県", color = borders, size = 5) +
  annotate(geom = "text", x = 130.8, y = 32.88, label = "KUMAMOTO\n熊本県", color = borders, size = 5) +
  annotate(geom = "text", x = 130.05, y = 32.92, label = "NAGASAKI\n長崎県", color = borders, size = 5) +
  annotate(geom = "text", x = 130.94, y = 33.35, label = "OITA\n大分県", color = borders, size = 5)
kuyshu_2

# close up

rivers_json_polygons_cropped <- rivers_json_polygons %>%
  filter(water == "river" | water == "canal" | water == "drain")


kuyshu_close <- ggplot(japan_sf %>% filter(AdmLevel == 1 | AdmLevel == 99)) +
  geom_sf(data = rivers_json_polygons_cropped["geometry"], color = "#c9d7dd", fill = "#c9d7dd") +
  geom_sf(na.rm = TRUE, color = borders) +
  theme_bw() +
  geom_point(data = sites %>% filter(Location_ID %in% c("N7", "N8", "N9", "N10", "N11", "N12", "N13")),
                                     aes(x = Longitude, y = Latitude, color = River_system, size = n)) +
  coord_sf(xlim = c(130.24, 130.31), ylim = c(33.15, 33.21), expand = FALSE) +
  theme(
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    axis.title = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.grid = element_line(color = borders),
    panel.background = element_rect(fill = "#fffcea"),
    panel.grid.major.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_text_repel(data = sites %>% filter(Location_ID %in% c("N7", "N8", "N9", "N10", "N11", "N12", "N13")),
                  aes(x = Longitude, y = Latitude, label = Location_ID, color = River_system),
                  size = 8, box.padding = 0.5, max.overlaps = 40, show.legend = FALSE) +
  scale_shape_manual(name = "Origin", values = 17) +
  scale_color_manual(name = "River system", values = "#51a16a") +
  guides(color = guide_legend(override.aes = list(size=6))) +
  guides(shape = guide_legend(override.aes = list(size=6))) +
  annotate(geom = "text", x = 130.29, y = 33.20, label = "SAGA\n佐賀県", color = "grey71", size = 8)
kuyshu_close

# save with ggsave