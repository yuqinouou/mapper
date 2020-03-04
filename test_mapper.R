# config
output_path <- "U:/Programming/Rproject/mapper/output"
shapefile_path <- "//SUN/Active/Internal_Groups/Safety_Team/Temp/Tiger_Shapefile/20190101/COUNTY"

program_root <- "U:/Programming/Rproject/mapper"

county_dt$region <- county_dt$fips
state_dt$region <- state_dt$fips

air_data_raw <- fread(file.path(program_root, "T100D_MARKET_US_CARRIER_ONLY.csv"))
airports_raw <- fread(file.path(program_root, "Airports.csv"))

limit <- 20000
air_data <- air_data_raw[PASSENGERS >= limit] %>% .[DEST != ORIGIN]
airports <- airports_raw[Loc_Id %in% unique(c(air_data$ORIGIN, air_data$DEST))] %>% 
  usmap::usmap_transform() %>% as.data.table() %>%
  setnames(c("X.1", "Y.1"), c("x", "y"))
air_data <- air_data %>% .[MONTH == 1] %>% 
  merge(airports[,.(Loc_Id, x, y)], by.x = "ORIGIN", by.y = "Loc_Id") %>%
  merge(airports[,.(Loc_Id, x, y)], by.x = "DEST", by.y = "Loc_Id", suffixes = c("_ORIGIN", "_DEST"))
  
require(ggplot2)
ggplot() +
  geom_map(data = usmap::countypop, map = county_dt, aes(map_id = fips, fill = pop_2015)) +
  geom_map(data = usmap::statepop, map = state_dt, aes(map_id = fips), fill = "transparent", colour = "grey30") +
  geom_curve(data = air_data, aes(x = x_ORIGIN, y = y_ORIGIN, xend = x_DEST, yend = y_DEST), curvature = 0.3, col = "#b29e7d", size = .4) +
  geom_point(data = airports, aes(x = x, y = y), color = "grey20") +
  expand_limits(x = county_dt$x, y = county_dt$y) +
  ggthemes::theme_map() +
  coord_cartesian(xlim = range(county_dt$x), ylim = range(county_dt$y)) +
  scale_fill_gradientn(
    limits = c(0, 1.1e7),
    breaks = c(0, 1e2, 1e3, 1e4, 1e5, 1e6, 1.1e7),
    colours = c('#edf8fb','#ccece6','#99d8c9','#66c2a4','#41ae76','#238b45','#005824'),
    trans = "log1p"
  )
