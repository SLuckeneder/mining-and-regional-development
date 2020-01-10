
library(sf)
library(tidyverse)
library(viridis)
library(fineprintutils)
library(stringr)
library(zoo)
library(imputeTS)


# spatial polygon data frames ---------------------------------------------

# Source: https://gadm.org/ 
# issue: MANAUS is missing!

path <- "brasil/input/gadm"

countries <- c("BRA")
# filename <- paste0("gadm36_", countries, "_1_sf.rds")
filename <- paste0("gadm36_", countries, "_2_sf.rds")
# filename <- paste0("gadm36_", countries, "_3_sf.rds")

sf_BRA <- readRDS(file.path(path, filename))

# save spdf for panel extension
sf_panel <- sf_BRA

# plot(sf_BRA %>% dplyr::select(GID_1))


# ore extraction data -----------------------------------------------------

load("brasil/input/SEL_brasil.RData")
data <- data %>% dplyr::mutate(source = "snl")

# to do: find conversion factors; # Ask Mirko!
# idea: calculate average conversion factors from all mines where we know ore processed and metal content 
# (except for commodities with few cases such as cobalt)

# Bauxite: value = ore?
mines <- data %>% filter(commodity %in% "Bauxite") %>% select(mine) %>% distinct()
content <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "Bauxite") %>% mutate(unid = paste(snl_id, year, sep = "."))
ore <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "OreProcessed") %>% mutate(unid = paste(snl_id, year, sep = "."))
# only 2 mines report both. ore processed is way higher, but still bauxite is nothing else than aluminum ore, hence these nrs might be dead rock
duplicate_ore <- data %>% filter(commodity %in% "Bauxite") %>% mutate(commodity = "OreProcessed", source = "estimate")
data <- bind_rows(data, duplicate_ore)
rm(duplicate_ore, mines, content, ore)
#
# Coal: value = ore?
mines <- data %>% filter(commodity %in% "Coal") %>% select(mine) %>% distinct()
content <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "Coal") %>% mutate(unid = paste(snl_id, year, sep = "."))
ore <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "OreProcessed") %>% mutate(unid = paste(snl_id, year, sep = "."))
# only 2 mines report both. ore processed is way higher, but still bauxite is nothing else than aluminum ore, hence these nrs might be dead rock
duplicate_ore <- data %>% filter(commodity %in% "Coal") %>% mutate(commodity = "OreProcessed", source = "estimate")
data <- bind_rows(data, duplicate_ore)
rm(duplicate_ore, mines, content, ore)
#
# Cobalt: only 3 mines
# Tocantins: ore processed available
# Santa Rita: ore processed available 2010-2014, 2016 (2015 generally missing). avg ore grade calculated ftom them is 0.00005 -> only 2009 missing and it is a very small amount
# Fortaleza de Minas: ore processed available
# 
# Copper: only 8 mines
# Chapada: ore processed available
# Fortaleza de Minas: ore processed available
# MCSA Mining Complex: ore processed available
# Palito: ore processed available
# Rio Verde: ore processed available
# Salobo: ore processed available
# Santa Rita: ore processed available 2010-2014, 2016 (2015 generally missing) -> only 2009 missing and it is a very small amount
# Sossego: ore processed available only for 2004 and 2018. -> calculate avg ore grade and fill data
mines <- data %>% filter(commodity %in% "Copper") %>% select(mine) %>% distinct()
content <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "Copper") %>% mutate(unid = paste(snl_id, year, sep = "."))
ore <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "OreProcessed") %>% mutate(unid = paste(snl_id, year, sep = "."))
train <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(!is.na(ore_tonnes)) %>%
  mutate(conversion_factor = value / ore_tonnes)
cf <- mean(train$conversion_factor)
apply_cf <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(is.na(ore_tonnes)) %>%
  filter(primary_commodity == "Copper") %>%
  mutate(ore_tonnes = value / cf) %>%
  select(-unid, -value) %>%
  rename(value = ore_tonnes) %>%
  mutate(commodity = "OreProcessed", unit = "tonnes", source = "estimate")
data <- bind_rows(data, apply_cf)
rm(cf, apply_cf, mines, content, ore, train)
# 
# Diamonds: only 5 mines
# Chapada: ore processed available
# Duas Barras: difficult. only 2007 data includes diamonds and ore processed. best case find grades somewhere
# Melgueira: only 1 observation, very small amount, can be dropped
# Property 214: ore processed available
# Sao Luis River Basin: only 3 observations, drop...
# 
# Gold: large number of mines (35), apply average conversion factors on all primary commodity gold mines
mines <- data %>% filter(commodity %in% "Gold") %>% select(mine) %>% distinct()
content <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "Gold") %>% mutate(unid = paste(snl_id, year, sep = "."))
ore <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "OreProcessed") %>% mutate(unid = paste(snl_id, year, sep = "."))
train <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(!is.na(ore_tonnes)) %>%
  mutate(conversion_factor = value / ore_tonnes)
cf <- mean(train$conversion_factor)
apply_cf <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(is.na(ore_tonnes)) %>%
  filter(primary_commodity == "Gold") %>%
  mutate(ore_tonnes = value / cf) %>%
  select(-unid, -value) %>%
  rename(value = ore_tonnes) %>%
  mutate(commodity = "OreProcessed", unit = "tonnes", source = "estimate")
data <- bind_rows(data, apply_cf)
rm(cf, apply_cf, mines, content, ore, train)

# Ilmenite
# only 1 mine (Buena); ore processed available
# 
# Iron ore: value = ore?; lets check if there exists a mine reporting both
mines <- data %>% filter(commodity %in% "Iron Ore") %>% select(mine) %>% distinct()
content <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "Iron Ore") %>% mutate(unid = paste(snl_id, year, sep = "."))
ore <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "OreProcessed") %>% mutate(unid = paste(snl_id, year, sep = "."))
# only 6 mines report both. ore processed is higher, but not remarkably; hence I choose to take value as ore
duplicate_ore <- data %>% filter(commodity %in% "Iron Ore") %>% mutate(commodity = "OreProcessed", source = "estimate")
data <- bind_rows(data, duplicate_ore)
rm(duplicate_ore, mines, content, ore)
# 
# Lanthanides
# only 1 mine (Buena); ore processed available
# 
# Lead
# only 3 mines
# Morro Agudo: ore processed available (except 2004)
# Vazante: ore processed available 
# Vermelho: ore processed available 
# 
# Lithium: only 2 mines
# Cachoeira: ore processed only available for 2017 and 2018 -> apply conversion factor 1066 / 59871 = 0.01780495 for all other observations
mines <- data %>% filter(commodity %in% "Lithium") %>% select(mine) %>% distinct()
content <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "Lithium") %>% mutate(unid = paste(snl_id, year, sep = "."))
ore <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "OreProcessed") %>% mutate(unid = paste(snl_id, year, sep = "."))
train <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(!is.na(ore_tonnes)) %>%
  filter(primary_commodity == "Lithium") %>%
  mutate(conversion_factor = value / ore_tonnes)
cf <- mean(train$conversion_factor)
apply_cf <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(is.na(ore_tonnes)) %>%
  filter(primary_commodity == "Lithium") %>%
  mutate(ore_tonnes = value / cf) %>%
  select(-unid, -value) %>%
  rename(value = ore_tonnes) %>%
  mutate(commodity = "OreProcessed", unit = "tonnes", source = "estimate")
data <- bind_rows(data, apply_cf)
rm(cf, apply_cf, mines, content, ore, train)
# Mibra: ore processed available 
# 
# Manganese: only 4 mines, but quite some work to do here
# Azul: ore processed only available for 2009, 2010 and 2012, but manganese content is almost same amount -> apply content as ore processed
# Morro da Mina: ore processed only available for 2011-2013. here manganese content is much smaller -> apply mean conversion factor for all other observations
# Rio Madeira: no ore processed available. do some research or apply mean conversion factor
# Urucum: only estimate from iron ore available -> do some research or re-calculate summing up both iron ore and manganese
# 
# Nickel: 9 mines
# Americano do Brasil: ore processed available 
# Barro Alto: ore processed available 
# Codemin: ore processed available 
# Fortaleza de Minas: ore processed available except earlier years -> apply conversion rate
mines <- data %>% filter(commodity %in% "Nickel") %>% select(mine) %>% distinct()
content <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "Nickel") %>% mutate(unid = paste(snl_id, year, sep = "."))
ore <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "OreProcessed") %>% mutate(unid = paste(snl_id, year, sep = "."))
train <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(!is.na(ore_tonnes)) %>%
  filter(primary_commodity == "Nickel") %>%
  mutate(conversion_factor = value / ore_tonnes) %>%
  filter(mine == "Fortaleza de Minas")
cf <- mean(train$conversion_factor)
apply_cf <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(is.na(ore_tonnes)) %>%
  filter(mine == "Fortaleza de Minas") %>%
  filter(primary_commodity == "Nickel") %>%
  mutate(ore_tonnes = value / cf) %>%
  select(-unid, -value) %>%
  rename(value = ore_tonnes) %>%
  mutate(commodity = "OreProcessed", unit = "tonnes", source = "estimate")
data <- bind_rows(data, apply_cf)
rm(cf, apply_cf, mines, content, ore, train)
# Onca Puma: ore processed available 
# Santa Rita: ore processed available 
# Tocantins: 2004-2006 missing -> apply conversion rate
mines <- data %>% filter(commodity %in% "Nickel") %>% select(mine) %>% distinct()
content <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "Nickel") %>% mutate(unid = paste(snl_id, year, sep = "."))
ore <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "OreProcessed") %>% mutate(unid = paste(snl_id, year, sep = "."))
train <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(!is.na(ore_tonnes)) %>%
  filter(primary_commodity == "Nickel") %>%
  mutate(conversion_factor = value / ore_tonnes)
cf <- mean(train$conversion_factor)
apply_cf <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(is.na(ore_tonnes)) %>%
  filter(primary_commodity == "Nickel") %>%
  mutate(ore_tonnes = value / cf) %>%
  select(-unid, -value) %>%
  rename(value = ore_tonnes) %>%
  mutate(commodity = "OreProcessed", unit = "tonnes", source = "estimate")
data <- bind_rows(data, apply_cf)
rm(cf, apply_cf, mines, content, ore, train)
# 
# Niobium: only 2 mines
# Araxa: only 1 observation (2000) but no ore processed available
# Catalao de Goias: ore processed available 
# 
# Phosphate: 10 mines
# Angico dos Dias: ore processed available for 2010, only phosphate for 2015-2017 -> conversion factor or more research
# Araxa: only phospgate -> if possible apply conversion factor, more research else
# Bonfim: only 1 observation, no ore processed -> if possible apply conversion factor, more research else
# Cajati, Catalao, Chapadao, Itafos, Lagamar, Patos de Minas, Tapira
mines <- data %>% filter(commodity %in% "Phosphate") %>% select(mine) %>% distinct()
content <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "Phosphate") %>% mutate(unid = paste(snl_id, year, sep = "."))
ore <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "OreProcessed") %>% mutate(unid = paste(snl_id, year, sep = "."))
train <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(!is.na(ore_tonnes)) %>%
  mutate(conversion_factor = value / ore_tonnes)
cf <- mean(train$conversion_factor)
apply_cf <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(is.na(ore_tonnes)) %>%
  filter(primary_commodity == "Phosphate") %>%
  mutate(ore_tonnes = value / cf) %>%
  select(-unid, -value) %>%
  rename(value = ore_tonnes) %>%
  mutate(commodity = "OreProcessed", unit = "tonnes", source = "estimate")
data <- bind_rows(data, apply_cf)
rm(cf, apply_cf, mines, content, ore, train)
# 
# Potash: 1 mine
# Taquari-Vassouras, no ore processed available
# 
# Rutile: 1 mine
# Buena: 2001, 2010, 2011 missing -> more research
# 
# Silver: 10 mines, none is primarily a silver mine
# 
# Tantalum: 1 mine
# Mibra: no ore processed available
# 
# Tin: 4 mines -> apply mean conversion factor
# Bom Futuro: only content available
# Estanho de Rondonia SA: only content available
# Pitinga: ore processed available for more recent years
# Santa Barbara: only content available
mines <- data %>% filter(commodity %in% "Tin") %>% select(mine) %>% distinct()
content <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "Tin") %>% mutate(unid = paste(snl_id, year, sep = "."))
ore <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "OreProcessed") %>% mutate(unid = paste(snl_id, year, sep = "."))
train <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(!is.na(ore_tonnes)) %>%
  mutate(conversion_factor = value / ore_tonnes)
cf <- mean(train$conversion_factor)
apply_cf <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(is.na(ore_tonnes)) %>%
  filter(primary_commodity == "Tin") %>%
  mutate(ore_tonnes = value / cf) %>%
  select(-unid, -value) %>%
  rename(value = ore_tonnes) %>%
  mutate(commodity = "OreProcessed", unit = "tonnes", source = "estimate")
data <- bind_rows(data, apply_cf)
rm(cf, apply_cf, mines, content, ore, train)
# 
# U3O8 i.e. Uran
# 1 mine, ore processed only for latest years -> calculate and apply conversion factor
mines <- data %>% filter(commodity %in% "U3O8") %>% select(mine) %>% distinct()
content <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "U3O8") %>% mutate(unid = paste(snl_id, year, sep = "."))
ore <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "OreProcessed") %>% mutate(unid = paste(snl_id, year, sep = "."))
train <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(!is.na(ore_tonnes)) %>%
  mutate(conversion_factor = value / ore_tonnes)
cf <- mean(train$conversion_factor)
apply_cf <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(is.na(ore_tonnes)) %>%
  filter(primary_commodity == "U3O8") %>%
  mutate(ore_tonnes = value / cf) %>%
  select(-unid, -value) %>%
  rename(value = ore_tonnes) %>%
  mutate(commodity = "OreProcessed", unit = "tonnes", source = "estimate")
data <- bind_rows(data, apply_cf)
rm(cf, apply_cf, mines, content, ore, train)
# 
# Vanadium: 1mine
# Vanadio de Maracas Menchen: no ore processed available -> research -> avg grade of 1.14% assumed https://miningdataonline.com/property/551/Maracas-Menchen-Mine.aspx
mines <- data %>% filter(commodity %in% "Vanadium") %>% select(mine) %>% distinct()
content <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "Vanadium") %>% mutate(unid = paste(snl_id, year, sep = "."))
ore <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "OreProcessed") %>% mutate(unid = paste(snl_id, year, sep = "."))
# only 6 mines report both. ore processed is higher, but not remarkably; hence I choose to take value as ore
duplicate_ore <- data %>% filter(commodity %in% "Vanadium") %>% mutate(commodity = "OreProcessed", source = "estimate") %>%
  mutate(value = value / 0.0114)
data <- bind_rows(data, duplicate_ore)
rm(duplicate_ore, mines, content, ore)
# 
# Zinc: 5 mines -> apply mean conversion factor
# Aripuana: only content available
# Monte Cristo: only content available
# Morro Agudo: ore processed available and zinc mine
# Tres Marias: ore processed only available for 2008 and 2009. Previous in content
# Vazante: ore processed available and zinc mine
mines <- data %>% filter(commodity %in% "Zinc") %>% select(mine) %>% distinct()
content <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "Zinc") %>% mutate(unid = paste(snl_id, year, sep = "."))
ore <- data %>% filter(mine %in% mines$mine) %>% filter(commodity %in% "OreProcessed") %>% mutate(unid = paste(snl_id, year, sep = "."))
train <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(!is.na(ore_tonnes)) %>%
  mutate(conversion_factor = value / ore_tonnes)
cf <- mean(train$conversion_factor)
apply_cf <- left_join(content, ore %>% select(unid, value) %>% rename(ore_tonnes = value), by = "unid") %>%
  filter(is.na(ore_tonnes)) %>%
  filter(primary_commodity == "Zinc") %>%
  mutate(ore_tonnes = value / cf) %>%
  select(-unid, -value) %>%
  rename(value = ore_tonnes) %>%
  mutate(commodity = "OreProcessed", unit = "tonnes", source = "estimate")
data <- bind_rows(data, apply_cf)
rm(cf, apply_cf, mines, content, ore, train)
# 
# Zircon: 1 mine
# Buena: ore processed available


snl_data <- data %>%
  dplyr::mutate(value = value / 1000) %>%
  dplyr::filter(commodity == "OreProcessed")

# remove observations without coords
snl_data <- snl_data %>%
  dplyr::filter(!is.na(X))

# to sf and join with polygons
snl_data_sf <- snl_data %>% 
  sf::st_as_sf(coords = c("X","Y"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  sf::st_join(sf_BRA, join = st_intersects) 

# aggregate point data into TL3 polygons
snl_data_sf_agg <- snl_data_sf %>%
  dplyr::group_by(GID_2, year) %>%
  dplyr::summarise(ore_extraction = sum(value, na.rm = TRUE)) %>%
  as.data.frame() %>%
  dplyr::select(-geometry)

# save snl_data_sf_agg for panel extension
snl_data_sf_agg_panel <- snl_data_sf_agg

# select year (cross-section)
snl_data_sf_agg_2010 <- snl_data_sf_agg %>% dplyr::filter(year == 2010)
snl_data_sf_agg_2016 <- snl_data_sf_agg %>% dplyr::filter(year == 2016)
snl_data_sf_agg_2017 <- snl_data_sf_agg %>% dplyr::filter(year == 2017)

# merge aggregates of selected year into sf (cross-section)
sf_BRA_2010 <- sf_BRA %>% dplyr::left_join(snl_data_sf_agg_2010, by = "GID_2")
sf_BRA_2016 <- sf_BRA %>% dplyr::left_join(snl_data_sf_agg_2016, by = "GID_2")
sf_BRA_2017 <- sf_BRA %>% dplyr::left_join(snl_data_sf_agg_2017, by = "GID_2")

# # subset to overcome memory problems when plotting
# sf_ore_2010 <- sf_BRA_2010
# sf_ore_2017 <- sf_BRA_2017


# ore extraction maps -----------------------------------------------------

# zoom table
zoom_bbox <- tibble::tribble(
  ~region,                    ~x_lim,            ~y_lim,
  "Chile",          c(-76.00, -67.00), c(-57.00, -14.00),
  "Latin America",  c( -88.00, -31.00), c(-58.00, 13.00),
  "Peru",           c(-82.00, -68.00), c(-19.00, 00.50),
  "Mexico",         c(-119.00, -84.00), c(13.00, 33.00),
  "Minas Gerais",   c(-52.00, -39.00), c(-23.00, -13.00),
  "Brasil",         c(-74.00, -34.00), c(-34.00, 6.00)
) %>% 
  dplyr::mutate(geometry = lapply(seq_along(region), function(i) sf::st_multipoint(matrix(c(x_lim[[i]], y_lim[[i]]), nrow = 2))),
                group = 1,
                geometry = lapply(geometry, sf::st_bbox),
                geometry = lapply(geometry, sf::st_as_sfc),
                geometry = lapply(geometry, sf::st_geometrycollection),
                geometry = sf::st_sfc(geometry)) %>% 
  sf::st_sf() %>% 
  sf::st_collection_extract()

lim <- zoom_bbox %>% 
  dplyr::filter(region == "Brasil")

sf_BRA_2010 %>% ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = log(ore_extraction))) +
  ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
  viridis::scale_fill_viridis(na.value = NA, option = "viridis") +
  ggplot2::geom_point(data = data %>% dplyr::filter(year == "2010"), aes(x = X, y = Y), size = 2, 
             shape = 15, color = "red") +
  ggplot2::labs(title = "Mining activities, Brasil, 2010", fill="Ore processed (kilotonnes, log)") +
  fineprintutils::theme_map2() +
  ggplot2::theme(legend.position = "bottom",
                 legend.justification = "center") +
  guides(fill = guide_colorbar(
    barheight = unit(2, units = "mm"),
    barwidth = unit(140, units = "mm"),
    title.position = "top",
    title.hjust = 0.5,
    label.hjust = 1))
ggplot2::ggsave("brasil/output/maps/brasil_ore_2010_mod.png", plot = last_plot(), device = "png",
                scale = 1, width = 270, height = 300, units = "mm")

pfull <- sf_BRA_2016 %>% ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = log(ore_extraction))) +
  ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
  viridis::scale_fill_viridis(na.value = NA, option = "viridis") +
  ggplot2::geom_point(data = data %>% dplyr::filter(year == "2016"), aes(x = X, y = Y), size = 2, 
                      shape = 15, color = "red") +
  ggplot2::labs(title = NULL, fill="Ore processed (kilotonnes, log)") +
  fineprintutils::theme_map2() +
  ggplot2::theme(legend.position = "bottom",
                 legend.justification = "center",
                 legend.box.margin=margin(-10,-10, 0,-10, unit = "mm"),
                 legend.title=element_text(size=15),
                 legend.text=element_text(size=15),
                 plot.margin=unit(c(-10, -10, 0, -10),"mm")) +
  guides(fill = guide_colorbar(
    barheight = unit(2, units = "mm"),
    barwidth = unit(140, units = "mm"),
    title.position = "top",
    title.hjust = 0.5,
    label.hjust = 1))
ggplot2::ggsave("brasil/output/maps/brasil_ore_2016_mod.png", plot = pfull, device = "png",
                scale = 1, width = 270, height = 300, units = "mm")

sf_BRA_2017 %>% ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = log(ore_extraction))) +
  ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
  viridis::scale_fill_viridis(na.value = NA, option = "viridis") +
  ggplot2::geom_point(data = data %>% dplyr::filter(year == "2017"), aes(x = X, y = Y), size = 2, 
                      shape = 15, color = "red") +
  ggplot2::labs(title = "Mining activities, Brasil, 2017", fill="Ore processed (kilotonnes, log)") +
  fineprintutils::theme_map2() +
  ggplot2::theme(legend.position = "bottom",
                 legend.justification = "center") +
  guides(fill = guide_colorbar(
    barheight = unit(2, units = "mm"),
    barwidth = unit(140, units = "mm"),
    title.position = "top",
    title.hjust = 0.5,
    label.hjust = 1))
ggplot2::ggsave("brasil/output/maps/brasil_ore_2017_mod.png", plot = last_plot(), device = "png",
                scale = 1, width = 270, height = 300, units = "mm")


# zoom into minas gerais
lim <- zoom_bbox %>% 
  dplyr::filter(region == "Minas Gerais")

sf_BRA_2017 %>% ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = log(ore_extraction))) +
  ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
  viridis::scale_fill_viridis(na.value = NA, option = "viridis") +
  ggplot2::geom_point(data = data %>% dplyr::filter(year == "2017"), aes(x = X, y = Y), size = 2, 
                      shape = 15, color = "red") +
  ggplot2::labs(title = "Mining activities, Minas Gerais, Brasil, 2017", fill="Ore processed (log)") +
  fineprintutils::theme_map2() +
  ggplot2::theme(legend.position = "bottom",
                 legend.justification = "center") +
  guides(fill = guide_colorbar(
    barheight = unit(2, units = "mm"),
    barwidth = unit(140, units = "mm"),
    title.position = "top",
    title.hjust = 0.5,
    label.hjust = 1))
ggplot2::ggsave("brasil/output/maps/minas_gerais_ore_2017_mod.png", plot = last_plot(), device = "png",
                scale = 1, width = 400, height = 300, units = "mm")

pzoom <- sf_BRA_2016 %>% ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = log(ore_extraction))) +
  ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
  viridis::scale_fill_viridis(na.value = NA, option = "viridis") +
  ggplot2::geom_point(data = data %>% dplyr::filter(year == "2016"), aes(x = X, y = Y), size = 2, 
                      shape = 15, color = "red") +
  ggplot2::labs(title = NULL, fill="Ore processed (log)") +
  fineprintutils::theme_map2() +
  ggplot2::theme(legend.position = "none",
                 legend.justification = "center",
                 panel.background = element_rect(color='black', fill="white")) +
  guides(fill = guide_colorbar(
    barheight = unit(2, units = "mm"),
    barwidth = unit(140, units = "mm"),
    title.position = "top",
    title.hjust = 0.5,
    label.hjust = 1))
ggplot2::ggsave("brasil/output/maps/minas_gerais_ore_2016_mod.png", plot = pzoom, device = "png",
                scale = 1, width = 400, height = 300, units = "mm")


# put Brasil 2016 and minas gerais zoom together
lim <- zoom_bbox %>% 
  dplyr::filter(region == "Brasil")

g <- ggplotGrob(pzoom)
p <- pfull +
  ggplot2::theme(plot.margin=unit(c(-10, 100, 0, -10),"mm")) +
  annotation_custom(grob = g, 
                    xmin = max(lim$x_lim[[1]]) - 12, xmax = max(lim$x_lim[[1]]) + 23, 
                    ymin = min(lim$y_lim[[1]]), ymax = min(lim$y_lim[[1]]) + 20) 
ggplot2::ggsave("brasil/output/maps/brasil_ore_incl_zoom_2016_mod.png", plot = p, device = "png",
                scale = 1, width = 400, height = 300, units = "mm")



### extend to shp to panel and merge extraction data

# stack spdf data t (= 19 years) times
sf_panel <- do.call("rbind", replicate(19, sf_panel, simplify = FALSE))
sf_panel <- sf_panel %>% dplyr::mutate(year = rep(as.character(2000:2018), each=5504))

# merge aggregates of selected year into sf
sf_panel <- sf_panel %>% dplyr::left_join(snl_data_sf_agg_panel, by = c("GID_2", "year"))




# ports -------------------------------------------------------------------

# Source: https://www.searoutes.com/country-ports/Brazil

# TO BE IMPLEMENTED

ports <- read.csv("brasil/input/ports.csv", sep = ";") # MANAUS MISSING!!




# IBGE demographics -------------------------------------------------------

ibge_pop <- read.csv("brasil/input/ibge/pop_mod.csv", sep = ";", skip = 1, stringsAsFactors = FALSE, 
                     check.names = F, fileEncoding="latin1") 
ibge_pop <- ibge_pop %>%
  `colnames<-`(c("level", "id", "municipio", "year", "variable", "value", "empty")) %>%
  dplyr::select(c(1:6)) %>%
  dplyr::filter(level == "MU") %>%
  dplyr::select(-level) %>%
  dplyr::mutate(state = stringr::str_sub(municipio,-4)) %>%
  dplyr::mutate(state = substr(state, 2, 3)) 
ibge_pop <- ibge_pop %>%
  dplyr::mutate(municipio = substr(municipio,1,nchar(ibge_pop$municipio)-4)) %>%
  dplyr::mutate(value = as.numeric(value))

unique(ibge_pop$year)
# there is no pop data for 2007 and 2010 -> take mean values of previous and following year

df <- data.frame("id" = rep(unique(ibge_pop$id), each = 2),
                 "year" = as.character(rep(c(2007, 2010)), each = 2)) 

ibge_pop <- dplyr::bind_rows(ibge_pop, df) %>%
  dplyr::arrange(id, year)

x <- ibge_pop %>% dplyr::group_by(id) %>%
  dplyr::mutate(value = imputeTS::na_ma(value, k = 1)) %>%
  dplyr::ungroup() %>%
  dplyr::select(value)

# check
y <- data.frame(ibge_pop$value, x)

# apply to ibge_pop df
ibge_pop <- ibge_pop %>% dplyr::group_by(id) %>%
  dplyr::mutate(value = imputeTS::na_ma(value, k = 1)) %>%
  dplyr::ungroup() 

# fill time invariant NAs
ibge_pop <- ibge_pop %>% 
  dplyr::mutate(municipio = zoo::na.locf(municipio),
                variable = zoo::na.locf(variable),
                state = zoo::na.locf(state))


# IBGE economics ----------------------------------------------------------

ibge_gdp <- read.csv("brasil/input/ibge/gdp_mod.csv", sep = ";", skip = 1, stringsAsFactors = FALSE, 
                     check.names = F, fileEncoding="latin1") 
ibge_gdp <- ibge_gdp %>%
  `colnames<-`(c("level", "id", "municipio", "year", "variable", "value")) %>%
  dplyr::filter(level == "MU") %>%
  dplyr::select(-level) %>%
  dplyr::mutate(state = stringr::str_sub(municipio,-4)) %>%
  dplyr::mutate(state = substr(state, 2, 3)) 
ibge_gdp <- ibge_gdp %>%
  dplyr::mutate(municipio = substr(municipio,1,nchar(ibge_gdp$municipio)-4)) %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  dplyr::mutate(year = as.character(year))


# merge data --------------------------------------------------------------

ibge_data <- dplyr::bind_rows(ibge_pop, ibge_gdp)
suppressWarnings(rm(ibge_pop, ibge_gdp))


# load or create region concordance ---------------------------------------

if("conc_mun_mod.csv" %in% dir("brasil/input")){
  conc <- read.csv("brasil/input/conc_mun_mod.csv", sep = ";", stringsAsFactors = FALSE)
} else{
  conc_ibge <- read.csv("brasil/input/conc_mun.csv", sep = ";", stringsAsFactors = FALSE)
  conc_gadm <- sf_BRA %>% dplyr::select(GID_2, NAME_2) %>% sf::st_set_geometry(NULL)
  conc <- dplyr::left_join(conc_gadm, conc_ibge, by = c("NAME_2" = "NO_MUN_MIN"))
  write.table(conc, file = paste0("brasil/input/conc_mun_mod.csv"), row.names=FALSE, sep = ";")
  # now modify manually!
}

# check concordance
dup <- conc[duplicated(conc$GID_2),]
dup <- conc[is.na(conc$GID_2),]
dup <- conc[duplicated(conc$CO_MUN_GEO),]
dup <- conc[is.na(conc$CO_MUN_GEO),]
rm(dup)

# merge to spatial data ---------------------------------------------------

# apply concordance
conc <- read.csv("brasil/input/conc_mun_mod_new.csv", sep = ";", stringsAsFactors = FALSE)
conc <- conc %>% 
  dplyr::mutate(CO_MUN_GEO = as.character(CO_MUN_GEO))
ibge_data <- ibge_data %>% dplyr::full_join(conc, by = c("id" = "CO_MUN_GEO"))

# check for NAs
check <- ibge_data %>% 
  dplyr::filter(year == 2003) %>%
  dplyr::filter(variable == "Produto Interno Bruto a pre\u0087os correntes (Mil Reais)") %>%
  dplyr::filter(is.na(GID_2))
# ca 100, okay
rm(check)

# remove columns that are already in the ore data
ibge_data <- ibge_data %>% 
  dplyr::select(-NAME_2)

# merge into spdf_panel (takes time!!)
if("panel.RData" %in% dir("input")){
  load("brasil/input/panel.RData")
} else{
  sf_panel <- sf_panel %>%
    dplyr::left_join(ibge_data , by = c("GID_2" = "GID_2", "year" = "year")) %>%
    dplyr::select(GID_2, year, ore_extraction, variable, value)
  
  sf_panel <- tidyr::spread(sf_panel, variable, value, drop = TRUE)
  save(sf_panel, file = "brasil/input/panel.RData")
}
sf_panel <- sf_panel %>% dplyr::select(-`<NA>`)
colnames(sf_panel) <- c("GID_2", "year", "ore_extraction", "tax", "pop", 
                        "gdp_current_thousand_reais", "gva_public_service_current_thousand_reais",
                        "gva_agriculture_current_thousand_reais", "gva_industry_current_thousand_reais",
                        "gva_private_service_current_thousand_reais", "gva_current_thousand_reais",
                        "geometry")

# map descriptives --------------------------------------------------------

lim <- zoom_bbox %>% 
  dplyr::filter(region == "Brasil")

# population
sf_panel %>%
  dplyr::filter(year == 2016) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = log(pop)), colour="white", lwd = 0.1) +
  ggplot2::coord_sf(datum = NA) +
  viridis::scale_fill_viridis(na.value = NA, option = "viridis") +
  # ggplot2::geom_point(data = data %>% dplyr::filter(year == "2017"), aes(x = X, y = Y), size = 1, 
  #                     shape = 4, color = "red") +
  ggplot2::labs(title = "Population, Brasil, 2017", fill="Inhabitants (log)") +
  fineprintutils::theme_map2() +
  ggplot2::theme(legend.position = "bottom",
                 legend.justification = "center",
                 legend.box.margin=margin(-10,-10, 0,-10, unit = "mm"),
                 legend.title=element_text(size=15),
                 legend.text=element_text(size=15),
                 plot.margin=unit(c(-10, -10, 0, -10),"mm")) +
  guides(fill = guide_colorbar(
    barheight = unit(2, units = "mm"),
    barwidth = unit(140, units = "mm"),
    title.position = "top",
    title.hjust = 0.5,
    label.hjust = 1))
ggplot2::ggsave("brasil/output/maps/brasil_pop_2016.png", plot = last_plot(), device = "png",
                scale = 1, width = 270, height = 300, units = "mm")

# gdp
sf_panel %>%
  dplyr::filter(year == 2016) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = log(gdp_current_thousand_reais)), colour="white", lwd = 0.1) +
  ggplot2::coord_sf(datum = NA) +
  viridis::scale_fill_viridis(na.value = NA, option = "viridis") +
  # ggplot2::geom_point(data = data %>% dplyr::filter(year == "2017"), aes(x = X, y = Y), size = 1, 
  #                     shape = 4, color = "red") +
  ggplot2::labs(title = "Regional GDP, Brasil, 2017", fill="GDP in current Real (thousand, log)") +
  fineprintutils::theme_map2() +
  ggplot2::theme(legend.position = "bottom",
                 legend.justification = "center",
                 legend.box.margin=margin(-10,-10, 0,-10, unit = "mm"),
                 legend.title=element_text(size=15),
                 legend.text=element_text(size=15),
                 plot.margin=unit(c(-10, -10, 0, -10),"mm")) +
  guides(fill = guide_colorbar(
    barheight = unit(2, units = "mm"),
    barwidth = unit(140, units = "mm"),
    title.position = "top",
    title.hjust = 0.5,
    label.hjust = 1))
ggplot2::ggsave("brasil/output/maps/brasil_gdp_2016.png", plot = last_plot(), device = "png",
                scale = 1, width = 270, height = 300, units = "mm")

# gdp per capita
sf_panel %>%
  dplyr::filter(year == 2016) %>% 
  dplyr::mutate(gdp_cap = log(gdp_current_thousand_reais / pop)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = gdp_cap), colour="white", lwd = 0.1) +
  ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
  viridis::scale_fill_viridis(na.value = NA, option = "viridis") +
  # ggplot2::geom_point(data = data %>% dplyr::filter(year == "2017"), aes(x = X, y = Y), size = 1, 
  #                     shape = 4, color = "red") +
  ggplot2::labs(title = NULL, fill="GDP in thousand current Real per capita (log)") +
  fineprintutils::theme_map2() +
  ggplot2::theme(legend.position = "bottom",
                 legend.justification = "center",
                 legend.box.margin=margin(-10,-10, 0,-10, unit = "mm"),
                 legend.title=element_text(size=15),
                 legend.text=element_text(size=15),
                 plot.margin=unit(c(-10, -10, 0, -10),"mm")) +
  guides(fill = guide_colorbar(
    barheight = unit(2, units = "mm"),
    barwidth = unit(140, units = "mm"),
    title.position = "top",
    title.hjust = 0.5,
    label.hjust = 1))
ggplot2::ggsave("brasil/output/maps/brasil_gdp_cap_2016.png", plot = last_plot(), device = "png",
                scale = 1, width = 270, height = 300, units = "mm")

  # merge ports into data ---------------------------------------------------

# panel
sf_panel <- sf_panel %>% dplyr::left_join(ports %>% dplyr::select(GID_2, large_port), by = "GID_2")



# some corrections --------------------------------------------------------

# check coverage
min(sf_panel$year)
max(sf_panel$year)
sub <- na.omit(sf_panel)
min(sub$year)
max(sub$year)

# there is no pop data for 2007 and 2010 -> take mean values of previous and following year

# there is no XXXX data for 2001, 2007 and 2010, 2017 -> take mean values of previous and following year


# TO BE IMPLEMENTED


# save full spatial data --------------------------------------------------

save(sf_panel, file = "brasil/input/full_panel.RData")

# for later: exlude all regions for which there is no IBGE data
conc_ibge <- read.csv("brasil/input/conc_mun.csv", sep = ";", stringsAsFactors = FALSE)
conc_gadm <- sf_BRA %>% dplyr::select(GID_2, NAME_2) %>% sf::st_set_geometry(NULL)
conc <- read.csv("brasil/input/conc_mun_mod_new.csv", sep = ";", stringsAsFactors = FALSE)

# c(setdiff(conc$GID_2, conc_gadm$GID_2), setdiff(conc_gadm$GID_2, conc$GID_2))

missing_polygons <- setdiff(conc_gadm$GID_2, conc$GID_2) #can be investigated a bit more at a later stage
save(missing_polygons, file = "brasil/input/missing_polygons.RData")

