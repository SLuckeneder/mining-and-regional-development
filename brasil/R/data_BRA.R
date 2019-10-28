
library(sf)
library(tidyverse)
library(viridis)
library(fineprintutils)
library(stringr)

# spatial polygon data frames ---------------------------------------------

# Source: https://gadm.org/ 

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

snl_data <- data %>%
  dplyr::mutate(value = value / 1000)

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
snl_data_sf_agg_2017 <- snl_data_sf_agg %>% dplyr::filter(year == 2017)

# merge aggregates of selected year into sf (cross-section)
sf_BRA_2010 <- sf_BRA %>% dplyr::left_join(snl_data_sf_agg_2010, by = "GID_2")
sf_BRA_2017 <- sf_BRA %>% dplyr::left_join(snl_data_sf_agg_2017, by = "GID_2")

# # subset to overcome memory problems when plotting
# sf_ore_2010 <- sf_BRA_2010
# sf_ore_2017 <- sf_BRA_2017

sf_BRA_2010 %>% ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = log(ore_extraction))) +
  ggplot2::coord_sf(datum = NA) +
  viridis::scale_fill_viridis(na.value = NA, option = "viridis") +
  ggplot2::geom_point(data = data %>% dplyr::filter(year == "2010"), aes(x = X, y = Y), size = 1, 
             shape = 4, color = "red") +
  ggplot2::labs(title = "Mining activities, Brasil, 2010", fill="Ore processed (log)") +
  fineprintutils::theme_map2() +
  ggplot2::theme(legend.position = "bottom",
                 legend.justification = "center") +
  guides(fill = guide_colorbar(
    barheight = unit(2, units = "mm"),
    barwidth = unit(140, units = "mm"),
    title.position = "top",
    title.hjust = 0.5,
    label.hjust = 1))
ggplot2::ggsave("brasil/output/maps/brasil_ore_2010.png", plot = last_plot(), device = "png",
                scale = 1, width = 400, height = 300, units = "mm")

sf_BRA_2017 %>% ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = log(ore_extraction))) +
  ggplot2::coord_sf(datum = NA) +
  viridis::scale_fill_viridis(na.value = NA, option = "viridis") +
  ggplot2::geom_point(data = data %>% dplyr::filter(year == "2017"), aes(x = X, y = Y), size = 1, 
                      shape = 4, color = "red") +
  ggplot2::labs(title = "Mining activities, Brasil, 2017", fill="Ore processed (log)") +
  fineprintutils::theme_map2() +
  ggplot2::theme(legend.position = "bottom",
                 legend.justification = "center") +
  guides(fill = guide_colorbar(
    barheight = unit(2, units = "mm"),
    barwidth = unit(140, units = "mm"),
    title.position = "top",
    title.hjust = 0.5,
    label.hjust = 1))
ggplot2::ggsave("brasil/output/maps/brasil_ore_2017.png", plot = last_plot(), device = "png",
                scale = 1, width = 400, height = 300, units = "mm")

# zoom into minas gerais
zoom_bbox <- tibble::tribble(
  ~region,                    ~x_lim,            ~y_lim,
  "Chile",          c(-76.00, -67.00), c(-57.00, -14.00),
  "Latin America",  c( -88.00, -31.00), c(-58.00, 13.00),
  "Peru",           c(-82.00, -68.00), c(-19.00, 00.50),
  "Mexico",         c(-119.00, -84.00), c(13.00, 33.00),
  "Minas Gerais",   c(-52.00, -39.00), c(-23.00, -13.00)
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
  dplyr::filter(region == "Minas Gerais")

sf_BRA_2017 %>% ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = log(ore_extraction))) +
  ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
  viridis::scale_fill_viridis(na.value = NA, option = "viridis") +
  ggplot2::geom_point(data = data %>% dplyr::filter(year == "2017"), aes(x = X, y = Y), size = 2, 
                      shape = 4, color = "red") +
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
ggplot2::ggsave("brasil/output/maps/minas_gerais_ore_2017.png", plot = last_plot(), device = "png",
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

# ports <- read.csv("brasil/input/ports.csv", sep = ";") MANAUS MISSING!!




# IBGE demographics -------------------------------------------------------

ibge_pop <- read.csv("brasil/input/ibge/pop_mod.csv", sep = ";", skip = 1, stringsAsFactors = FALSE) 
ibge_pop <- ibge_pop %>%
  dplyr::select(c(1:6)) %>%
  `colnames<-`(c("level", "id", "municipio", "year", "variable", "value")) %>%
  dplyr::filter(level == "MU") %>%
  dplyr::select(-level) %>%
  dplyr::mutate(state = stringr::str_sub(municipio,-4)) %>%
  dplyr::mutate(state = substr(state, 2, 3)) 
ibge_pop <- ibge_pop %>%
  dplyr::mutate(municipio = substr(municipio,1,nchar(ibge_pop$municipio)-4)) %>%
  dplyr::mutate(value = as.numeric(value))


# IBGE economics ----------------------------------------------------------

ibge_gdp <- read.csv("brasil/input/ibge/gdp_mod.csv", sep = ";", skip = 1, stringsAsFactors = FALSE) 
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
  dplyr::filter(variable == "Produto Interno Bruto a pre‡os correntes (Mil Reais)") %>%
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
                 legend.justification = "center") +
  guides(fill = guide_colorbar(
    barheight = unit(2, units = "mm"),
    barwidth = unit(140, units = "mm"),
    title.position = "top",
    title.hjust = 0.5,
    label.hjust = 1))
ggplot2::ggsave("brasil/output/maps/brasil_pop_2016.png", plot = last_plot(), device = "png",
                scale = 1, width = 400, height = 300, units = "mm")

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
                 legend.justification = "center") +
  guides(fill = guide_colorbar(
    barheight = unit(2, units = "mm"),
    barwidth = unit(140, units = "mm"),
    title.position = "top",
    title.hjust = 0.5,
    label.hjust = 1))
ggplot2::ggsave("brasil/output/maps/brasil_gdp_2016.png", plot = last_plot(), device = "png",
                scale = 1, width = 400, height = 300, units = "mm")

# gdp per capita
sf_panel %>%
  dplyr::filter(year == 2016) %>% 
  dplyr::mutate(gdp_cap = log(gdp_current_thousand_reais / pop)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = gdp_cap), colour="white", lwd = 0.1) +
  ggplot2::coord_sf(datum = NA) +
  viridis::scale_fill_viridis(na.value = NA, option = "viridis") +
  # ggplot2::geom_point(data = data %>% dplyr::filter(year == "2017"), aes(x = X, y = Y), size = 1, 
  #                     shape = 4, color = "red") +
  ggplot2::labs(title = "Regional GDP, Brasil, 2017", fill="GDP in thousand current Real per capita (log)") +
  fineprintutils::theme_map2() +
  ggplot2::theme(legend.position = "bottom",
                 legend.justification = "center") +
  guides(fill = guide_colorbar(
    barheight = unit(2, units = "mm"),
    barwidth = unit(140, units = "mm"),
    title.position = "top",
    title.hjust = 0.5,
    label.hjust = 1))
ggplot2::ggsave("brasil/output/maps/brasil_gdp_cap_2016.png", plot = last_plot(), device = "png",
                scale = 1, width = 400, height = 300, units = "mm")

# merge ports into data ---------------------------------------------------

# panel
spdf_panel <- spdf_panel %>% dplyr::left_join(ports %>% dplyr::select(GID_1, large_port), by = "GID_1")

# save full spatial data --------------------------------------------------

save(spdf_panel, file = "input/full_panel.RData")
