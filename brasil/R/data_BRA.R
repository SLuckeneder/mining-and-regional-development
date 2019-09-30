
library(sf)
library(tidyverse)
library(viridis)
library(fineprintutils)

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



### extend to shp to panel and merge extraction data

# stack spdf data t (= 19 years) times
sf_panel <- do.call("rbind", replicate(19, sf_panel, simplify = FALSE))
sf_panel <- sf_panel %>% dplyr::mutate(year = rep(as.character(2000:2018), each=5504))

# merge aggregates of selected year into sf
sf_panel <- sf_panel %>% dplyr::left_join(snl_data_sf_agg_panel, by = c("GID_2", "year"))




# ports -------------------------------------------------------------------

# Source: https://www.searoutes.com/worldwide-ports 

# TO BE IMPLEMENTED

# ports <- read.csv("input/ports.csv", sep = ";")


# OECD --------------------------------------------------------------------

# region concordance and subset vector
dstruc <- OECD::get_data_structure("REGION_DEMOGR") 
reg <- dstruc$REG_ID
reg_ids <- c(paste0("ME0", c(1:9)), paste0("ME", c(10:32)), 
             paste0("PE0", c(1:9)), paste0("PE", c(10:25)), 
             paste0("CL0", c(1:9)), paste0("CL", c(10:15)))

# demography

if("oecd_demography.RData" %in% dir("input/oecd")){
  load("input/oecd/oecd_demography.RData")
} else{
  
  dstruc <- OECD::get_data_structure("REGION_DEMOGR") 
  var_desc <- dstruc$VAR_DESC # dstruc variable descriptions
  var <- dstruc$VAR # list of available variables
  filter_list <- list()
  filter_list[[1]] <- 2 # TL2
  filter_list[[2]] <- reg_ids # region
  filter_list[[3]] <- c("POP_DEN", # population density (pop. per km2)
                        "T") # population, all ages
  filter_list[[4]] <- "T" # sex (M+F)
  oecd_demography <- OECD::get_dataset("REGION_DEMOGR", filter = filter_list) %>% # download
    dplyr::select(-TL, -SEX, -POS) %>%
    dplyr::left_join(var, by = c("VAR" = "id"))
  save(oecd_demography, file = "input/oecd/oecd_demography.RData")
}

# economy

if("oecd_economy.RData" %in% dir("input/oecd")){
  load("input/oecd/oecd_economy.RData")
} else{
  
  dstruc <- OECD::get_data_structure("REGION_ECONOM") 
  var_desc <- dstruc$VAR_DESC
  var <- dstruc$VAR
  filter_list <- list()
  filter_list[[1]] <- 2 # TL2
  filter_list[[2]] <- reg_ids # region
  filter_list[[3]] <- "SNA_2008" # Last SNA classification (SNA 2008 or latest available)
  filter_list[[4]] <- c("GDP", # Regional GDP
                        "GVA_TOTAL", # Regional Gross Value Added, total activities
                        "GVA_IND_10_VA", # GVA in agriculture, forestry and fishing (ISIC rev4)
                        "GVA_IND_10_VB_E", # GVA in industry, including energy (ISIC rev4)
                        "GVA_IND_10_VC", # ..Of which: GVA in manufacturing (ISIC rev4)
                        "GVA_IND_10_VK") # GVA in financial and insurance activities (ISIC rev4)
  filter_list[[5]] <- "USD_PPP" # measured in Millions USD, current prices, current PPP
  filter_list[[6]] <- "ALL" # All regions
  oecd_economy <- OECD::get_dataset("REGION_ECONOM", filter = filter_list) %>% # download
    dplyr::select(-TL, -POS) %>%
    dplyr::left_join(var, by = c("VAR" = "id"))
  save(oecd_economy, file = "input/oecd/oecd_economy.RData")
}

suppressWarnings(rm(dstruc, filter_list, reg, var, var_desc))

# merge data --------------------------------------------------------------

oecd_data <- dplyr::bind_rows(oecd_demography, oecd_economy)
suppressWarnings(rm(oecd_demography, oecd_economy))

# merge to spatial data ---------------------------------------------------

# load and apply concordance
conc <- read.csv("input/concordance_regions.csv", sep = ";", stringsAsFactors = FALSE)
oecd_data <- oecd_data %>% dplyr::left_join(conc, by = c("REG_ID" = "oecd_id"))

# merge into spdf_panel (takes time!!)
if("panel.RData" %in% dir("input")){
  load("input/panel.RData")
} else{
  spdf_panel <- spdf_panel %>%
    dplyr::left_join(oecd_data , by = c("GID_1" = "gadm_id", "year" = "TIME")) %>%
    dplyr::select(GID_1, year, ore_extraction, VAR, UNIT, obsValue) %>%
    tidyr::unite("VAR", c("VAR","UNIT"), sep ="-")
  
  spdf_panel <- tidyr::spread(spdf_panel, VAR, obsValue, drop = TRUE)
  save(spdf_panel, file = "input/panel.RData")
}

# merge ports into data ---------------------------------------------------

# panel
spdf_panel <- spdf_panel %>% dplyr::left_join(ports %>% dplyr::select(GID_1, large_port), by = "GID_1")

# save full spatial data --------------------------------------------------

save(spdf_panel, file = "input/full_panel.RData")
