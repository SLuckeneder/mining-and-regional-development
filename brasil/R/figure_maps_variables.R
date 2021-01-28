

load("brasil/input/full_panel.RData")
t_vector <- seq(2002, 2016, 2)
g_horizon <- 1
drop_horizon <- c((max(t_vector)-g_horizon+1) : max(t_vector))

yr <- "2010"


# prepare data ------------------------------------------------------------

W_data <- sf_panel %>% 
  dplyr::filter(gdp_current_thousand_reais > 0) %>% 
  dplyr::filter(gva_agriculture_current_thousand_reais > 0) %>% 
  dplyr::filter(gva_industry_current_thousand_reais > 0) %>% 
  dplyr::group_by(CD_GEOCMU) %>% 
  dplyr::mutate(n = n()) %>% 
  dplyr::filter(n == 15) %>%
  dplyr::ungroup() %>%
  dplyr::filter(year == t_vector[1]) %>%
  dplyr::arrange(year, CD_GEOCMU)

# read spatial
geometries_mu <- sf::st_read("brasil/input/spatial_weights.gpkg", stringsAsFactors = FALSE)
geometries_mu <- geometries_mu %>% 
  dplyr::filter(CD_GEOCMU %in% W_data$CD_GEOCMU) %>%
  dplyr::arrange(CD_GEOCMU)

data <- sf_panel %>% 
  dplyr::filter(year %in% t_vector) %>%
  dplyr::filter(CD_GEOCMU %in% W_data$CD_GEOCMU) %>%
  dplyr::arrange(CD_GEOCMU) 

# select variables
X <- data %>%
  dplyr::select("CD_GEOCMU", "state", "year", "gdp_current_thousand_reais", "ore_extraction", "pop", 
                "gva_agriculture_current_thousand_reais", "gva_industry_current_thousand_reais", "port") %>%
  dplyr::mutate(gva_agriculture_current_thousand_reais = log(gva_agriculture_current_thousand_reais+1)) %>%
  dplyr::mutate(gva_industry_current_thousand_reais = log(gva_industry_current_thousand_reais+1)) %>%
  dplyr::mutate(pop_log = log(pop)) %>%
  dplyr::mutate(ore_extraction_log = log(ore_extraction+1)) %>%
  replace(is.na(.), 0) # WORKS ONLY AS LONG AS ONLY ORE EXTRACTION DATA IS MISSING
# Dealing with NAs by setting = 0 is okay, since extraction is NA if there is none

# calculate growth (in %)
Y <- X %>%
  dplyr::group_by(CD_GEOCMU) %>%
  # dplyr::mutate(g = log(gdp_current_thousand_reais / lag(gdp_current_thousand_reais,g_horizon)) / g_horizon * 100) %>%
  dplyr::mutate(g = (gdp_current_thousand_reais - lag(gdp_current_thousand_reais,g_horizon)) /  lag(gdp_current_thousand_reais,g_horizon)  * 100) %>%
  dplyr::mutate(g = lead(g, g_horizon)) %>%
  dplyr::select(CD_GEOCMU, year, g) %>%
  dplyr::filter(! year %in% drop_horizon) 


# calculate log initial income
vars <- c("int", "gdp_current_thousand_reais", colnames(X)[! colnames(X) %in% "gdp_current_thousand_reais"])
X <- X %>% 
  dplyr::mutate(gdp_current_thousand_reais = log(gdp_current_thousand_reais)) %>%
  dplyr::mutate(int = rep(1, nrow(X))) %>%
  dplyr::select(vars) %>%
  dplyr::filter(! year %in% drop_horizon)




X %>% filter(CD_GEOCMU == "1100015")
Y %>% filter(CD_GEOCMU == "1100015")


# spatial and cross section -----------------------------------------------

Y <- geometries_mu %>% dplyr::left_join(Y, by = "CD_GEOCMU") %>%
  dplyr::filter(year == yr)


# draw maps setup ----------------------------------------------------------

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

# draw maps ---------------------------------------------------------------

lim <- zoom_bbox %>% 
  dplyr::filter(region == "Brasil")

Y %>%
  dplyr::filter(g < quantile(g, 0.9)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = g), colour="white", lwd = 0.1) +
  ggplot2::coord_sf(datum = NA) +
  viridis::scale_fill_viridis(na.value = NA, option = "viridis") +
  #ggplot2::labs(title = "Population, Brasil, 2017", fill="Inhabitants (log)") +
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
  



