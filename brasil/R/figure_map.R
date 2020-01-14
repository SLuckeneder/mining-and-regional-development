# plot mines and growth rates ---------------------------------------------

load("brasil/input/full_panel.RData")
load("brasil/input/mine_data.RData")

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

# gdp per capita
pfull <- sf_panel %>%
  dplyr::filter(year == 2016) %>% 
  dplyr::mutate(gdp_cap = log(gdp_current_thousand_reais / pop)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = gdp_cap), colour="white", lwd = 0.1) +
  ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
  viridis::scale_fill_viridis(na.value = NA, option = "viridis") +
  ggplot2::geom_point(data = data %>% dplyr::filter(year == "2016"), aes(x = X, y = Y), size = 1,
                      shape = 15, color = "red") +
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

lim <- zoom_bbox %>% 
  dplyr::filter(region == "Minas Gerais")

pzoom <- sf_panel %>%
  dplyr::filter(year == 2016) %>% 
  dplyr::mutate(gdp_cap = log(gdp_current_thousand_reais / pop)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = gdp_cap), colour="white", lwd = 0.1) +
  ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
  viridis::scale_fill_viridis(na.value = NA, option = "viridis") +
  ggplot2::geom_point(data = data %>% dplyr::filter(year == "2016"), aes(x = X, y = Y), size = 2,
                      shape = 15, color = "red") +
  ggplot2::labs(title = NULL, fill="GDP in thousand current Real per capita (log)") +
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

# put Brasil 2016 and minas gerais zoom together
lim <- zoom_bbox %>% 
  dplyr::filter(region == "Brasil")

g <- ggplotGrob(pzoom)
p <- pfull +
  ggplot2::theme(plot.margin=unit(c(-10, 100, 0, -10),"mm")) +
  annotation_custom(grob = g, 
                    xmin = max(lim$x_lim[[1]]) - 12, xmax = max(lim$x_lim[[1]]) + 23, 
                    ymin = min(lim$y_lim[[1]]), ymax = min(lim$y_lim[[1]]) + 20) 
ggplot2::ggsave("brasil/output/maps/brasil_gdp_mines_2016_zoom.png", plot = p, device = "png",
                scale = 1, width = 400, height = 300, units = "mm")


