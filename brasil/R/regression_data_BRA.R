
load("brasil/input/full_panel.RData")

# neighbours --------------------------------------------------------------

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
  
# read spatial again
geometries_mu <- sf::st_read("brasil/input/spatial_weights.gpkg", stringsAsFactors = FALSE)
geometries_mu <- geometries_mu %>% 
  dplyr::filter(CD_GEOCMU %in% W_data$CD_GEOCMU) %>%
  dplyr::arrange(CD_GEOCMU)

# neighbours (see https://cran.r-project.org/web/packages/spdep/vignettes/nb_sf.html)
coords_sf <- sf::st_coordinates(sf::st_centroid(geometries_mu, of_largest_polygon = TRUE))
W_k <- spdep::knearneigh(coords_sf, k = k_nn) # takes 5-10 min
knear_nb <- spdep::knn2nb(W_k)
W_k <- spdep::nb2mat(knear_nb)
# plot neighbours
png("brasil/output/maps/knearest_map.png", width = 4000, height = 3000)
#pdf("brasil/output/maps/knearest_map.pdf", width = 400, height = 300)
plot(st_geometry(geometries_mu), border="grey", lwd = 6)
plot(knear_nb, coords = coords_sf, col = viridis(5)[2], lwd = 4, points=FALSE, add=TRUE)
dev.off()

# structure of W must fit Y and X
W_str <- sf::st_centroid(geometries_mu) %>%
  sf::st_set_geometry(NULL)

# subset panel ------------------------------------------------------------

data <- sf_panel %>% 
  dplyr::filter(year %in% t_vector) %>%
  dplyr::filter(CD_GEOCMU %in% W_data$CD_GEOCMU) %>%
  dplyr::arrange(year, CD_GEOCMU) 

# # check
# which(!W_str$CD_GEOCMU %in% data$CD_GEOCMU)
# which(!data$CD_GEOCMU %in% W_str$CD_GEOCMU)

# select variables
X <- data %>%
  dplyr::select("CD_GEOCMU", "state", "year", "gdp_current_thousand_reais", "ore_extraction", "pop", 
                "gva_agriculture_current_thousand_reais", "gva_industry_current_thousand_reais", "port") %>%
  dplyr::mutate(gva_agriculture_current_thousand_reais = log(gva_agriculture_current_thousand_reais+1)) %>%
  dplyr::mutate(gva_industry_current_thousand_reais = log(gva_industry_current_thousand_reais+1)) %>%
  dplyr::mutate(pop = log(pop)) %>%
  dplyr::mutate(ore_extraction = log(ore_extraction+1)) %>%
  replace(is.na(.), 0) # WORKS ONLY AS LONG AS ONLY ORE EXTRACTION DATA IS MISSING
# Dealing with NAs by setting = 0 is okay, since extraction is NA if there is none

# calculate growth (in %)
Y <- X %>%
  dplyr::group_by(CD_GEOCMU) %>%
  dplyr::mutate(g = log(gdp_current_thousand_reais / lag(gdp_current_thousand_reais,g_horizon)) / g_horizon * 100) %>%
  #dplyr::mutate(g = (gdp_current_thousand_reais - lag(gdp_current_thousand_reais,g_horizon)) /  lag(gdp_current_thousand_reais,g_horizon)  * 100) %>%
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
coefs <- vars[! vars %in% c("CD_GEOCMU", "state", "year")]

# year dummies
D <- dummies::dummy(X$year, sep = "_")[,-1]

# state dummies
C <- X %>%
  dplyr::mutate(bra_1 = ifelse(state  == "AC", 1, 0)) %>%
  dplyr::mutate(bra_2 = ifelse(state  == "AL", 1, 0)) %>%
  dplyr::mutate(bra_3 = ifelse(state  == "AM", 1, 0)) %>%
  dplyr::mutate(bra_4 = ifelse(state  == "AP", 1, 0)) %>%
  dplyr::mutate(bra_5 = ifelse(state  == "BA", 1, 0)) %>%
  dplyr::mutate(bra_6 = ifelse(state  == "CE", 1, 0)) %>%
  dplyr::mutate(bra_7 = ifelse(state  == "DF", 1, 0)) %>%
  dplyr::mutate(bra_8 = ifelse(state  == "ES", 1, 0)) %>%
  dplyr::mutate(bra_9 = ifelse(state  == "GO", 1, 0)) %>%
  dplyr::mutate(bra_10 = ifelse(state  == "MA", 1, 0)) %>%
  dplyr::mutate(bra_11 = ifelse(state  == "MG", 1, 0)) %>%
  dplyr::mutate(bra_12 = ifelse(state  == "MS", 1, 0)) %>%
  dplyr::mutate(bra_13 = ifelse(state  == "MT", 1, 0)) %>%
  dplyr::mutate(bra_14 = ifelse(state  == "PA", 1, 0)) %>%
  dplyr::mutate(bra_15 = ifelse(state  == "PB", 1, 0)) %>%
  dplyr::mutate(bra_16 = ifelse(state  == "PE", 1, 0)) %>%
  dplyr::mutate(bra_17 = ifelse(state  == "PI", 1, 0)) %>%
  dplyr::mutate(bra_18 = ifelse(state  == "PR", 1, 0)) %>%
  dplyr::mutate(bra_19 = ifelse(state  == "RJ", 1, 0)) %>%
  dplyr::mutate(bra_20 = ifelse(state  == "RN", 1, 0)) %>%
  dplyr::mutate(bra_21 = ifelse(state  == "RO", 1, 0)) %>%
  dplyr::mutate(bra_22 = ifelse(state  == "RR", 1, 0)) %>%
  dplyr::mutate(bra_23 = ifelse(state  == "RS", 1, 0)) %>%
  dplyr::mutate(bra_24 = ifelse(state  == "SC", 1, 0)) %>%
  dplyr::mutate(bra_25 = ifelse(state  == "SE", 1, 0)) %>%
  dplyr::mutate(bra_26 = ifelse(state  == "SP", 1, 0)) %>%
  dplyr::mutate(bra_27 = ifelse(state  == "TO", 1, 0)) %>%
  dplyr::select(bra_1,bra_2,bra_3,bra_4,bra_5,bra_6,bra_7,bra_8,bra_9,bra_10,
                bra_11,bra_12,bra_13,bra_14,bra_15,bra_16,bra_17,bra_18,bra_19,bra_20,
                bra_21,bra_22,bra_23,bra_24,bra_25,bra_26,bra_27)

# remove year and transform to numeric matrices
Y <- as.numeric(Y$g)
X <- X %>% dplyr::select(-state, -CD_GEOCMU, -year)
X <- matrix(as.numeric(unlist(X)),nrow=nrow(X))
C <- matrix(as.numeric(unlist(C)),nrow=nrow(C))


# save matrices -----------------------------------------------------------




