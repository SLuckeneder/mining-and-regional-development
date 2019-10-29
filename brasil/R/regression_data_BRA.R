
load("brasil/input/full_panel.RData")
load("brasil/input/missing_polygons.RData")

# neighbours --------------------------------------------------------------

W_data <- sf_BRA %>% 
  dplyr::filter(NAME_0 %in% c_vector1)

# neighbours (see https://cran.r-project.org/web/packages/spdep/vignettes/nb_sf.html)
# exlude all regions for which there is no IBGE data

coords_sf <- sf::st_coordinates(sf::st_centroid(W_data) %>% dplyr::filter(!GID_2 %in% missing_polygons))
W_k <- spdep::knearneigh(coords_sf, k = k_nn)
knear_nb <- spdep::knn2nb(W_k)
W_k <- spdep::nb2mat(knear_nb)

# structure of W must fit Y and X
W_str <- sf::st_centroid(W_data) %>% dplyr::filter(!GID_2 %in% missing_polygons) %>%
  sf::st_set_geometry(NULL)

# subset panel ------------------------------------------------------------

data <- sf_panel %>% 
  dplyr::filter(year %in% t_vector) %>%
  dplyr::filter(substr(GID_2, 1, 3) %in% c_vector2)

# select variables
X <- data %>%
  `colnames<-`(gsub("-.*", "", colnames(data))) %>%
  sf::st_set_geometry(NULL) %>% # drop geometry
  dplyr::select("GID_2", "year", "gdp_current_thousand_reais", "ore_extraction", "pop", 
                "gva_agriculture_current_thousand_reais", "gva_industry_current_thousand_reais", "large_port") %>%
  dplyr::mutate(gva_agriculture_current_thousand_reais = log(gva_agriculture_current_thousand_reais)) %>%
  dplyr::mutate(gva_industry_current_thousand_reais = log(gva_industry_current_thousand_reais)) %>%
  dplyr::mutate(pop = log(pop)) %>%
  dplyr::mutate(ore_extraction = log(ore_extraction+1)) %>%
  replace(is.na(.), 0) %>% # WORKS ONLY AS LONG AS ONLY ORE EXTRACTION DATA IS MISSING
  dplyr::filter(gdp_current_thousand_reais > 0)

# Dealing with NAs by setting = 0 is okay, since extraction is NA if there is none, 
# and other NAs drop later because it is 2014 and 2015 (Peru) data

# calculate growth (in %)
Y <- X %>%
  dplyr::group_by(GID_2) %>%
  dplyr::arrange(GID_2, year) %>%
  dplyr::mutate(g = log(gdp_current_thousand_reais / lag(gdp_current_thousand_reais,g_horizon)) / g_horizon * 100) %>%
  dplyr::mutate(g = lead(g, g_horizon)) %>%
  dplyr::select(GID_2, year, g) %>%
  dplyr::filter(! year %in% drop_horizon) %>%
  dplyr::arrange(year, factor(GID_2, levels = W_str$GID_2))

# calculate log initial income
vars <- c("int", "gdp_current_thousand_reais", colnames(X)[! colnames(X) %in% "gdp_current_thousand_reais"])
X <- X %>% 
  dplyr::mutate(gdp_current_thousand_reais = log(gdp_current_thousand_reais)) %>%
  dplyr::arrange(year, factor(GID_2, levels = W_str$GID_2)) %>% 
  dplyr::mutate(int = rep(1, nrow(X))) %>%
  dplyr::select(vars) %>%
  dplyr::filter(! year %in% drop_horizon)
coefs <- vars[! vars %in% c("GID_2", "year")]

# year dummies
D <- dummies::dummy(X$year, sep = "_")[,-1]

# state dummies
C <- X %>%
  dplyr::mutate(bra_1 = ifelse(substr(GID_2, 1, 6) == "BRA.1.", 1, 0)) %>%
  dplyr::mutate(bra_2 = ifelse(substr(GID_2, 1, 6) == "BRA.2.", 1, 0)) %>%
  dplyr::mutate(bra_3 = ifelse(substr(GID_2, 1, 6) == "BRA.3.", 1, 0)) %>%
  dplyr::mutate(bra_4 = ifelse(substr(GID_2, 1, 6) == "BRA.4.", 1, 0)) %>%
  dplyr::mutate(bra_5 = ifelse(substr(GID_2, 1, 6) == "BRA.5.", 1, 0)) %>%
  dplyr::mutate(bra_6 = ifelse(substr(GID_2, 1, 6) == "BRA.6.", 1, 0)) %>%
  dplyr::mutate(bra_7 = ifelse(substr(GID_2, 1, 6) == "BRA.7.", 1, 0)) %>%
  dplyr::mutate(bra_8 = ifelse(substr(GID_2, 1, 6) == "BRA.8.", 1, 0)) %>%
  dplyr::mutate(bra_9 = ifelse(substr(GID_2, 1, 6) == "BRA.9.", 1, 0)) %>%
  dplyr::mutate(bra_10 = ifelse(substr(GID_2, 1, 6) == "BRA.10", 1, 0)) %>%
  dplyr::mutate(bra_11 = ifelse(substr(GID_2, 1, 6) == "BRA.11", 1, 0)) %>%
  dplyr::mutate(bra_12 = ifelse(substr(GID_2, 1, 6) == "BRA.12", 1, 0)) %>%
  dplyr::mutate(bra_13 = ifelse(substr(GID_2, 1, 6) == "BRA.13", 1, 0)) %>%
  dplyr::mutate(bra_14 = ifelse(substr(GID_2, 1, 6) == "BRA.14", 1, 0)) %>%
  dplyr::mutate(bra_15 = ifelse(substr(GID_2, 1, 6) == "BRA.15", 1, 0)) %>%
  dplyr::mutate(bra_16 = ifelse(substr(GID_2, 1, 6) == "BRA.16", 1, 0)) %>%
  dplyr::mutate(bra_17 = ifelse(substr(GID_2, 1, 6) == "BRA.17", 1, 0)) %>%
  dplyr::mutate(bra_18 = ifelse(substr(GID_2, 1, 6) == "BRA.18", 1, 0)) %>%
  dplyr::mutate(bra_19 = ifelse(substr(GID_2, 1, 6) == "BRA.19", 1, 0)) %>%
  dplyr::mutate(bra_20 = ifelse(substr(GID_2, 1, 6) == "BRA.20", 1, 0)) %>%
  dplyr::mutate(bra_21 = ifelse(substr(GID_2, 1, 6) == "BRA.21", 1, 0)) %>%
  dplyr::mutate(bra_22 = ifelse(substr(GID_2, 1, 6) == "BRA.22", 1, 0)) %>%
  dplyr::mutate(bra_23 = ifelse(substr(GID_2, 1, 6) == "BRA.23", 1, 0)) %>%
  dplyr::mutate(bra_24 = ifelse(substr(GID_2, 1, 6) == "BRA.24", 1, 0)) %>%
  dplyr::mutate(bra_25 = ifelse(substr(GID_2, 1, 6) == "BRA.25", 1, 0)) %>%
  dplyr::mutate(bra_26 = ifelse(substr(GID_2, 1, 6) == "BRA.26", 1, 0)) %>%
  dplyr::mutate(bra_27 = ifelse(substr(GID_2, 1, 6) == "BRA.27", 1, 0)) %>%
  dplyr::select(bra_1,bra_2,bra_3,bra_4,bra_5,bra_6,bra_7,bra_8,bra_9,bra_10,
                bra_11,bra_12,bra_13,bra_14,bra_15,bra_16,bra_17,bra_18,bra_19,bra_20,
                bra_21,bra_22,bra_23,bra_24,bra_25,bra_26,bra_27)

# remove year and transform to numeric matrices
Y <- as.numeric(Y$g)
X <- X %>% dplyr::select(-GID_2, -year)
X <- matrix(as.numeric(unlist(X)),nrow=nrow(X))
C <- matrix(as.numeric(unlist(C)),nrow=nrow(C))


