
# domestic extraction data ------------------------------------------------

# Source: https://www.resourcepanel.org/global-material-flows-database 

load("input/domestic_extraction.RData")

cat <- "Metals"

# arrange data
dat <- q %>%
  dplyr::filter(material_category %in% cat) %>%
  dplyr::mutate(region = "Rest of the World") %>%
  dplyr::mutate(region = ifelse(MRIO == "CHL", "Chile", region)) %>%
  dplyr::mutate(region = ifelse(MRIO == "PER", "Peru", region)) %>%
  dplyr::mutate(region = ifelse(MRIO == "MEX", "Mexico", region)) %>%
  dplyr::group_by(region, year, material_category) %>%
  dplyr::summarise(extraction = sum(extraction))


# plot --------------------------------------------------------------------

p <- dat %>% ggplot2::ggplot(aes(x = year, y = extraction, fill = region, group = region)) +
  ggplot2::geom_area(position = "stack") +
  ggplot2::labs(x = NULL, y = "Million tonnes", title = NULL) +
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(), labels=scales::comma) +
  ggplot2::scale_x_discrete(expand = c(0, 0), breaks = seq(1970, 2017, 2)) +
  ggplot2::scale_fill_manual(values = c("#1f78b4","#b2df8a","#33a02c", "#a6cee3")) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom", 
                 legend.title=element_blank(),
                 legend.text=element_text(size=14),
                 legend.spacing.x = unit(0.1, 'cm'),
                 panel.grid.minor =   element_blank(),
                 panel.grid.major =   element_blank(),
                 axis.line = element_line(colour = "black"),
                 panel.border = element_blank(),
                 axis.title.y = element_text(hjust = 1, size = 16),
                 axis.text.y = element_text(size = 14),
                 axis.text.x = element_text(angle = 90, vjust = 0.5, size = 14))

pdf(file="output/metal_extraction_global.pdf", width = 8, height = 6)
grid.arrange(p)
dev.off()


# total
dat <- q %>%
  dplyr::filter(material_category %in% cat) %>%
  dplyr::group_by(year, material_category) %>%
  dplyr::summarise(extraction = sum(extraction))

p <- dat %>% ggplot2::ggplot(aes(x = year, y = extraction, group = 1)) +
  ggplot2::geom_rect(aes(xmin= "2000", xmax=max(year), ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.2) +
  ggplot2::geom_line() +
  ggplot2::labs(x = NULL, y = "Million tonnes", title = "Global metal ore extraction since 1970", caption = "Source: UN IRP Global Material Flows Database") +
  ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 10000), breaks = scales::pretty_breaks(), labels=scales::comma) +
  ggplot2::scale_x_discrete(expand = c(0, 0), breaks = seq(1970, 2017, 2)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom", 
                 legend.title=element_blank(),
                 legend.text=element_text(size=14),
                 legend.spacing.x = unit(0.1, 'cm'),
                 panel.grid.minor =   element_blank(),
                 panel.grid.major =   element_blank(),
                 axis.line = element_line(colour = "black"),
                 panel.border = element_blank(),
                 axis.title.y = element_text(hjust = 1, size = 16),
                 axis.text.y = element_text(size = 14),
                 axis.text.x = element_text(angle = 90, vjust = 0.5, size = 14))

pdf(file="output/metal_extraction_global_total.pdf", width = 9, height = 6)
grid.arrange(p)
dev.off()

