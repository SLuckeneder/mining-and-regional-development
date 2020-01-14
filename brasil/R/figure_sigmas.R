

load("brasil/output/initial_store_BRA_2020-01-13.RData")


sigmas <- store[["2full"]][["sigmas"]]

dat <- rbind(
  cbind(model = "2y growth horizon (no interaction)",reshape2::melt(sigmas))
)

# dat <- dat %>%
#   dplyr::mutate(Var1 = ifelse(Var1 == 1, "Mexico", ifelse(Var1 == 2, "Peru", "Chile")))
# 
# dat$Var1 <- factor(dat$Var1, levels = c("Mexico", "Peru", "Chile"))

p <- dat %>%
  ggplot2::ggplot(aes(x = value, group = Var1)) +
  ggplot2::geom_density(alpha = 0.6, fill = "lightgrey") +
  ggplot2::scale_y_continuous(expand = c(0, 0, 0.05, 0)) +
  ggplot2::scale_x_continuous(limits = c(0, 200)) +
  ggplot2::labs(x = NULL, y = NULL, title = NULL) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom", 
                 legend.title=element_blank(),
                 legend.text=element_text(size=14),
                 legend.spacing.x = unit(0.2, 'cm'),
                 strip.text.x = element_text(size = 14),
                 panel.grid.minor =   element_blank(),
                 panel.grid.major =   element_blank(),
                 axis.title.y = element_text(hjust = 1, size = 16),
                 axis.text.y = element_text(size = 14),
                 axis.text.x = element_text(vjust = 0.5, size = 14))

pdf(file="brasil/output/sigmas_posterior_dist.pdf", width = 8, height = 6)
grid.arrange(p)
dev.off()


# sigmas2_full <- store[["2full"]][["sigmas"]]
# sigmas5_full <- store[["5full"]][["sigmas"]]
# 
# sigmas2_int <- store[["2int"]][["sigmas"]]
# sigmas5_int <- store[["5int"]][["sigmas"]]
# 
# dat <- rbind(
#   cbind(model = "2y growth horizon (no interaction)",reshape2::melt(sigmas2_full)),
#   cbind(model = "5y growth horizon (no interaction)",reshape2::melt(sigmas5_full)),
#   cbind(model = "2y growth horizon (interaction)",reshape2::melt(sigmas2_int)),
#   cbind(model = "5y growth horizon (interaction)",reshape2::melt(sigmas5_int))
# )
# 
# dat <- dat %>%
#   dplyr::mutate(Var1 = ifelse(Var1 == 1, "Mexico", ifelse(Var1 == 2, "Peru", "Chile")))
# 
# dat$Var1 <- factor(dat$Var1, levels = c("Mexico", "Peru", "Chile"))
# 
# p <- dat %>%
#   ggplot2::ggplot(aes(x = value, group = Var1, fill = Var1)) +
#   ggplot2::geom_density(alpha = 0.6) +
#   ggplot2::facet_wrap(~model, nrow = 2, scales = "free") +
#   ggplot2::scale_y_continuous(expand = c(0, 0, 0.05, 0)) +
#   ggplot2::labs(x = NULL, y = NULL, title = NULL) +
#   ggplot2::scale_fill_manual(values = c("Chile" = "#1f78b4","Mexico" = "#b2df8a","Peru" = "#33a02c")) +
#   ggplot2::theme_bw() +
#   ggplot2::theme(legend.position = "bottom", 
#                  legend.title=element_blank(),
#                  legend.text=element_text(size=14),
#                  legend.spacing.x = unit(0.2, 'cm'),
#                  strip.text.x = element_text(size = 14),
#                  panel.grid.minor =   element_blank(),
#                  panel.grid.major =   element_blank(),
#                  axis.title.y = element_text(hjust = 1, size = 16),
#                  axis.text.y = element_text(size = 14),
#                  axis.text.x = element_text(vjust = 0.5, size = 14))
# 
# pdf(file="brasil/output/sigmas_posterior_dist.pdf", width = 8, height = 6)
# grid.arrange(p)
# dev.off()
