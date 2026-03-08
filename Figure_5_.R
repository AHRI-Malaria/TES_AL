library(readxl)
library(dplyr)
library(survival)
library(ggplot2)
library(scales)
library(patchwork)
library(ggpubr)
# read data
data_km <- read_excel("data/Figure_5_km_data.xlsx")

# make group variable
data_km <- data_km %>%
  mutate(
    corrected = factor(
      corrected,
      levels = c("PCR-corrected", "PCR-uncorrected")
    )
  )

# KM fit
fit <- survfit(Surv(day, status) ~ corrected, data = data_km)

# exact time points to show
time_points <- c(0, 1, 2, 3, 7, 14, 21, 28)

# extract KM summary at exact times
sfit <- summary(fit, times = time_points, extend = TRUE)

# survival curve data
plot_dat <- data.frame(
  time   = sfit$time,
  surv   = sfit$surv,
  strata = sfit$strata,
  n.risk = sfit$n.risk,
  n.event = sfit$n.event,
  n.censor = sfit$n.censor
)

# clean strata names
plot_dat$strata <- sub("^corrected=", "", plot_dat$strata)

# censor points only
censor_dat <- plot_dat %>%
  filter(n.censor > 0)

# main KM plot
p1 <- ggplot(plot_dat, aes(x = time, y = surv, color = strata)) +
  geom_step(linewidth = 1) +
  geom_point(
    data = censor_dat,
    aes(x = time, y = surv, color = strata),
    shape = 3,
    size = 2,
    stroke = 1
  ) +
  scale_color_manual(
    values = c("PCR-corrected" = "steelblue2",
               "PCR-uncorrected" = "salmon"),
    name = ""
  ) +
  scale_x_continuous(
    breaks = time_points,
    limits = c(0, 28),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    limits = c(0, 1.05),
    labels = percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  labs(
    x = "Follow-up time (days)",
    y = "Treatment success (%)"
  ) +
  theme_pubr(base_size = 14) +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

# number at risk table data
risk_dat <- plot_dat %>%
  select(time, strata, n.risk) %>%
  distinct()

# set row order for table
risk_dat$strata <- factor(
  risk_dat$strata,
  levels = c("PCR-corrected", "PCR-uncorrected")
)

# manual risk table
p2 <- ggplot(risk_dat, aes(x = time, y = strata, label = n.risk, color = strata)) +
  geom_text(size = 3) +
  scale_color_manual(
    values = c("PCR-corrected" = "steelblue2",
               "PCR-uncorrected" = "salmon"),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = time_points,
    limits = c(0, 28),
    expand = c(0.04, 0)
  ) +
  labs(
    x = "Follow-up time (days)",
    y = NULL,
    title = "Number at risk"
  ) +
  theme_pubr(base_size = 14) +
  theme(
    plot.title = element_text(size = 14, hjust = 0),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )

# combine
final_plot <- p1 / p2 + plot_layout(heights = c(4, 1.3))

final_plot

# save
ggsave(
  "plots/km_plot_figure_5.tiff",
  plot = final_plot,
  dpi = 600,
  height =6,
  width = 9,
  units = "in",
  compression = "lzw"
)


ggsave(
  "plots/km_plot_figure_5.pdf",
  plot = final_plot,
  dpi = 600,
  height =6,
  width = 9,
  units = "in"
)


