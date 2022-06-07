library(tidyverse)
library(glue)
library(cowplot)

# sim data for gauge, should mimic actual data range

df <- tibble(
  x = seq(32.5, 82.5, by = 5),
  y =1
)

# sim data

rando_data <- tibble(
  x = sample(c(35:80), 50, replace = TRUE),
  y = sample(c(35:80), 50, replace = TRUE)
) 

# Plot for illustrative purposes

p <- rando_data %>%
  ggplot(aes(x = x, y = y, color = x)) +
  geom_point() +
  scale_x_continuous(limits = c(15, NA)) +
  scale_y_continuous(limits = c(15, NA)) +
  scale_color_viridis_b(option = "magma", direction = -1,
                        breaks = c(seq(35, 85, by = 5))) +
  theme(legend.position = "none") +
  labs(title = "Simulating a gauge plot/legend")

# Gauge plot
g <- df %>%
  ggplot(aes(x = x, y = y, fill = as.character(x))) +
  # Add rect for white circular background
  annotate(geom = "rect", xmin = 20, xmax = 95, ymin = -3, ymax = 2,
           fill = "white") +
  geom_col() +
  # Text labels
  geom_text(aes(label = ifelse(x == 32.5, "", x-2.5), x = x-2.5, y = 1.5), size = 3) +
  # Center text, large number 
  annotate(geom = "text", x = 57.5, y = -2.5, 
           label = glue("{round(mean(rando_data$x), 1)} dB"), size = 8) +
  # Center subtext
  annotate(geom = "text", x = 20, y = -2, 
           label = glue("(avg)"), size = 5) +
  # Gauge needle
  annotate(geom = "segment", x= mean(rando_data$x), xend = mean(rando_data$x),
           y = 0, yend = .25, arrow = arrow(type = "closed")) +
  # need to expand limits so columns don't extend to center
  scale_y_continuous(limits = c(-3, NA)) +
  scale_fill_viridis_d(option = "magma", direction = -1,
                       breaks = seq(35, 85, by = 5)) +
  # rotated so gauge is upright
  coord_polar(start = 3.15) +
  theme_void() +
  # ironic that we're remove the legend for this
  theme(legend.position = "none")

# Combining for illustrative purposes.
ggdraw() +
  draw_plot(p) +
  draw_plot(g, scale = .45, halign = 0.1, valign = 0.1)

ggsave("example_plot.png")
