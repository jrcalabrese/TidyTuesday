  # title: 'TidyTuesday: US Broadband'
  # author: "Julianna Calabrese"
  # date: "5/11/2021"
  
library(tidyverse)
library(maps)
library(ggmap)
library(gganimate)
library(cowplot)
library(scales)

broadband <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv")

ms_broadband <- broadband %>%
  filter(ST == "MS")

oh_broadband <- broadband %>%
  filter(ST == "OH")

ms <- map_data('county', 'mississippi')
oh <- map_data('county', 'ohio')

state.ms <- map_data('state', 'mississippi')
state.oh <- map_data('state', 'ohio')

ms_broadband$COUNTY.NAME <- word(ms_broadband$COUNTY.NAME, 1)
ms_broadband$COUNTY.NAME <- tolower(ms_broadband$COUNTY.NAME)
names(ms_broadband)[names(ms_broadband) == "COUNTY.NAME"] <- "subregion"

oh_broadband$COUNTY.NAME <- word(oh_broadband$COUNTY.NAME, 1)
oh_broadband$COUNTY.NAME <- tolower(oh_broadband$COUNTY.NAME)
names(oh_broadband)[names(oh_broadband) == "COUNTY.NAME"] <- "subregion"

ms_data <- inner_join(ms, ms_broadband, by="subregion")
oh_data <- inner_join(oh, oh_broadband, by="subregion")

ms_data$BROADBAND.USAGE <- as.numeric(ms_data$BROADBAND.USAGE)

oh_data$BROADBAND.USAGE <- as.numeric(oh_data$BROADBAND.USAGE)

ms_plot <- ggplot() + 
  geom_polygon(data = ms_data, aes(x=long, y = lat, fill=BROADBAND.USAGE, group = group), color="white") + 
  geom_polygon(data = state.ms, aes(x=long, y=lat, group=group), color="black", fill=NA) +
  coord_fixed(1.3) +
  scale_fill_gradient(low = "#ffffe0", high = "#000f89", na.value="grey80", 
                      limits=c(0, .6), labels = percent_format(accuracy = 1)) +
  theme_void() +
  labs(fill=' ') 

oh_plot <- ggplot() + 
  geom_polygon(data = oh_data, aes(x=long, y = lat, fill=BROADBAND.USAGE, group = group), color="white") + 
  geom_polygon(data = state.oh, aes(x=long, y=lat, group=group), color="black", fill=NA) +
  coord_fixed(1.3) +
  scale_fill_gradient(low = "#ffffe0", high = "#000f89", na.value="grey80", 
                      limits=c(0, .6), labels = percent_format(accuracy = 1)) +
  theme_void()

prow <- plot_grid(
  ms_plot + theme(legend.position="none"),
  oh_plot + theme(legend.position="none"),
  align = 'none',
  hjust = -1,
  nrow = 1
)
prow

legend <- get_legend(
  ms_plot + theme(legend.box.margin = margin(0, 0, 0, 12))
)

title <- ggdraw() + draw_label("\n\nMy Home State versus my Current State:\nPercent of people per county that use the Internet at\nbroadband speeds of 25 Mbps/3 Mbps as of 2017\n")

p <- plot_grid(prow, legend, rel_widths = c(3, .4))

pp <- plot_grid(title, p, ncol=1, rel_heights=c(0.2, 1))

ggdraw(add_sub(pp, "Data: Microsoft\n#TidyTuesday\n@jrosecalabrese", x=0.9, hjust=0.9, size=12))
