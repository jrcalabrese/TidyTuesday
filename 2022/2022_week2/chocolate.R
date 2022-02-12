library(tidyverse)
library(stringr)
library(joycon)
library(extrafont)
library(ggthemr)
library(cowplot)

chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')
dat <- chocolate

dat <- dat %>%
  select(ref, cocoa_percent, most_memorable_characteristics, rating) %>%
  mutate(most_memorable_characteristics = 
           strsplit(as.character(most_memorable_characteristics), ",")) %>% 
  unnest(most_memorable_characteristics) %>%
  mutate(most_memorable_characteristics = 
           str_replace_all(most_memorable_characteristics, pattern=" ", repl="")) %>%
  mutate(cocoa_percent = parse_number(cocoa_percent)) 

flavor <- c("woody", "leather", "licorice", "sticky", "lemon", 
            "tobacco", "bourbon", "earthy", "strawberry", "hazelnut")

dat <- dat %>%
  subset(most_memorable_characteristics %in% flavor) %>%
  rename(char = most_memorable_characteristics) %>%
  mutate(char = str_to_sentence(char, locale = "en"))

dust_theme <- ggthemr('dust', 
                      set_theme = FALSE,
                      #layout = "clean"
                      )

val <- joycon_pal("Korok", 
                  n = 10, 
                  type = "continuous")

plot <- dat %>%
  ggplot(aes(x = reorder(char, -rating, FUN = mean),
             y = rating, fill = val)) +
  geom_boxplot(fill = val, color = "black") + 
  xlab("") +
  ylab("Chocolate Rating") +
  ggtitle("Hazelnut is a very good chocolate flavor.") +
  labs(subtitle = "Common flavors of chocolate and their quality rating.") +
  ylim(2, 4) +
  dust_theme$theme +
  theme(text=element_text(size = 20, family = "Garamond")) +
  theme(plot.title = element_text(face = "plain")) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 28))

plot <- ggdraw(add_sub(plot, 
                     "Data: Flavors of Cocoa | #TidyTuesday | @jrosecalabrese", 
                     x = 0.9, y = 1, hjust = 0.9, size = 14, color = "#5b4f41",
                     fontfamily = "Garamond"))


ggsave("chocolate.png", height=6, width=10.5)
