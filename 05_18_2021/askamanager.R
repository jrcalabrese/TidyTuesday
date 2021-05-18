#title: "Ask a Manager"
#author: "Julianna Calabrese"
#date: "5/18/2021"

library(tidyverse)
library(joycon)
library(ggridges)
library(scales)
library(ggthemes)
options(scipen=99999)

survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

survey <- survey %>%
  filter(currency == "USD") %>%
  filter(industry %in% c("Computing or Tech", "Social Work", "Health care", "Law", "Retail",
                         "Art & Design", "Sales", "Hospitality & Events"))

ggplot(survey, 
       aes(x = annual_salary, y = industry, fill = industry)) +
  geom_density_ridges() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_fill_manual(values = joycon_pal(name = "hyrule")) +
  theme_gray() + 
  theme(legend.position = "none") +
  ggtitle("Annual Salaries among Industries") +
  ylab("") + xlab("Annual Salary") +
  labs(caption = "Source: Ask a Manager Survey\n#TidyTuesday\n@jrosecalabrese") +
  coord_cartesian(xlim=c(0, 300000)) 
