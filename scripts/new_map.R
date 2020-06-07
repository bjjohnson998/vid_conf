library(magrittr)
library(tidyverse)
library(plm)
library(stargazer)
library(scales)
library(grid)
library(gtable)
library(haven)
library(foreign)
library(lmtest)
library(broom)
library(lubridate)
library(RColorBrewer)
library(usmap)
library(mapproj)

dt1 <- read.csv("~/Research/debate_turn_vid.csv")

dt1 <- dt1 %>% 
  select(
    State.Name, State.Abbreviation, `FIPS.Code`, `Type.of.Voter.ID`, `Year.In.Effect`
  ) %>% 
  filter(
    `State.Abbreviation` != "USA"
  ) %>% 
  mutate(
    law = Type.of.Voter.ID %>% factor %>% plyr::mapvalues(
      c("Debate", "Non-Strict Non-Photo ID", "Non-Strict Photo ID", 
        "None", "Strict Non-Photo ID", "Strict Photo ID"),
      c("Failed Bill", "Non-Photo or\nNon-Strict", "None", "Non-Photo or\nNon-Strict", "Strict Photo") %>% rep(c(1,2,1,1,1))
    ) %>% relevel("None")
  )

mapt <- dt1 %>% 
  select(
    State.Name, law
  ) %>% 
  set_names(c("full","law"))

mapd <- us_map() %>% select(fips, full) %>% 
  group_by(full) %>% 
  summarize(fips = fips %>% first)

mapt <- mapt %>% full_join(mapd)


mapcols <- brewer.pal(4,"Greys")


#Figure 1
plot_usmap(data = mapt,regions = "states", values = "law")+
  scale_fill_manual(name = "Type of Voter ID Law", values = mapcols)+
  theme(legend.position =c(.85,.15), legend.background = element_blank(),
        legend.title = element_text(size = 8), 
              legend.text = element_text(size = 6))+
  ggsave(filename = "~/Research/Writing/Publication Attempts/SPPQ/newmap.png",
         width = 7, height = 5, units = "in", dpi  = 300, type = "cairo")
