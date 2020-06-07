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

laws <- "https://github.com/bjjohnson998/vid_conf/raw/master/data/raw_data/vid_laws.csv" %>% read.csv

laws <- laws %>% 
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

#usmap is pretty particular about how you create the table that forms the backbone of the map
#here I am fitting myself to work with it
mapt <- laws %>% 
  select(
    State.Name, law
  ) %>% 
  set_names(c("full","law"))

mapd <- us_map() %>% select(fips, full) %>% 
  group_by(full) %>% 
  summarize(fips = fips %>% first)

mapt <- mapt %>% full_join(mapd)


mapcols <- brewer.pal(4,"Greys")


#Figure 1 - usmap gives a lot of warnings, but there's nothing seriously wrong happening
#the file path specified here is the directory on my machine where the .tex file is
#so pay attention to the ggsave() if you're running this yourself
plot_usmap(data = mapt,regions = "states", values = "law")+
  scale_fill_manual(name = "Type of Voter ID Law", values = mapcols)+
  theme(legend.position =c(.85,.15), legend.background = element_blank(),
        legend.title = element_text(size = 8), 
              legend.text = element_text(size = 6))+
  ggsave(filename = "~/Research/Writing//SPPQ/newmap.png",
         width = 7, height = 5, units = "in", dpi  = 300, type = "cairo")
