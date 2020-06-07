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

# post-election survey datasets
post_2004 <- "https://github.com/bjjohnson998/vid_conf/raw/master/data/raw_data/post_04.sav" %>% 
  read.spss(to.data.frame = T)
post_2006 <- "https://github.com/bjjohnson998/vid_conf/raw/master/data/raw_data/post_06.por" %>% 
  read.spss(to.data.frame = T)
post_2008 <- "https://github.com/bjjohnson998/vid_conf/raw/master/data/raw_data/post_08.sav" %>% 
  read.spss(to.data.frame = T)
post_2010 <- "https://github.com/bjjohnson998/vid_conf/raw/master/data/raw_data/post_10.sav" %>% 
  read.spss(to.data.frame = T)
post_2012 <- "https://github.com/bjjohnson998/vid_conf/raw/master/data/raw_data/post_12.sav" %>% 
  read.spss(to.data.frame = T)
post_2014 <- "https://github.com/bjjohnson998/vid_conf/raw/master/data/raw_data/post_14.sav" %>% 
  read.spss(to.data.frame = T)
post_2016 <- "https://github.com/bjjohnson998/vid_conf/raw/master/data/raw_data/post_16.sav" %>% 
  read.spss(to.data.frame = T)

#from each post-election dataset I need: date, respondent, age,
#race, education, party, state, vote confidence (national and personal) - recodes

coded_04 <- post_2004 %>% 
  select(
    respid, sage, state, racethn, ssex, int_date, party, partyln, seduc, q58f1, q59f2
  ) %>% 
  mutate(
    id = str_c("2004post",respid),
    party = ifelse(party%in% c("Democrat", "Republican") %>% not,partyln %>% as.character,party %>% as.character),
    date = int_date %>% mdy,
    p_post_conf = q58f1 %>%
      plyr::mapvalues(
        c("Very confident", "Somewhat confident", "Not too confident", 
          "Not at all confident", "(VOL. DO NOT READ) Don't know/Refused"),
        c(1,2/3,1/3,0,NA)
      ) %>% as.character %>% as.numeric,
    n_post_conf = q59f2 %>% 
      plyr::mapvalues(
        c("Very confident", "Somewhat confident", "Not too confident", 
          "Not at all confident", "(VOL. DO NOT READ) Don't know/Refused"),
        c(1,2/3,1/3,0,NA)
      ) %>% as.character %>% as.numeric,
    age = sage %>% as.character %>% as.numeric,
    sex = ssex,
    educ = seduc %>% 
      plyr::mapvalues(
        c("None, or grade 1-8", "High school incomplete (Grades 9-11)", 
          "High school graduate (Grade 12 or GED certificate)", "Technical, trade, or vocational school AFTER high school", 
          "Some college, no 4-year degree (including associate degree)", 
          "College graduate (B.S., B.A., or other 4-year degree)", "Post-graduate training or professional schooling after colle", 
          "Don't know/Refused (VOL.)"),
        c("Less than HS", "HS Incomplete", "HS Grad or Bus/Voc/Tech", "HS Grad or Bus/Voc/Tech",
          "Some College or Assoc. Degree", "College Grad", "Graduate School", NA
        )
      ),
    racethn = racethn %>% 
      plyr::mapvalues(
        c("White~H", "Black~H", "Hispanic", "Other"),
        c("White non-Hispanic", "Black non-Hispanic", "Hispanic", "Other")
      )
  ) %>% 
  select(
    id, age, state, racethn, sex, date, party, educ, p_post_conf, n_post_conf
  )

coded_06 <- post_2006 %>%
  set_names(
    post_2006 %>% names %>% str_to_lower
  ) %>% 
  select(
    psraid, age, state, racethn, sex, int_date, party, partyln, educ, q17, q18
  ) %>% 
  mutate(
    id = str_c("2006post",psraid),
    party = ifelse(party%in% c("Democrat", "Republican") %>% not,partyln %>% as.character,party %>% as.character),
    date = int_date %>% mdy,
    p_post_conf = q17 %>%
      plyr::mapvalues(
        c("Very confident", "Somewhat confident", "Not too confident", 
          "Not at all confident", "(VOL. DO NOT READ) Don't know/Refused"),
        c(1,2/3,1/3,0,NA)
      ) %>% as.character %>% as.numeric,
    n_post_conf = q18 %>% 
      plyr::mapvalues(
        c("Very confident", "Somewhat confident", "Not too confident", 
          "Not at all confident", "(VOL. DO NOT READ) Don't know/Refused"),
        c(1,2/3,1/3,0,NA)
      ) %>% as.character %>% as.numeric,
    age = age %>% as.character %>% as.numeric,
    educ = educ %>% 
      plyr::mapvalues(
        c("None, or grade 1-8", "High school incomplete (Grades 9-11)", 
          "High school graduate (Grade 12 or GED certificate)", "Technical, trade, or vocational school AFTER high school", 
          "Some college, no 4-year degree (including associate degree)", 
          "College graduate (B.S., B.A., or other 4-year degree)", "Post-graduate training or professional schooling after colle", 
          "Don't know/Refused (VOL.)"),
        c("Less than HS", "HS Incomplete", "HS Grad or Bus/Voc/Tech", "HS Grad or Bus/Voc/Tech",
          "Some College or Assoc. Degree", "College Grad", "Graduate School", NA)
      ),
    racethn = racethn %>% 
      plyr::mapvalues(
        c("White~H", "Black~H", "Hispanic", "Other"),
        c("White non-Hispanic", "Black non-Hispanic", "Hispanic", "Other")
      )
  ) %>% 
  select(
    id, age, state, racethn, sex, date, party, educ, p_post_conf, n_post_conf
  )

coded_08 <- post_2008 %>% 
  select(
    psraid, age, state, racethn, sex, int_date, party, partyln, educ, q60f1, q61f2
  ) %>% 
  mutate(
    id = str_c("2008post",psraid),
    party = ifelse(party%in% c("Democrat", "Republican") %>% not,partyln %>% as.character,party %>% as.character),
    date = int_date %>% mdy,
    p_post_conf = q60f1 %>%
      plyr::mapvalues(
        c("Very confident", "Somewhat confident", "Not too confident", 
          "Not at all confident", "Don't know/Refused (VOL)"),
        c(1,2/3,1/3,0,NA)
      ) %>% as.character %>% as.numeric,
    n_post_conf = q61f2 %>% 
      plyr::mapvalues(
        c("Very confident", "Somewhat confident", "Not too confident", 
          "Not at all confident", "Don't know/Refused (VOL)"),
        c(1,2/3,1/3,0,NA)
      ) %>% as.character %>% as.numeric,
    age = age %>% as.character %>% as.numeric,
    educ = educ %>% 
      plyr::mapvalues(
        c("None, or grade 1-8", "High school incomplete (Grades 9-11)", 
          "High school graduate (Grade 12 or GED certificate)", "Technical, trade, or vocational school AFTER high school", 
          "Some college, associate degree, no 4-year degree", "College graduate (B.S., B.A., or other 4-year degree)", 
          "Post-graduate training or professional schooling after college", 
          "Don't know/Refused (VOL.)"),
        c("Less than HS", "HS Incomplete", "HS Grad or Bus/Voc/Tech", "HS Grad or Bus/Voc/Tech",
          "Some College or Assoc. Degree", "College Grad", "Graduate School", NA
        )
      ),
    racethn = racethn %>% 
      plyr::mapvalues(
        c("White~Hisp", "AA~Hisp", "Hisp", "Other~Hisp"),
        c("White non-Hispanic", "Black non-Hispanic", "Hispanic", "Other")
      )
  ) %>% 
  select(
    id, age, state, racethn, sex, date, party, educ, p_post_conf, n_post_conf
  )

coded_10 <- post_2010 %>% 
  select(
    psraid, age, state, racethn, sex, int_date, party, partyln, educ, q18, q19
  ) %>% 
  mutate(
    id = str_c("2010post",psraid),
    party = ifelse(party%in% c("Democrat", "Republican") %>% not,partyln %>% as.character,party %>% as.character),
    date = int_date %>% mdy,
    p_post_conf = q18 %>%
      plyr::mapvalues(
        c("Very confident", "Somewhat confident", "Not too confident", 
          "Not at all confident", "[VOL. DO NOT READ] Don't know/Refused"),
        c(1,2/3,1/3,0,NA)
      ) %>% as.character %>% as.numeric,
    n_post_conf = q19 %>% 
      plyr::mapvalues(
        c("Very confident", "Somewhat confident", "Not too confident", 
          "Not at all confident", "[VOL. DO NOT READ] Don(t know/Refused"),
        c(1,2/3,1/3,0,NA)
      ) %>% as.character %>% as.numeric,
    age = age %>% as.character %>% as.numeric,
    educ = educ %>% 
      plyr::mapvalues(
        c("None, or grade 1-8", "High school incomplete (Grades 9-11)", 
          "High school graduate (Grade 12 or GED certificate)", "Technical, trade, or vocational school AFTER high school", 
          "Some college, associate degree, no 4-year degree", "College graduate (B.S., B.A., or other 4-year degree)", 
          "Post-graduate training or professional schooling after college", 
          "Don't know/Refused (VOL.)"),
        c("Less than HS", "HS Incomplete", "HS Grad or Bus/Voc/Tech", "HS Grad or Bus/Voc/Tech",
          "Some College or Assoc. Degree", "College Grad", "Graduate School", NA
        )
      ),
    racethn = racethn %>% 
      plyr::mapvalues(
        c("White, non-Hisp", "Black, non-Hisp", "Hispanic", "Other"),
        c("White non-Hispanic", "Black non-Hispanic", "Hispanic", "Other")
      )
  ) %>% 
  select(
    id, age, state, racethn, sex, date, party, educ, p_post_conf, n_post_conf
  )

coded_12 <- post_2012 %>% 
  select(
    psraid, ewage, state, ewracethn, ewsex, int_date, ewparty, ewpartyln, eweduc2, q76, q77
  ) %>% 
  mutate(
    id = str_c("2012post",psraid,sep = ""),
    sex = ewsex,
    party = ifelse(ewparty%in% c("Democrat", "Republican") %>% not,ewpartyln %>% as.character,ewparty %>% as.character),
    date = int_date %>% mdy,
    p_post_conf = q76 %>%
      plyr::mapvalues(
        c("Very confident", "Somewhat confident", "Not too confident [OR]", 
          "Not at all confident", "[VOL. DO NOT READ] Don't know/Refused"),
        c(1,2/3,1/3,0,NA)
      ) %>% as.character %>% as.numeric,
    n_post_conf = q77 %>% 
      plyr::mapvalues(
        c("Very confident", "Somewhat confident", "Not too confident [OR]", 
          "Not at all confident", "[VOL. DO NOT READ] Don't know/Refused"),
        c(1,2/3,1/3,0,NA)
      ) %>% as.character %>% as.numeric,
    age = ewage %>% as.character %>% as.numeric,
    educ = eweduc2 %>% 
      plyr::mapvalues(
        c("Less than high school (Grades 1-8 or no formal schooling)", 
          "High school incomplete (Grades 9-11 or Grade 12 with NO diploma)", 
          "High school graduate (Grade 12 with diploma or GED certificate)", 
          "Some college, no degree includes community college", "Two year associate degree from any college or university", 
          "Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)", 
          "Some postgraduate or professional schooling, no postgraduate degree (some graduate school)", 
          "Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD, JD, gr", 
          "(VOL.) Don't know/Refused"),
        c("Less than HS", "HS Incomplete", "HS Grad or Bus/Voc/Tech", "Some College or Assoc. Degree",
          "Some College or Assoc. Degree", "College Grad", "Graduate School","Graduate School", NA)
      ),
    racethn = ewracethn %>% 
      plyr::mapvalues(
        c("White, non-Hisp", "Black, non-Hisp", "Hispanic", "Other"),
        c("White non-Hispanic", "Black non-Hispanic", "Hispanic", "Other")
      )
  ) %>% 
  select(
    id, age, state, racethn, sex, date, party, educ, p_post_conf, n_post_conf
  )

coded_14 <- post_2014 %>% 
  select(
    psraid, age, state, racethn, sex, int_date, party, partyln, educ2, q36, q37
  ) %>% 
  mutate(
    id = str_c("2014post",psraid,sep = ""),
    sex = sex,
    party = ifelse(party%in% c("Democrat", "Republican") %>% not,partyln %>% as.character,party %>% as.character),
    date = int_date %>% mdy,
    p_post_conf = q36 %>%
      plyr::mapvalues(
        c("Very confident", "Somewhat confident", "Not too confident [OR]", 
          "Not at all confident", "Don't know/Refused (VOL.)"),
        c(1,2/3,1/3,0,NA)
      ) %>% as.character %>% as.numeric,
    n_post_conf = q37 %>% 
      plyr::mapvalues(
        c("Very confident", "Somewhat confident", "Not too confident [OR]", 
          "Not at all confident", "Don't know/Refused (VOL.)"),
        c(1,2/3,1/3,0,NA)
      ) %>% as.character %>% as.numeric,
    age = age %>% as.character %>% as.numeric,
    educ = educ2 %>% 
      plyr::mapvalues(
        c("Less than high school (Grades 1-8 or no formal schooling)", 
          "High school incomplete (Grades 9-11 or Grade 12 with NO diploma)", 
          "High school graduate (Grade 12 with diploma or GED certificate)", 
          "Some college, no degree (includes some community college)", 
          "Two year associate degree from a college or university", "Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)", 
          "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)", 
          "Postgraduate or professional degree, including master's, doctorate, medical or law degree", 
          "Don't know/Refused"),
        c("Less than HS", "HS Incomplete", "HS Grad or Bus/Voc/Tech", "Some College or Assoc. Degree",
          "Some College or Assoc. Degree", "College Grad", "Graduate School","Graduate School", NA)
      ),
    racethn = racethn %>% 
      plyr::mapvalues(
        c("White, non-Hisp", "Black, non-Hisp", "Hispanic", "Other"),
        c("White non-Hispanic", "Black non-Hispanic", "Hispanic", "Other")
      )
  ) %>% 
  select(
    id, age, state, racethn, sex, date, party, educ, p_post_conf, n_post_conf
  )

coded_16 <- post_2016 %>% 
  select(
    psraid, origage, state, origracethn, origsex, int_date, party, partyln, origeduc2, q65, q66
  ) %>% 
  mutate(
    id = str_c("2016post",psraid,sep = ""),
    sex = origsex,
    party = ifelse(party%in% c("Democrat", "Republican") %>% not,partyln %>% as.character,party %>% as.character),
    date = int_date %>% mdy,
    p_post_conf = q65 %>%
      plyr::mapvalues(
        c("Very confident", "Somewhat confident", "Not too confident [OR]", 
          "Not at all confident", "[VOL. DO NOT READ] Don't know/Refused"),
        c(1,2/3,1/3,0,NA)
      ) %>% as.character %>% as.numeric,
    n_post_conf = q66 %>% 
      plyr::mapvalues(
        c("Very confident", "Somewhat confident", "Not too confident [OR]", 
          "Not at all confident", "[VOL. DO NOT READ] Don't know/Refused"),
        c(1,2/3,1/3,0,NA)
      ) %>% as.character %>% as.numeric,
    age = origage %>% as.character %>% as.numeric,
    educ = origeduc2 %>% 
      plyr::mapvalues(
        c("Less than high school (Grades 1-8 or no formal schooling) ", 
          "High school incomplete (Grades 9-11 or Grade 12 with NO diploma)", 
          "High school graduate (Grade 12 with diploma or GED certificate)", 
          "Some college, no degree (includes some community college)", 
          "Two year associate degree from a college or university", "Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)", 
          "Some postgraduate or professional schooling, no postgraduate degree", 
          "Postgraduate or professional degree, including master's, doctorate, medical or law degree", 
          "Don't know/Refused (VOL.)"),
        c("Less than HS", "HS Incomplete", "HS Grad or Bus/Voc/Tech", "Some College or Assoc. Degree",
          "Some College or Assoc. Degree", "College Grad", "Graduate School","Graduate School", NA)
      ),
    racethn = origracethn %>% 
      plyr::mapvalues(
        c("White, non-Hisp", "Black, non-Hisp", "Hispanic", "Other"),
        c("White non-Hispanic", "Black non-Hispanic", "Hispanic", "Other")
      )
  ) %>% 
  select(
    id,age, state, racethn, sex, date, party, educ, p_post_conf, n_post_conf
  )


#here is our merge of everything, recoded, into a single data.frame
fullcdf <- coded_04 %>% 
  full_join(coded_06) %>% 
  full_join(coded_08) %>% 
  full_join(coded_10) %>% 
  full_join(coded_12) %>% 
  full_join(coded_14) %>% 
  full_join(coded_16) %>% 
  mutate(
    state = state %>% as.factor,
    p_post_conf = p_post_conf %>% as.character %>% as.numeric,
    n_post_conf = n_post_conf %>% as.character %>% as.numeric,
    party = ifelse(party %>% str_detect(pattern = "Republican|Democrat"),party %>% as.character,NA )
  )%>%
  mutate(
    year = date %>% year()
  ) %>% 
  filter(
    year %>% is.na %>% 
      not
  ) %>% 
  mutate(
    winner = year %>% as.factor %>% 
      plyr::mapvalues(
        c("2004", "2006", "2008", "2010", "2012", "2014", "2016"),
        c("Republican", "Democrat","Democrat","Republican","Democrat","Republican","Republican")
      ) %>% as.character,
    win = ifelse(party==winner,1,0)
  )

#here we create a table with state VID laws in effect, which becomes our variables of interest
laws <- "https://github.com/bjjohnson998/vid_conf/raw/master/data/raw_data/vid_laws.csv" %>% read.csv%>%
  select(-X) %>% 
  filter(
    `State.Abbreviation` != "USA"
  ) %>%
  mutate(
    law_04 = ifelse(`Year.In.Effect` %>% is.na,"None",ifelse(`Year.In.Effect`<=2004, `Type.of.Voter.ID` %>% as.character, "None")),
    law_06 = ifelse(`Year.In.Effect` %>% is.na,"None",ifelse(`Year.In.Effect`<=2006, `Type.of.Voter.ID` %>% as.character, "None")),
    law_08 = ifelse(`Year.In.Effect` %>% is.na,"None",ifelse(`Year.In.Effect`<=2008, `Type.of.Voter.ID` %>% as.character(), "None")),
    law_10 = ifelse(`Year.In.Effect` %>% is.na,"None",ifelse(`Year.In.Effect`<=2010, `Type.of.Voter.ID` %>% as.character(), "None")),
    law_12 = ifelse(`Year.In.Effect` %>% is.na,"None",ifelse(`Year.In.Effect`<=2012, `Type.of.Voter.ID` %>% as.character(), "None")),
    law_14 = ifelse(`Year.In.Effect` %>% is.na,"None",ifelse(`Year.In.Effect`<=2014, `Type.of.Voter.ID` %>% as.character(), "None")),
    law_16 = ifelse(`Year.In.Effect` %>% is.na,"None",ifelse(`Year.In.Effect`<=2016, `Type.of.Voter.ID` %>% as.character(), "None"))
  )

laws2 <- laws %>% 
  set_names(
    c("fips", "state", "State Abbreviation", "Type.of.Voter.ID", 
      "Year.In.Effect", "2004", "2006", "2008", "2010", "2012", 
      "2014", "2016")
  ) %>% 
  select(
    state, `Type.of.Voter.ID`, `Year.In.Effect`, matches("\\d{4}")
  ) %>% 
  gather(
    year, law, -c(state, `Type.of.Voter.ID`, `Year.In.Effect`)
  ) %>%
  select(
    state, year, law
  ) %>% 
  mutate(
    year = year %>% as.numeric,
    state = state %>% as.character() %>%  factor
  )

#here we join our laws in and create the yfac vector for year fixed effects
#as well as removing some NAs in year and state
fullcdf <- fullcdf %>% 
  left_join(laws2) %>%
  filter(
    year %>% is.na %>% 
      not&
      state %>% is.na %>% 
      not
  ) %>% 
  mutate(
    yfac = year %>% factor
  )

#here we begin to create the event study variable vector
#begin by counting elections from the year the law took effect
descdf <- fullcdf %>% 
  select(year,yfac,age,sex,state,racethn,educ,party,win,winner,law,
         p_post_conf, n_post_conf) %>% 
  left_join(laws %>% select(`State.Name`, `Year.In.Effect`, `Type.of.Voter.ID`)%>%
              set_names(c("state", "yie", "eventlaw"))
  ) %>% 
  mutate(
    state = state,
    y_since = ifelse(yie %>% is.na %>% not,year-yie, NA),
    e_since = y_since/2 %>% as.numeric(),
    e_since = floor(e_since)
  )

# this data.frame here allows me to check for state/year combinations which are pre-treatment periods
dstatecheck <- descdf %>% 
  filter(
    yie %>% is.na %>% not
  ) %>% 
  select(state,e_since,) %>% 
  group_by(state) %>% 
  summarize(
    most_before = -min(e_since)
  ) %>% 
  filter(
    most_before >1
  )

#joining dstatecheck to create the final version of the event study variable, with the timer
descdf <- descdf %>% 
  full_join(dstatecheck) %>% 
  mutate(
    law = law %>% factor %>% plyr::mapvalues(
      c("Debate", "Non-Strict Non-Photo ID", "Non-Strict Photo ID", 
        "None", "Strict Non-Photo ID", "Strict Photo ID"),
      c("Debate", "Non-Crawford Law", "None", "Non-Crawford Law", "Strict Photo ID") %>% rep(c(1,2,1,1,1))
    ) %>% relevel("None"),
    e_since = ifelse(most_before %>% is.na,
                     ifelse(e_since<0,NA,e_since),
                     ifelse(e_since<(-2),
                            NA,e_since)),
    e_since = ifelse(e_since>=3,3,e_since),
    deslaw_ = eventlaw %>% 
      plyr::mapvalues(
        c("Debate", "Non-Strict Non-Photo ID", "Non-Strict Photo ID", 
          "None", "Strict Non-Photo ID", "Strict Photo ID"),
        c("deb","nonc","nonc","none","nonc","sp")
      ) %>% fct_relevel(c("none")),
    deslaw_ = ifelse(deslaw_=="none",deslaw_ %>% as.character,
                     ifelse(e_since %>% is.na %>% not,str_c(deslaw_,e_since, sep = "_"),"none")) %>% 
      as.factor() %>% reorder(e_since) %>% relevel("none")
  )

#rescdf is the final dataset, with race recoded to be binary
rescdf <- descdf %>%
  mutate(
    race = ifelse(racethn=="White non-Hispanic","White","Non-White") %>% factor %>% relevel("White")
  )



#this RDS file is up on the repository under data/rescdf.RDS
saveRDS(rescdf,"~/Research/rescdf")
#don't mind the directory it's going to, but now the research dataset is completely created
#all analyses contained in paper_script.R use this data.frame