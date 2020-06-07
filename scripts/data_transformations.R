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
post_2004 <- read.spss("~/Research/Pew Data/PEW YOUR VOTE + WAS/2004/Post-Election04c.sav", to.data.frame = T)
post_2006 <- read.spss("~/Research/Pew Data/PEW YOUR VOTE + WAS/2006/p2006postele.por", to.data.frame = T)
post_2008 <- read.spss("~/Research/Pew Data/PEW YOUR VOTE + WAS/2008/Nov08PostElectc.sav", to.data.frame = T)
post_2010 <- read.spss("~/Research/Pew Data/PEW YOUR VOTE + WAS/2010/Nov10 post-election public.sav", to.data.frame = T)
post_2012 <- read.spss("~/Research/Pew Data/PEW YOUR VOTE + WAS/2012/Nov12 Post-Elect public.sav", to.data.frame = T)
post_2014 <- read.spss("~/Research/Pew Data/PEW YOUR VOTE + WAS/2014/Nov14 public.sav", to.data.frame = T)
post_2016 <- read.spss("~/Research/Pew Data/PEW YOUR VOTE + WAS/2016/Nov16 Post-Election public.sav", to.data.frame = T)

#from each post-election dataset I need: date, respondent, age,
#race, education, party, state, vote confidence (national and personal) - recodes

d8 <- post_2004 %>% 
  select(
    respid, sage, state, racethn, ssex, int_date, party, partyln, seduc, q58f1, q59f2, weight
  ) %>% 
  mutate(
    id = str_c("2004post",respid),
    party = ifelse(party=="Independent",partyln %>% as.character,party %>% as.character),
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
    regi = T,
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
    id, age, state, racethn, sex, date, party, educ, p_post_conf, n_post_conf, regi, weight
  )

d9 <- post_2006 %>%
  set_names(
    post_2006 %>% names %>% str_to_lower
  ) %>% 
  select(
    psraid, age, state, racethn, sex, int_date, party, partyln, educ, q17, q18, weight
  ) %>% 
  mutate(
    id = str_c("2006post",psraid),
    party = ifelse(party=="Independent",partyln %>% as.character,party %>% as.character),
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
    regi = T,
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
    id, age, state, racethn, sex, date, party, educ, p_post_conf, n_post_conf, regi, weight
  )

d10 <- post_2008 %>% 
  select(
    psraid, age, state, racethn, sex, int_date, party, partyln, educ, q60f1, q61f2, weight
  ) %>% 
  mutate(
    id = str_c("2008post",psraid),
    party = ifelse(party=="Independent",partyln %>% as.character,party %>% as.character),
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
    regi = T,
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
    id, age, state, racethn, sex, date, party, educ, p_post_conf, n_post_conf, regi, weight
  )

d11 <- post_2010 %>% 
  select(
    psraid, age, state, racethn, sex, int_date, party, partyln, educ, q18, q19, weight
  ) %>% 
  mutate(
    id = str_c("2010post",psraid),
    party = ifelse(party=="Independent",partyln %>% as.character,party %>% as.character),
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
    regi = T,
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
    id, age, state, racethn, sex, date, party, educ, p_post_conf, n_post_conf, regi, weight
  )

d12 <- post_2012 %>% 
  select(
    psraid, ewage, state, ewracethn, ewsex, int_date, ewparty, ewpartyln, eweduc2, q76, q77, weight
  ) %>% 
  mutate(
    id = str_c("2012post",psraid,sep = ""),
    sex = ewsex,
    party = ifelse(ewparty=="Independent",ewpartyln %>% as.character,ewparty %>% as.character),
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
    regi = T,
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
    id, age, state, racethn, sex, date, party, educ, p_post_conf, n_post_conf, regi, weight
  )

d13 <- post_2014 %>% 
  select(
    psraid, age, state, racethn, sex, int_date, party, partyln, educ2, q36, q37, weight
  ) %>% 
  mutate(
    id = str_c("2014post",psraid,sep = ""),
    sex = sex,
    party = ifelse(party=="Independent",partyln %>% as.character,party %>% as.character),
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
    regi = T,
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
    id, age, state, racethn, sex, date, party, educ, p_post_conf, n_post_conf, regi, weight
  )

d14 <- post_2016 %>% 
  select(
    psraid, origage, state, origracethn, origsex, int_date, party, partyln, origeduc2, q65, q66, weight
  ) %>% 
  mutate(
    id = str_c("2016post",psraid,sep = ""),
    sex = origsex,
    party = ifelse(party=="Independent",partyln %>% as.character,party %>% as.character),
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
    regi = T,
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
    id,age, state, racethn, sex, date, party, educ, p_post_conf, n_post_conf, regi, weight
  )

fullcdf <- d8 %>% 
  full_join(d9) %>% 
  full_join(d10) %>% 
  full_join(d11) %>% 
  full_join(d12) %>% 
  full_join(d13) %>% 
  full_join(d14) %>% 
  mutate(
    state = state %>% as.factor,
    p_post_conf = p_post_conf %>% as.character %>% as.numeric,
    n_post_conf = n_post_conf %>% as.character %>% as.numeric,
    party = ifelse(party %>% str_detect(pattern = "Republican|Democrat"),party %>% as.character,NA )
  ) %>% 
  select(-weight)

rm(list = c("post_2004","post_2008","post_2016","pre_2004",
            "pre_2008","pre_2016","post_2006","post_2010",
            "post_2012","pree_2006","pret_2006","post_2014",
            "d1","d2","d3","d4","d5","d6","d7","d8","d9","d10",
            "d11","d12","d13","d14"))



fullcdf <- fullcdf %>%
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

dt1 <- read.csv("~/Research/debate_turn_vid.csv")

dt1 <- dt1 %>% 
  select(
    State.Name, State.Abbreviation, `FIPS.Code`, `Type.of.Voter.ID`, `Year.In.Effect`
  ) %>% 
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

dt2 <- dt1 %>% 
  mutate(
    `FIPS.Code`= `FIPS.Code` %>% 
      plyr::mapvalues(
        c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 
          "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", 
          "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", 
          "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", 
          "46", "47", "48", "49", "50", "51"),
        c("1", "2", "4", "5", "6", "8", "9", "10", "11", "12", "13", 
          "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", 
          "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", 
          "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", 
          "49", "50", "51", "53", "54", "55", "56")
      )
  ) %>% 
  set_names(
    c("state", "State Abbreviation", "FIPS Code", "Type.of.Voter.ID", 
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
    state = state %>% as.factor
  )

dfullcdf <- fullcdf %>% 
  left_join(dt2) %>%
  filter(
    year %>% is.na %>% 
      not&
      state %>% is.na %>% 
      not
  ) %>% 
  mutate(
    yfac = year %>% factor
  )


descdf <- dfullcdf %>% 
  select(year,yfac,age,sex,state,racethn,educ,party,win,winner,law,
         p_post_conf, n_post_conf) %>% 
  left_join(dt1 %>% select(`State.Name`, `Year.In.Effect`, `Type.of.Voter.ID`)%>%
              set_names(c("state", "yie", "eventlaw"))
  ) %>% 
  mutate(
    state = state,
    y_since = ifelse(yie %>% is.na %>% not,year-yie, NA),
    e_since = y_since/2 %>% as.numeric(),
    e_since = floor(e_since)
  )

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

rescdf <- descdf %>%
  mutate(
    race = ifelse(racethn=="White non-Hispanic","White","Non-White") %>% factor %>% relevel("White")
  )

#at this point, rescdf is the only relevant data.frame for all further analyses
#so let's remove all the old ones
rm(list = c("dfullcdf","dstatecheck","dt1","dt2","fullcdf","postcdf","precdf"))

saveRDS(rescdf,"~/Research/rescdf")
