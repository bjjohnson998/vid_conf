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

rescdf <- readRDS("~/Research/rescdf")


#the general specification DID model and output table
#post-election confidence by law dummy variables with state and year fixed effects
#first, though, function to quickly cluster standard errors
#default is state, but this is lightweight enough you could slap any clust_var in

cluster <- function(mod,tag = "group",clust_var = "state"){
  # testing using named inputs
  # mod <- m_p
  # tag <- "group"
  # clust_var <- "state"
  #compute covariance matrix
  vcov <-  vcovHC(mod, type = "HC0", cluster = tag, adjust = T)
  # return coeftest object as regression output with clustered standard errors by state
  return(coeftest(mod, vcov. = vcov))}


#did models, clustered standard errors by state
did_model_perspost <- plm(data = rescdf,
                          model = "within",
                          index = "state",
                          effect = "individual",
                          p_post_conf ~ law*race + law*party + age + sex + educ+ win + yfac)
clust_did_perspost <- cluster(did_model_perspost)

did_model_natpost <- plm(data = rescdf,
                         model = "within",
                         index = "state",
                         effect = "individual",
                         n_post_conf ~ law*race + law*party + age + sex + educ+ win + yfac)
clust_did_natpost <- cluster(did_model_natpost)


#this table needs significant alterations in TeX to be accurate
#the terms are also rearranged in TeX to get them in a more presentable order (but the names here should be 100% correct)
#specifically, interaction variables were moved under the relevant ID variable (to present heterogenity)
stargazer(header = F, title = "VID Difference-in-Differences Results", label = "tab:1",
          clust_did_perspost,clust_did_natpost, dep.var.caption = "\\textit{Dependent Variable}: Public Confidence",
          column.separate = c(1,1), font.size = "small", star.cutoffs = c(0.05,0.01,0.001), no.space = T,
          keep = c("law"),
          covariate.labels = c("Failed VID Law", "Non-Crawford ID Law", "Strict Photo ID",
                               "Failed Law $\\times$ Non-White", "Non-Crawford $\\times$ Non-White",
                               "Strict Photo $\\times$ Non-White","Failed Law $\\times$ Republican",
                               "Non-Crawford $\\times$ Republican","Strict Photo $\\times$ Republican"),
          add.lines = list(c("Controls","$\\times$","$\\times$"),
                           c("State FEs","$\\times$","$\\times$"),
                           c("Year FEs", "$\\times$", "$\\times$")))


#no interaction specification event studies and code to produce the table that feeds figure 2
es_model_perspost <-  plm(data = rescdf,
                          model = "within",
                          effect = "individual",
                          index = "state",
                          p_post_conf ~ deslaw_ + age + sex + race + educ + party + win + yfac)

t_es_ppost <- cluster(es_model_perspost)%>%
  broom::tidy(conf.int = T) %>% 
  filter(
    term %>% str_detect("deslaw_")
  ) %>% 
  mutate(
    conf.low = estimate-2*std.error,
    conf.high = estimate+2*std.error,
    term = term %>% str_remove("deslaw_"),
    term = term %>% str_remove("\\:(.*)"),
    terms = term %>% str_split("_"),
    law = terms %>% sapply("[[",1),
    time = terms %>% sapply("[[",2),
    dv = "Personal Vote Confidence"
  ) %>% 
  select(
    law, time, dv, estimate, std.error, conf.low, conf.high
  )



es_model_natpost <-  plm(data = rescdf,
                         model = "within",
                         effect = "individual",
                         index = "state",
                         n_post_conf ~ deslaw_ + age + sex + race + educ + party + win + yfac)
cluster(es_model_natpost)

t_es_npost <- cluster(es_model_natpost)%>%
  broom::tidy(conf.int = T) %>% 
  filter(
    term %>% str_detect("deslaw_")
  ) %>% 
  mutate(
    conf.low = estimate-2*std.error,
    conf.high = estimate+2*std.error,
    term = term %>% str_remove("deslaw_"),
    term = term %>% str_remove("\\:(.*)"),
    terms = term %>% str_split("_"),
    law = terms %>% sapply("[[",1),
    time = terms %>% sapply("[[",2),
    dv = "National Vote Confidence"
  ) %>% 
  select(
    law, time, dv, estimate, std.error, conf.low, conf.high
  )

tot_es <- t_es_ppost %>% full_join(t_es_npost) %>% 
  mutate(
    law = law %>% factor %>% plyr::mapvalues(
      c("deb", "nonc", "sp"),
      c("Failed Law", "Non-Crawford\nID Law", "Strict Photo")
    ),
    time = time %>% as.character() %>% as.numeric(),
    p = abs(estimate/std.error) %>% pnorm(lower.tail = F)*2,
    stars = ifelse(p>.05,"",
                   ifelse(p>.01,"*",
                          ifelse(p>.001,"**","***"))),
    y.coord = ifelse(estimate>0,conf.high+.02,conf.low-.03)
  )


#race interaction models, labelled by DV
rdes_model_ppost <- plm(data = rescdf,
                        index = "state",
                        model = "within",
                        effect = "individual",
                        p_post_conf ~ deslaw_ * race + age + sex + educ + party + win + yfac)

rdes_model_npost <- plm(data = rescdf,
                        index = "state",
                        model = "within",
                        effect = "individual",
                        n_post_conf ~ deslaw_ * race + age + sex + educ + party + win + yfac)

#partisan interaction models, labelled by DV
pes_model_ppost <- plm(data = rescdf,
                       index = "state",
                       model = "within",
                       effect = "individual",
                       p_post_conf ~ deslaw_ * party + age + sex + educ + race + win + yfac)

pes_model_npost <- plm(data = rescdf,
                       index = "state",
                       model = "within",
                       effect = "individual",
                       n_post_conf ~ deslaw_ * party + age + sex + educ + race + win + yfac)


#you need this excellent function, which I found on stackexchange, to do the overlapping facet labels
#important for making the figures
OverlappingStripLabels = function(plot) {
  
  # Get the ggplot grob
  pg = ggplotGrob(plot)
  
  ### Collect some information about the strips from the plot
  # Get a list of strips
  stripr = lapply(grep("strip-r", pg$layout$name), function(x) {pg$grobs[[x]]})
  
  stript = lapply(grep("strip-t", pg$layout$name), function(x) {pg$grobs[[x]]})
  
  # Number of strips
  NumberOfStripsr = sum(grepl(pattern = "strip-r", pg$layout$name))
  NumberOfStripst = sum(grepl(pattern = "strip-t", pg$layout$name))
  
  # Number of columns
  NumberOfCols = length(stripr[[1]])
  NumberOfRows = length(stript[[1]])
  
  # Panel spacing
  plot_theme <- function(p) {
    plyr::defaults(p$theme, theme_get())
  }
  PanelSpacing = plot_theme(plot)$panel.spacing
  
  # Map the boundaries of the new strips
  Nlabelr = vector("list", NumberOfCols)
  mapr = vector("list", NumberOfCols)
  for(i in 1:NumberOfCols) {
    
    for(j in 1:NumberOfStripsr) {
      Nlabelr[[i]][j] = getGrob(grid.force(stripr[[j]]$grobs[[i]]), gPath("GRID.text"), grep = TRUE)$label
    }
    
    mapr[[i]][1] = TRUE
    for(j in 2:NumberOfStripsr) {
      mapr[[i]][j] = as.character(Nlabelr[[i]][j]) != as.character(Nlabelr[[i]][j-1])#Nlabelr[[i]][j] != Nlabelr[[i]][j-1]
    }
  }
  
  # Map the boundaries of the new strips
  Nlabelt = vector("list", NumberOfRows)
  mapt = vector("list", NumberOfRows)
  for(i in 1:NumberOfRows) {
    
    for(j in 1:NumberOfStripst) {
      Nlabelt[[i]][j] = getGrob(grid.force(stript[[j]]$grobs[[i]]), gPath("GRID.text"), grep = TRUE)$label
    }
    
    mapt[[i]][1] = TRUE
    for(j in 2:NumberOfStripst) {
      mapt[[i]][j] = as.character(Nlabelt[[i]][j]) != as.character(Nlabelt[[i]][j-1])#Nlabelt[[i]][j] != Nlabelt[[i]][j-1]
    }
  }
  
  
  ## Construct gtable to contain the new strip
  newStripr  = gtable(heights = unit.c(rep(unit.c(unit(1, "null"), PanelSpacing), NumberOfStripsr-1), unit(1, "null")), 
                      widths = stripr[[1]]$widths)
  ## Populate the gtable  
  seqTop = list()
  for(i in NumberOfCols:1) {  
    Top = which(mapr[[i]] == TRUE)
    seqTop[[i]] = if(i == NumberOfCols) 2*Top - 1 else  sort(unique(c(seqTop[[i+1]], 2*Top - 1)))  
    seqBottom = c(seqTop[[i]][-1] -2, (2*NumberOfStripsr-1))
    newStripr = gtable_add_grob(newStripr, lapply(stripr[(seqTop[[i]]+1)/2], function(x) x[[1]][[i]]), l = i, t = seqTop[[i]], b = seqBottom)
  }
  
  mapt <- mapt[NumberOfRows:1]
  Nlabelt <- Nlabelt[NumberOfRows:1]
  ## Do the same for top facets
  newStript  = gtable(heights = stript[[1]]$heights,
                      widths = unit.c(rep(unit.c(unit(1, "null"), PanelSpacing), NumberOfStripst-1), unit(1, "null")))
  seqTop = list()
  for(i in NumberOfRows:1) {  
    Top = which(mapt[[i]] == TRUE)
    seqTop[[i]] = if(i == NumberOfRows) 2*Top - 1 else  sort(unique(c(seqTop[[i+1]], 2*Top - 1)))  
    seqBottom = c(seqTop[[i]][-1] -2, (2*NumberOfStripst-1))
    # newStript = gtable_add_grob(newStript, lapply(stript[(seqTop[[i]]+1)/2], function(x) x[[1]][[i]]), l = i, t = seqTop[[i]], b = seqBottom)
    newStript = gtable_add_grob(newStript, lapply(stript[(seqTop[[i]]+1)/2], function(x) x[[1]][[(NumberOfRows:1)[i]]]), t = (NumberOfRows:1)[i], l = seqTop[[i]], r = seqBottom)
  }
  
  ## Put the strip into the plot
  # Get the locations of the original strips
  posr = subset(pg$layout, grepl("strip-r", pg$layout$name), t:r)
  post = subset(pg$layout, grepl("strip-t", pg$layout$name), t:r)
  
  ## Use these to position the new strip
  pgNew = gtable_add_grob(pg, newStripr, t = min(posr$t), l = unique(posr$l), b = max(posr$b))
  pgNew = gtable_add_grob(pgNew, newStript, l = min(post$l), r = max(post$r), t=unique(post$t))
  grid.draw(pgNew)
  
  return(pgNew)
}

#function automates the process for creating the interaction event study figures
intplotbw <- function(modp, modn, type = c("racethn","party", "race"), levels){
  # modp <- rdes_model_ppost
  # modn <- rdes_model_npost
  #type <-  "race"
  #levels <-  c("White", "Non-White")
  t_mp <- modp %>% cluster() %>% broom::tidy() %>% mutate(dv = "Personal Vote Confidence")
  t_mn <- modn %>% cluster() %>% broom::tidy() %>% mutate(dv = "National Vote Confidence")
  t_ms <- t_mp %>% full_join(t_mn) %>% 
    mutate(
      conf.low = estimate - 1.96*std.error,
      conf.high = estimate + 1.96*std.error
    ) %>% 
    filter(term %>% str_detect("deslaw_")) %>% 
    mutate(
      terms =  term %>% str_split(":") %>% as.list(),
      law = terms %>% sapply("[",1),
      int = ifelse(term %>% str_detect(":"), sapply(terms, "[", 2), levels[1]),
      law = law %>% str_remove("deslaw_"),
      int = int %>% str_remove(type) %>% str_split(" ") %>% sapply("[",1) %>% factor(levels = levels),
      time = law %>% str_split("_") %>% sapply("[",2) %>% as.character() %>% as.numeric(),
      law = law %>% str_split("_") %>% sapply("[",1) %>% factor %>% 
        plyr::mapvalues(
          c("deb", "nonc", "sp"),
          c("Failed Law", "Non-Crawford\nID Law", "Strict Photo")
        ),
      p = abs(estimate/std.error) %>% pnorm(lower.tail = F)*2,
      stars = ifelse(p>.05,"",
                     ifelse(p>.01,"*",
                            ifelse(p>.001,"**","***"))),
      y.coord = ifelse(estimate>0,conf.high+.03,conf.low-.09)
    )
  form <- ifelse(type=="racethn","int~dv+law","law~dv+int") %>% as.formula()
  p <- t_ms %>% 
    ggplot(aes(x = time, group = law))+
    geom_point(aes(y = estimate), size = .8)+
    geom_path(aes(y = estimate))+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high))+
    geom_hline(yintercept = 0, lty =2)+
    geom_text(aes(x = time, y = y.coord, label = stars), color = "black")+
    facet_grid(form)+
    theme_bw()+
    ylab("Event-Time Dummy Estimate and 95% CI")+
    labs(caption = "Stars indicate statistical significance of event study OLS coefficients,\n*p<.05 **p<.01 ***p<.001")+
    scale_x_continuous(name = "Elections Since Law Adopted", labels = c(-2:2,"3+"), breaks = c(-2:3))+
    theme(strip.text = element_text(size = 12), axis.title = element_text(size = 14), plot.caption = element_text(size = 12))
  
  plot <- OverlappingStripLabels(p)
  grid.newpage()
  grid.draw(plot)
}


#figure 2
tot_es %>% 
  ggplot(aes(x = time, group = law))+
  geom_point(aes(y = estimate))+
  geom_path(aes(y = estimate))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high))+
  geom_hline(yintercept = 0, lty = 2)+
  geom_text(aes(x = time, y = y.coord, label = stars), color = "black")+
  facet_grid(law~dv, scales = "free_y")+
  theme_bw()+
  ylab("Event-Time Dummy Estimate and 95% CI")+
  scale_x_continuous(name = "Elections Since Law Adopted", labels = c(-2:2,"3+"), breaks = c(-2:3))+
  labs(caption = "Stars indicate statistical significance of event study OLS coefficients,\n*p<.05 **p<.01 ***p<.001")+
  ggsave(filename = "~/Research/Writing/Publication Attempts/SPPQ/es_res_bw_cairo.png",
         type = "cairo", dpi = 300, width = 7, height = 5, units = "in")

#figure 3
png(filename = "~/Research/Writing/Publication Attempts/SPPQ/rdes_res_bw_cairo.png",
    width = 7, height = 7, res = 300, units = "in")
intplotbw(modp = rdes_model_ppost, modn = rdes_model_npost,
          type = "race", levels = c("White", "Non-White"))
dev.off()

#png code to save/export fig 4 as it is currently named
png(filename = "~/Research/Writing/Publication Attempts/SPPQ/pes_res_bw_cairo.png", type = "cairo",
    width = 7, height = 7, res = 300, units = "in")
intplotbw(modp = pes_model_ppost, modn = pes_model_npost,
          type = "party", levels = c("Democrat","Republican"))
dev.off()


