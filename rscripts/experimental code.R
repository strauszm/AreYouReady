#############################
# Michael Strausz
# Experimental code
# 5/16/2024 : 
#############################
install.packages("knitr")
install.packages("readr")
library(knitr)
library(readr)

flextable(anes2020 %>% 
  #first I filter out the NAs for all 3 variables
  filter(!is.na(ft_trump)&!is.na(partyid3)&!is.na(outrage2)) %>%
  #then I group by the control first, and then the IV
  group_by(partyid3, outrage2) %>% 
  #then I summarise in the same way that I do with a two-variable table
  summarise(mean=mean(ft_trump), sd=sd(ft_trump), n=n()))


flextable(anes2020 %>% 
  filter(!is.na(ft_trump)&!is.na(outrage2)) %>% 
  group_by(outrage2) %>% 
  summarise(mean=mean(ft_trump), sd=sd(ft_trump), n=n()))

flextable(anes2020 %>% 
  filter(!is.na(marital)) %>%  
  group_by(marital) %>%     
  summarise(group.n=n()) %>%
  mutate(total.n=sum(group.n)) %>% 
  mutate(proportion=group.n/total.n) %>% 
  rowwise() %>%
  mutate(lower_ci = prop.test(group.n, total.n, conf.level=0.95)$conf.int[1]) %>% 
  mutate(upper_ci = prop.test(group.n, total.n, conf.level=0.95)$conf.int[2])) 

table(anes2020$age_groups)
anes2020$age_groups<-ordered(anes2020$age_groups)

levels(anes2020$age_groups)
relevel
library(epiDisplay)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)
library(Hmisc)
library(tigerstats)
library(knitr)
library(readr)
library(pandoc)
library(officer)
library(flextable)
??as_flex_table
region.freq<-tab1(states2010$region, cum.percent=FALSE)
chazer<-colPerc(xtabs(~income+edu, data=anes2020))
flextable(as.data.frame(chazer)%>%rownames_to_column("income")) %>% 
  save_as_docx(path = "name.docx")
rm(chazer)

income.edu.table<-colPerc(xtabs(~income+edu, data=anes2020))
income.edu.table<-as.data.frame(income.edu.table)%>%rownames_to_column("income")

flextable(income.edu.table) %>% 
  save_as_docx(path="firstcrosstab.docx")

flextable(income.edu.table) %>% 
  save_as_(path="firstcrosstab.docx")
rm(income.edu.table)
regions.evangelical<-states2010 %>% 
  filter(!is.na(evangelical_pop)) %>% 
  group_by(region) %>% 
  summarise(mean=mean(evangelical_pop), sd=sd(evangelical_pop), n=n())

flextable(regions.evangelical)

?flextable
?as.data.frame
chazer <- as_flex_table(chazer)
#https://stackoverflow.com/questions/70139600/export-flextable-to-word-in-r

write_file(kable(chazer, row.names=NA, format="html"),"test.doc")
