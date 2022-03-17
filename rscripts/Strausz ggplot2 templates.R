####################################
# Michael Strausz
# Templates for ggplot2 graphs
# POSC 20093 Spring 2022
# 1/1/2022 : 2/17/2022
####################################

#Instructions for use---------------------------------------
#Copy a graph template and replace all words and phrases in all capital letters
#with the objects and titles relevant to the graph that you want to make

#Chapter 3: bar graph of a nominal or ordinal variable--------------------------
library(tidyverse)
ggplot(DATAFRAME, aes(x=VARIABLE))+
  geom_bar(fill = "purple", colour = "black")+
  ggtitle("GRAPH TITLE")+
  xlab("X-AXIS TITLE")

#Chapter 3: bar graph of a nominal or ordinal variable with NA bar removed------
library(tidyverse)
ggplot(DATAFRAME %>% filter(!is.na(VARIABLE)), aes(x=VARIABLE))+
  geom_bar(fill = "purple", colour = "black")+
  ggtitle("GRAPH TITLE")+
  xlab("X-AXIS TITLE")

#Chapter 3: bar graph of a nominal or ordinal variable with overlapping labels----
library(tidyverse)
ggplot(DATAFRAME %>% filter(!is.na(VARIABLE)), aes(x=VARIABLE))+
  geom_bar(fill = "purple", colour = "black")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  ggtitle("GRAPH TITLE")+
  xlab("X-AXIS TITLE")

#Chapter 3: histogram--------------------
library(tidyverse)
ggplot(DATAFRAME, aes(x = VARIABLE)) +
  geom_histogram(fill = "purple", colour = "black")+
  ggtitle("GRAPH TITLE")+
  xlab("X-AXIS TITLE")

#Chapter 3: histogram without scientific notation----------------
library(tidyverse)
library(scales)
ggplot(DATAFRAME, aes(x = VARIABLE)) +
  geom_histogram(fill = "purple", colour = "black")+
  scale_x_continuous(labels = label_comma())+   #gets rid of scientific notation
  ggtitle("GRAPH TITLE")+
  xlab("X-AXIS TITLE")

#Chapter 3: histogram with bin-width specified----------------
library(tidyverse)
ggplot(DATAFRAME, aes(x = VARIABLE)) +
  geom_histogram(bidwidth= NUMBER, fill = "purple", colour = "black")+
  ggtitle("GRAPH TITLE")+
  xlab("X-AXIS TITLE")
#if you would rather specify the number of bins instead of the bidwidth, you
#can use bins=NUMBER instead of binwidth=NUMBER

#Chapter 5: stacked bar graph, color------------------------------
#DV means dependent variable and IV means independent variable
library(tidyverse)
library(scales)
plotting.data<-DATAFRAME %>%
  filter(!is.na(IV)&!is.na(DV)) %>% 
  group_by(IV, DV) %>% 
  summarise(n=n()) %>% 
  mutate(freq = n / sum(n))
ggplot(plotting.data, aes(x = IV, y = freq, fill=DV)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels=percent)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+#you can remove this line if 
                                                  #you don't want to stagger the 
                                                  #labels on the x-axis
  scale_fill_viridis_d(name="LEGEND TITLE")+
  ggtitle("GRAPH TITLE")+
  xlab("X-AXIS TITLE")+
  ylab("Y-AXIS TITLE")

#Chapter 5: stacked bar graph, black and white-----------------------
#DV means dependent variable and IV means independent variable
library(tidyverse)
library(scales)
plotting.data<-DATAFRAME %>%
  filter(!is.na(IV)&!is.na(DV)) %>% 
  group_by(IV, DV) %>% 
  summarise(n=n()) %>% 
  mutate(freq = n / sum(n))
ggplot(plotting.data, aes(x = IV, y = freq, fill=DV)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels=percent)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+#you can remove this line if 
                                                  #you don't want to stagger the 
                                                    #labels on the x-axis
  scale_fill_brewer(palette="Greys", name="LEGEND TITLE")+
  ggtitle("GRAPH TITLE")+
  xlab("X-AXIS TITLE")+
  ylab("Y-AXIS TITLE")

#chapter 5: boxplot of a single interval variable--------------
library(tidyverse)
ggplot(DATAFRAME, aes(y = VARIABLE)) +
  geom_boxplot(color="black", fill="purple", alpha=0.2)+
  scale_x_continuous(breaks = NULL) +
  ggtitle("GRAPH TITLE")+
  xlab(NULL)+
  ylab("Y-AXIS LABEL")

#chapter 5: boxplot of an interval DV and an ordinal/nominal IV------
#DV means dependent variable and IV means independent variable
library(tidyverse)
ggplot(DATAFRAME, aes(x = IV, y = DV)) +
  geom_boxplot(color="black", fill="purple", alpha=0.2)+
  ggtitle("GRAPH TITLE")+
  xlab("X-AXIS LABEL")+
  ylab("Y-AXIS LABEL")

#chapter 5: boxplot of an interval DV, an ordinal/nominal IV, and a control-----
#DV means dependent variable and IV means independent variable
library(tidyverse)
library(scales)
ggplot(DATAFRAME %>% filter(!is.na(IV) & (!is.na(CONTROL))), 
       aes(x = CONTROL, y = DV, fill=IV)) +
  geom_boxplot()+
  ggtitle("GRAPH TITLE")+
  xlab("X-AXIS LABEL (RELATES TO CONTROL")+
  ylab("Y-AXIS TITLE")+
  scale_fill_discrete(name = "LEGEND TITLE -- RELATES TO IV")

#Chapter 6: bar graph of an ordinal/nominal IV and an interval DV with error bars-----
library(tidyverse)
plotting.data<-DATAFRAME %>% 
  filter(!is.na(IV)) %>%
  filter(!is.na(DV)) %>%
  group_by(IV) %>% 
  summarise( 
    n=n(),
    mean=mean(DV),
    sd=sd(DV)) %>%
  mutate(se=sd/sqrt(n))  %>%
  mutate(ci=se * qt((1-0.05)/2 + .5, n-1))

ggplot(plotting.data) +
  geom_bar(aes(x=IV, y=mean), stat="identity", fill="purple", alpha=0.5) +
  geom_errorbar( aes(x=IV, ymin=mean-ci, ymax=mean+ci), width=0.4, colour="black", alpha=0.9, size=1.5) +
  ggtitle("GRAPH TITLE", 
          subtitle="Error bars represent 95% confidence intervals")+
  xlab("X-AXIS LABEL")+
  ylab("Y-AXIS LABEL")

#Chapter 6: bar graph of an ordinal/nominal IV and nominal/ordinal DV with error bars-----
#NOTE: YOU MUST SELECT A VALUE OF THE DV THAT YOU WANT TO FOCUS ON FOR THIS ANALYSIS
library(tidyverse)
plotting.data<-DATAFRAME %>% 
  filter(!is.na(IV)) %>% 
  filter(!is.na(DV)) %>% 
  group_by(IV) %>% 
  summarise(
    n=n(),
    numerat=sum(DV=="VALUE YOU ARE INTERESTED IN")) %>% 
  mutate(proportion=numerat/n) %>% 
  rowwise() %>%
  mutate(lower_ci = prop.test(numerat, n, conf.level=0.95)$conf.int[1]) %>% 
  rowwise() %>% 
  mutate(upper_ci = prop.test(numerat, n, conf.level=0.95)$conf.int[2]) 

ggplot(plotting.data) +
  geom_bar(aes(x=IV, y=proportion), stat="identity", fill="purple", alpha=0.5)  +
  geom_errorbar(aes(x=IV, ymin=lower_ci, ymax=upper_ci), width=0.4, colour="black", alpha=0.9, size=1.5) +
  ggtitle("GRAPH TITLE",
          subtitle="Error bars represent 95% confidence intervals")+
  xlab("X-AXIS LABEL")+
  ylab("Y-AXIS LABEL")

#Chapter 8: basic scatterplot----------------
library(tidyverse)
library(ggrepel)
ggplot(DATAFRAME, aes(x = IV, y = DV)) +
  geom_point(alpha=.3)+
  labs(x = "X-AXIS LABEL", 
       y = "Y-AXIS LABEL")+
  ggtitle("GRAPH TITLE")+  #if you aren't adding labels to your dots, delete the +
  #only include the following line if you want to name your dots after states or 
  #countries or some other variable.
  geom_text_repel(aes(label = VARIABLE YOU WANT TO USE TO NAME DOTS), size=3)

#Chapter 8: scatterplot with regression line fitted-----------------
library(tidyverse)
library(ggrepel)
ggplot(DATAFRAME, aes(x = IV, y = DV)) +
  geom_smooth(method='lm', formula= y~x)+
  geom_point(alpha=.3)+
  labs(x = "X-AXIS LABEL", 
       y = "Y-AXIS LABEL")+
  ggtitle("GRAPH TITLE")+  #if you aren't adding labels to your dots, delete the +
  #only include the following line if you want to name your dots after states or 
  #countries or some other variable.
  geom_text_repel(aes(label = VARIABLE YOU WANT TO USE TO NAME DOTS), size=3)

#chapter 9: scatterplot faceted by a nominal or ordinal variable--------------
ggplot(data=DATAFRAME, aes(x=IV, y=DV)) +
  geom_point(size=2, col="PURPLE", alpha=.5) +
  theme_bw()+
  #add axis labels
  labs(title = "GRAPH TITLE", x="X-AXIS TITLE", y="Y-AXIS TITLE")+
  #add regression line
  geom_smooth(method='lm', color="BLACK")+
  #add nominal or ordinal variable to use for facets
  facet_wrap(~SECOND IV, WHICH IS NOMINAL OR ORDINAL)

#chapter 9: scatterplot with dots colored according to a nominal or ordinal variable---------
ggplot(data=DATAFRAME, aes(x=IV, y=DV,color=NOMINAL OR ORDINAL VARIABLE)) +
  geom_point(size=2, alpha=.5) +
  theme_bw()+
  #add axis labels
  labs(title = "GRAPH TITLE", x="X-AXIS TITLE", y="Y-AXIS TITLE", 
       color="LEGEND NAME")+
  #add regression line
  geom_smooth(method='lm')



