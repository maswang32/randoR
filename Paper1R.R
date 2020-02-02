############################
####PRELIMINARY STEPS#######
############################


#Installing and Loading Packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("data.table")
install.packages("farver")
install.packages("tidyverse")
install.packages("readstata13")
install.packages("cowplot")
library(dplyr)
library(ggplot2)
library(ggthemes)
library(data.table)
library(farver)
library(readstata13)
library(tidyverse)
library(cowplot)

#Putting the data into the dataframe
df<-read.dta13("ECLS-K.dta",
               nonint.factors = T,
               generate.factors = T)


#Select the five variables of interest
df_sub_1 <- select(df, p1ageent, p2homecm, homelang, P1SINGSO, c1gscale )
View(df_sub_1)
df_use <- df_sub_1 %>%
  drop_na() %>%
  filter(p2homecm != -9) %>%
  filter(p2homecm !=-8) %>%
  filter(p2homecm != -7) %>%
  filter(c1gscale  > 0) %>%
  droplevels()


View(df)
View(df_use)







#########################
######UNIVARIATES########
#########################

#First, analyze the univariate distribution of age of entry
summary(df_use$p1ageent)
sd(df_use$p1ageent)

df_use %>%
  ggplot(aes(p1ageent)) +
  geom_histogram(bins=26, colour="black", 
                 aes(y=..density.., fill=..count..)) +
  scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C") +
  stat_function(fun=dnorm,
                color="red",
                args=list(mean=mean(df_use$p1ageent), 
                          sd=sd(df_use$p1ageent))) +
  labs(title="Distribution of Kindergarten entry age",x="Age in Months")


#Second, analyze the distribution of the child computer
df_use %>%
  select(p2homecm) %>%
  table()

df_use %>%
  select(p2homecm) %>%
  table() %>%
  prop.table()

df_use %>%
  ggplot(aes(p2homecm)) + 
  geom_bar(width = 0.5) +
  theme_bw() +
  labs(title ="Does the Child have a home computer that they use?")

#Third, analyze the distribution of non-english home languages
df_use %>%
  select(homelang) %>%
  table()

df_use %>%
  select(homelang) %>%
  table() %>%
  prop.table() 

df_use %>%
  ggplot(aes(homelang)) + 
  geom_bar(width = 0.5) +
  theme_bw() +
  labs(title ="What language does the child speak at home?")


#Fourth, analyze the distribution of song singing
df_use %>%
  select(P1SINGSO) %>%
  table()

df_use %>%
  select(P1SINGSO) %>%
  table() %>%
  prop.table()

df_use %>%
  ggplot(aes(P1SINGSO)) + 
  geom_bar(width = 0.5) +
  theme_bw() +
  labs(title="How often do parents and their children sing songs together?")


#Fifth, analyze the distribution of IRT Reading scores
summary(df_use$c1gscale )
sd(df_use$c1gscale)

df_use %>%
  ggplot(aes(c1gscale )) +
  geom_histogram(bins=26) +
  theme_bw() +
  xlim(0,50) +
  labs(title="Distribution of IRT General Knowledge scores",x="score")





###############################
#####BIVARIATE ASSOCIATIONS####
###############################


###First, entry age and reading scores.
df_use %>%
  ggplot(aes(x = p1ageent, y = c1gscale )) + geom_point() +
  theme_bw() +
  geom_smooth() +
  labs(y="General Knowledge IRT Score", x="Age at Kindergarten entry, months", title="Knowledge vs Entry Age")

cor(df_use$p1ageent, df_use$c1gscale )

#For ages less than and greater than 75?
df_filtered <- df_use %>%
  filter(p1ageent<76)

View(df_filtered)
cor(df_filtered$p1ageent, df_filtered$c1gscale )

df_fil2 <- df_use %>%
  filter(p1ageent>75)
cor(df_fil2$p1ageent, df_fil2$c1gscale )




##Second, computer use and reading scores.
df_use %>%
  ggplot(aes(x = p2homecm, y = c1gscale )) + 
  geom_boxplot(width = 0.5) +
  theme_bw()+
  labs(y="General Knowledge IRT Score", x="Home Computer usage", title="Computer Usage and Knowledge Scores")

df_use %>%
  filter(p2homecm == "no") %>%
  select(c1gscale ) %>%
  summary()

df_use %>%
  filter(p2homecm == "yes") %>%
  select(c1gscale ) %>%
  summary()

df_use %>%
  group_by(p2homecm) %>%
  summarize(n_obs = n(),
            mean_reading = mean(c1gscale ),
            std_reading = sqrt(var(c1gscale )), # square root of var
            the_25th_q = quantile(c1gscale , .25),
            median = median(c1gscale ),
            the_75th_q = quantile(c1gscale , .75)
  )

##Third, non-english home languages and reading score
df_use %>%
  ggplot(aes(x = homelang, y = c1gscale )) + 
  geom_boxplot(width = 0.5) +
  theme_bw()+
  labs(x="Language usage at home", y="General Knowledge IRT Score", title="Home language vs Knowledge level")

df_use %>%
  filter(homelang == "no") %>%
  select(c1gscale ) %>%
  summary()

df_use %>%
  filter(homelang == "yes") %>%
  select(c1gscale ) %>%
  summary()

df_use %>%
  group_by(homelang) %>%
  summarize(n_obs = n(),
            mean_reading = mean(c1gscale ),
            std_reading = sqrt(var(c1gscale )),
            the_25th_q = quantile(c1gscale , .25),
            median = median(c1gscale ),
            the_75th_q = quantile(c1gscale , .75)
  )

#What if we separate by hispanics?

df_sub_2 <- select(df, homelang, c1gscale, HISPANIC, race)

df_his <- df_sub_2 %>%
  drop_na() %>%
  filter(c1gscale  > 0) %>%
  droplevels()

df_his %>%
  ggplot(aes(x = homelang, y = c1gscale, color = race, shape = race)) + 
  geom_boxplot(width = 0.5) +
  facet_grid(~race) +   
  theme_bw() + 
  xlab("") + 
  ylab("Knowledge IRT score") + 
  theme(legend.position = 'none') +
  scale_x_discrete(labels = c('English','NonEnglish'))



#Fourth, song singing and reading score
df_use %>%
  ggplot(aes(x = P1SINGSO, y = c1gscale )) + 
  geom_boxplot(width = 0.5) +
  theme_bw()+
  labs(x="How often to children sing songs with Parents?", y="General Knowledge IRT score", title="Singing and General Knoweldge")


df_use %>%
  group_by(P1SINGSO) %>%
  summarize(n_obs = n(),
            mean_reading = mean(c1gscale ),
            std_reading = sqrt(var(c1gscale )),
            the_25th_q = quantile(c1gscale , .25),
            median = median(c1gscale ),
            the_75th_q = quantile(c1gscale , .75))







