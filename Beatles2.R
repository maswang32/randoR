#Installing and Loading Packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("data.table")
install.packages("farver")


library(dplyr)
library(ggplot2)
library(ggthemes)
library(data.table)
library(farver)


#Loading the Data Frame
df <-read.csv("BeatlesChords.csv")

#Calculating Each Song's Average Brightness, from Chords
#Also calculating how many chords are basic (I, IV, or V),
#Average minorness, and average dominance of the song.
df <- df %>% 
  mutate(pop_score = 50-hot_100_peak+2*num_weeks) %>%
  mutate(avg_bright = (-30*chord_IIm + 35*chord_II - 20*chord_IIIm +
                         40*chord_III + 15*chord_IV + 20*chord_V + 30*chord_V7 +
                         -25*chord_VIm + 20*chord_VII)/total_chords) %>%
  mutate(basicness_ratio = (chord_I + chord_IV + chord_V +chord_V7)/(total_chords)) %>%
  mutate(chord_I_ratio = chord_I/total_chords) %>%
  mutate(minor_ratio = (chord_IIm + chord_IIIm + chord_IVm + chord_VIm)/total_chords) %>%
  mutate(dominance = (chord_V7 + chord_V + chord_III) + 
           0.5*(chord_IV + chord_IVm + chord_IIm)) %>%
  mutate(complexity = (10*chord_IIm + 30*chord_II + 15*chord_IIIm +
                         30*chord_III + 9*chord_V7 +
                         +5*chord_VIm + 35*chord_VII)/total_chords + total_chords/5) %>%
  mutate(effort = complexity + (length-120)/10 + instruments*1.5) %>%
  arrange(effort)



#A simplified table with the most important data, for the report
simple_table <- select(df, c(song_name, avg_bright, basicness_ratio, pop_score, complexity, length, instruments, effort))
simple_table
#Setting the theme and graphing histograms
theme_set(theme_wsj()+theme(axis.title=element_text(size=16)))
ggplot(df, aes(avg_bright)) + geom_histogram(bins=30)+
  annotate(
    "curve",
    x = 18, y = 2.5,
    xend = 20, yend = 1,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "grey40"
  )+
  annotate(
    "text",
    x = 18, y = 3,
    label = "Day\nTripper",
    vjust = 1, size = 5, color = "grey40"
  )+
  annotate(
    "curve",
    x = -11, y = 2,
    xend = -12, yend = 1,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "grey40"
  )+
  annotate(
    "text",
    x = -10, y = 2,
    label = "Eleanor\nRigby",
    vjust = 1, size = 5, color = "grey40"
  )+
  labs(title = "Distribution of Brightness Scores", y="Occurences",x="Brightness Score")



ggplot(df, aes(complexity)) + geom_histogram(bins=30)+
  annotate(
    "curve",
    x = 40, y = 2.5,
    xend = 41, yend = 1,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "grey40"
  )+
  annotate(
    "text",
    x = 40, y = 3,
    label = "We Can Work It Out",
    vjust = 1, size = 5, color = "grey40"
  )+

  labs(title = "Distribution of Complexity Scores", y="Occurences",x="Complexity Score")

ggplot(df, aes(effort)) + geom_histogram(bins=30)+
  annotate(
    "curve",
    x =86, y = 2.5,
    xend = 88, yend = 1,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "grey40"
  )+
  annotate(
    "text",
    x = 86, y = 3,
    label = "Hey Jude",
    vjust = 1, size = 5, color = "grey40"
  )+
  
  labs(title = "Distribution of Effort Scores", y="Occurences",x="Effort Score")






#Graphing various chordal properies vs. popularity.
ggplot(df, aes(x=avg_bright, y=pop_score)) + 
  geom_point() +
  geom_smooth() + 
  labs(y="Popularity", x="Brightness of Chords",
       title = "Popularity vs Brightness")

ggplot(df, aes(x=length, y=pop_score)) + 
  geom_point() +
  geom_smooth() + 
  labs(y="Popularity", x="Length of Song",
       title = "Popularity vs Duration")

ggplot(df, aes(x=instruments, y=pop_score)) + 
  geom_point() +
  geom_smooth() + 
  labs(y="Popularity", x="Number of Instruments",
       title = "Popularity vs Number of Instruments")


ggplot(df, aes(x=complexity, y=pop_score)) + 
  geom_point() +
  geom_smooth() + 
  labs(y="Popularity", x="Chordal Complexity Score",
       title = "Popularity vs Complexity")

ggplot(df, aes(x=effort, y=pop_score)) + 
  geom_point() +
  geom_smooth() + 
  labs(y="Popularity", x="Effort Score",
       title = "Popularity vs Effort Score")

ggplot(df,aes(y=pop_score,x=avg_bright,
              color=effort))+
  geom_point()+
  stat_smooth(method="lm",se=FALSE)+
   facet_wrap(~ cut_number(effort, 5))+
  labs(y="Popularity", x="Brightness of Chords",
       title = "Different Effort Ranges")


#
#
#
#Extra graphs and factors for fun
ggplot(df, aes(x=basicness_ratio, y=pop_score)) +
  geom_point() +
  geom_smooth() + 
  labs(y="Popularity", x="Basicness of Chords",
       title = "Popularity vs Basicness")

