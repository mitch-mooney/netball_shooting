library(ggplot2)
library(plotly)
library(ggpmisc)
library(dplyr)

shots <- read.csv('netball_shots.csv')

# plot to see if home or away predictions are more effective
plotly_build(shots%>%
               mutate(shotResult = ifelse(scorepoints == 1, "Goal", "Miss")) %>% 
               ggplot(aes(x = shotResult, fill = shotResult))+
               geom_bar(aes(y = (..count..)/sum(..count..)))+
               ggtitle('Goal shooting percentage for all attempts since 2009')+
               ylab("percentage of all shots")+
               xlab('Attempt result')
)
  

# long shots percentage
plotly_build(shots %>%
  filter(shotDistance == "long") %>% 
ggplot(aes(x = shotResult, fill = shotResult))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  ggtitle('Long goal shooting percentage for all attempts since 2009')+
  ylab("percentage of all long shots")+
  xlab('Attempt result')
)

# Percent of shots taken at distance vs short
plotly_build(shots %>%
               ggplot(aes(x = shotDistance, fill = shotDistance))+
               geom_bar(aes(y = (..count..)/sum(..count..)))+
               ggtitle('Long v short goal attempts since 2009')+
               ylab("percentage of all shots")+
               xlab('Attempt distancet')+
               facet_grid(shotResult ~.))

# Percent of shots taken at distance
plotly_build(shots %>%
               ggplot(aes(x = shotDistance, fill = shotDistance))+
               geom_bar(aes( y = (..count..)/sum(..count..)))+
               ggtitle('Long v short goal shooting percentage for all attempts since 2009')+
               ylab("percentage of all shots")+
               xlab('Attempt result')
)



#summarize shots by position
shot_summary<-shots %>%
  group_by(shot_pos, displayName) %>%
  summarise(goals = sum(scorepoints), attempts = n(), perc = goals/attempts)
#provide an x and y coordinate for the shot summary
shot_summary <- shot_summary %>%
  mutate(x = ifelse(shot_pos == 1, -4,
                    ifelse(shot_pos == 2, -3.5,
                           ifelse(shot_pos == 3, 0,
                                  ifelse(shot_pos == 4, 3.5, 4))))) %>%
  mutate(y = ifelse(shot_pos == 1, 0.25,
                    ifelse(shot_pos == 2, 1.8,
                           ifelse(shot_pos == 3, 3.5,
                                  ifelse(shot_pos == 4, 1.8, 0.25)))))

shot_summary$perc <- round(shot_summary$perc,2) # round percentage to 2 decimal points

# plot the display of long shots
shot_summary %>%
  filter(displayName == "E.Bell")%>% #choose a different player
  ggplot(aes(x = x, y = y))+
  geom_text(aes(label=paste("(",goals,",",attempts,")",perc)))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"))+
  geom_path(data = court, aes(Y, X - 2.805), colour = "black", size = 1)+
  geom_path(data = court, aes(Y/2, X/2-1.4), colour = "black", size = 1)+
  #Baseline shot Line (10.16m
  geom_segment(aes(x = -4.9, y = 0.5, xend = 4.9, yend = 0.5), lwd = 1, color = "black") +
  #outter segements
  geom_segment(aes(x = 0, y = 0, xend = 2.5, yend = 4.24), lwd = 1, color = "black") +
  #outter segements
  geom_segment(aes(x = 0, y = 0, xend = -2.5, yend = 4.24), lwd = 1, color = "black")+
  ggtitle("Long shot chart: E.Bell",
          subtitle = "(goals, attempts) percentage")
