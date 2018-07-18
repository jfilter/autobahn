library(tidyverse)

df <- read_csv('raw.csv')

df$yy <- paste(ceiling(df$Baujahr / 5) * 5 - 5 , 'bis', ceiling(df$Baujahr / 5) * 5 - 1)
df$Zustand <- df$`Zustandsnote 03/2018` / 10
df[df=="2015 bis 2019"] <- "seit 2015"

df %>%
  filter(Baujahr > 1930) %>%
  mutate(high = Zustand < 3.0) %>%
  ggplot() +
  geom_bar(aes(Zustand, fill=high), width=0.1, position = position_dodge(width=0.1), stat="count") + 
  facet_wrap(~yy, ncol = 3) + 
  theme_classic() +
  labs(title="Zustand der Fernstra\u00DFenbr\u00FCcken in Deutschland", subtitle="Die Br\u00FCcken werden mit Schulnoten von 1.0 bis 4.0 benotet. Noten ab 3.0 sind in der Graphik hervorgehoben.", caption="Stand: 03/2018, Quelle: BMVI/BASt") + 
  guides(fill=FALSE) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.grid.major.y = element_line(colour="gray91", size=0.2), strip.background = element_rect(colour = 'gray81')) +
  scale_fill_manual(values=c("#ef8a62","#67a9cf", "#ef8a62"))
