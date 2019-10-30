
```{r}

library(ggplot2)
library(ggthemes)
library(reshape2)

#loading the sheet
thing <- read.csv("10.csv", header= TRUE, check.names = FALSE)

# changing data structure
data <- melt(thing, id=c("Party"))

#show(data)

ggplot(data) +
  #Re-Order gives the chart a sequential look, which makes it easier to interpret
  geom_bar(mapping = aes(x = reorder(variable,value), y = value, fill = Party), stat = "identity") +
  coord_flip() +
  # the reverse stack
  scale_fill_manual(values = c("dodgerblue4","black","red","gold2","darkgreen", "darkmagenta")) +
  # change the colours to match party affiliation
  facet_wrap(~Party)+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "moccasin"),
        plot.background = element_rect(fill = "moccasin"),
        panel.grid = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(colour = "black"),
        strip.background = element_rect(fill = "moccasin")
        
  )+
  labs(title = "Welsh AM's education - Split by Party", x = "Education Level", y = "Number of Assembly Members")

ggsave("amseducation2.png", width = 30, height = 10, units = "cm")


```





```{r}
library(ggplot2)
library(ggthemes)
library(reshape2)


#loading the sheet
thing <- read.csv("10.csv", header= TRUE, check.names = FALSE)

# changing data structure
data <- melt(thing, id=c("Party"))



ggplot(data) +
  #Re-Order gives the chart a sequential look, which makes it easier to interpret
  geom_bar(mapping = aes(x = reorder(variable,value), y = value, fill = Party), stat = "identity") +
  coord_flip() +
  # the reverse stacfk
  scale_fill_manual(values = c("dodgerblue4","black","red","gold2","darkgreen", "darkmagenta")) +
  # change the colours to match party affiliation
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "moccasin"),
        plot.background = element_rect(fill = "moccasin"),
        panel.grid = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(colour = "black"))+
  labs(title = "Welsh AM's education", x = "Education Level", y = "Number of Assembly Members")

ggsave("amseducation.png", width = 30, height = 10, units = "cm")


```









```{r}

library(ggplot2)
library(ggthemes)
library(reshape2)

#loading the sheet
thing <- read.csv("8.csv", header=+ TRUE, check.names = FALSE)

# changing data structure
data <- melt(thing, id=c("Party"))

#show(data)



ggplot(data) +
  geom_bar(mapping = aes(x = reorder(variable,value), y = value, fill = Party), stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("dodgerblue4","black","red","gold2","darkgreen", "darkmagenta")) + # While it would have been preferential to use the variable method of the other diagrames, it didn't seem to work for this visualisation. 
  theme(legend.position = "top", 
        panel.background = element_rect(fill = "moccasin"),
        plot.background = element_rect(fill = "moccasin"),
        panel.grid = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(colour = "black")
        
  )+
  labs(title = "Welsh AM's Career Distribution", x = "Industry", y = "Number of Assembly Members")




ggsave("amsjobs.png", width = 30, height = 20, units = "cm")



```

```{r}
library(ggplot2)
library(ggthemes)
library(reshape2)

#loading the sheet
thing <- read.csv("8.csv", header= TRUE, check.names = FALSE)

# changing data structure
data <- melt(thing, id=c("Party"))

#show(data)



ggplot(data) +
  geom_bar(mapping = aes(x = reorder(variable,value), y = value, fill = Party), stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("dodgerblue4","black","red","gold2","darkgreen", "darkmagenta")) +
  facet_grid(~Party, scales = "free_y") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "moccasin"),
        plot.background = element_rect(fill = "moccasin"),
        panel.grid = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill = "moccasin"),
        legend.background = element_rect(colour = "black")
        
  )+
  labs(title = "Welsh AM's Career Distribution", x = "Industry", y = "Number of Assembly Members")




ggsave("amsjobs2.png", width = 30, height = 20, units = "cm")

```




```{r}
library(ggplot2)
library(ggalluvial)
library(readr)
library(ggthemes)

party <- c(
  "Conservatives" = "dodgerblue4",
  "Independent" = "black",
  "Labour" = "red",
  "Liberal Democrats" = "gold2",
  "Plaid Cymru" = "darkgreen",
  "UKIP" = "darkmagenta"
) # Variable to store Party colours 

#thing <- read_csv("6.csv", col_names = TRUE, na = c("", NA)) # FILLING BLANKS WITH na

thing <- read.csv("rejig.csv", header= TRUE, na = (""))


show(thing)

ds <- as.data.frame(thing)

ggplot(data = ds,
       aes(axis1 = State, axis2 = One, axis3 = Two, axis4 = Three, axis5 = Four, axis6 = Five, axis7 = Six, axis8 = Seven, axis9 = Eight, axis10 = Nine, axis11 = Ten, axis12 = Eleven, axis13 = Twelve, axis14 = Thirteen, axis15 = Fourteen, axis16 = Senedd, 
           y = Freq)) + # Loading the nodes each axis is currently alone apart from School and "Post" reflecting start and end points
  scale_x_discrete(limits = c(), expand= c(.1,0.5)) +
  scale_fill_manual(
    values = party,
    limits = names(party)
  ) + # loading the variable values and assigning colours
  # stat_alluvium() +
  geom_alluvium(aes(fill = Party)) +  # letting the fill occur
  
  geom_stratum(alpha = .55) + # Changing the strata to see-through to allow counting
  
  geom_text(stat = "stratum",  label.strata = TRUE, check_overlap = TRUE) + # Labels, and preventing overlap
  
  # Adjusting the flow aesthetics, I think these allow a clear sight of where things are going. 
  
  theme(axis.title.y = element_blank(), # removing the "frequency"
        axis.text.y = element_blank(),  # values as they're utterly
        axis.ticks.y = element_blank(), # meaningless in this
        legend.position = "bottom",
        legend.background = element_rect(fill = "white", colour = "black"),
        panel.background = element_rect(fill = "moccasin", colour = "moccasin"),
        plot.background = element_rect(fill = "moccasin"),
        panel.grid = element_line(colour = "moccasin") # removing white gridelines
  )+
  labs(title = "Welsh Assembley Member's Career pathways") +
  
  ggsave("pic.png", width = 50, height = 30, units = "cm")

```

```{r}

library(ggplot2)
library(ggalluvial)
library(readr)
library(ggthemes)

party <- c(
  "Conservatives" = "dodgerblue4",
  "Independent" = "black",
  "Labour" = "red",
  "Liberal Democrats" = "gold2",
  "Plaid Cymru" = "darkgreen",
  "UKIP" = "darkmagenta"
) # Variable to store Party colours 


thing <- read.csv("rejig.csv", header= TRUE, na = (""))


show(thing)

ds <- as.data.frame(thing)

ggplot(data = ds,
       aes(axis1 = State, axis2 = One, axis3 = Two, axis4 = Three, axis5 = Four, axis6 = Five, axis7 = Six, axis8 = Seven, axis9 = Eight, axis10 = Nine, axis11 = Ten, axis12 = Eleven, axis13 = Twelve, axis14 = Thirteen, axis15 = Fourteen, axis16 = Senedd, 
           y = Freq)) + # Loading the nodes each axis is currently alone apart from School and "Post" reflecting start and end points
  scale_x_discrete(limits = c(), expand= c(.1,0.5)) +
  scale_fill_manual(
    values = party,
    limits = names(party)
  ) + # loading the variable values and assigning colours
  # stat_alluvium() +
  geom_alluvium(aes(fill = Party)) +  # letting the fill occur
  
  geom_stratum(alpha = .55) + # Changing the strata to see-through to allow counting
  
  geom_text(stat = "stratum",  label.strata = TRUE, check_overlap = TRUE) + # Labels, and preventing overlap
  
  # Adjusting the flow aesthetics, I think these allow a clear sight of where things are going. 
  facet_wrap(~Name) +
  theme(axis.title.y = element_blank(), # removing the "frequency"
        axis.text.y = element_blank(),  # values as they're 
        axis.ticks.y = element_blank(), # meaningless in this context
        legend.position = "None",
        panel.background = element_rect(fill = "moccasin", colour = "moccasin"),
        plot.background = element_rect(fill = "moccasin"),
        strip.background = element_rect(fill = "moccasin"),
        panel.grid = element_line(colour = "moccasin")  # removing white gridelines
  ) +
  labs(title = "Individual Assembley Members Career pathways")

ggsave("facetpic.png", width = 100, height = 30, units = "cm")

```


```{r}

library(ggplot2)
library(ggalluvial)
library(readr)
library(ggthemes)

party <- c(
  "Conservatives" = "dodgerblue4",
  "Independent" = "black",
  "Labour" = "red",
  "Liberal Democrats" = "gold2",
  "Plaid Cymru" = "darkgreen",
  "UKIP" = "darkmagenta"
) # Variable to store Party colours 

#thing <- read_csv("6.csv", col_names = TRUE, na = c("", NA)) # FILLING BLANKS WITH na

thing <- read.csv("pc.csv", header= TRUE, na = (""))

show(thing)

ggplot(thing, aes(x="", y=Number, fill= Party)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = party,
                    limits = names(party)) +
  theme(panel.background = element_rect(fill = "moccasin"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "moccasin"),
        legend.background = element_rect(fill = "white", colour = "black")
        
  )+
  labs(title = "Senedd Composition", x ="", y="")


ggsave("pie.png", width = 10, height = 10, units = "cm")


```
