sum(is.na(olympics_II))
olympics <- na.omit(olympics)
sum(is.na(olympics))

devtools::install_github("thomasp85/transformr")

install.packages("dplyr")
library(dplyr)
library(ggplot2)

olympic_new <- olympics_II  

install.packages("RColorBrewer")
library(RColorBrewer)


############## Grouping bar graphs (top 4 countries by year) #####################################
da <- filter(olympic_new, Country_Code == "USA")
db <- filter(olympic_new, Country_Code == "RUS")
dc <- filter(olympic_new, Country_Code == "AUS")
dd <- filter(olympic_new, Country_Code == "GER")
total <- rbind(da, db,dc,dd)

total <- group_by(total, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+
                                                              sum(Silver) + sum(Gold))
total
test <- ggplot(total, aes(x = Year, y = All_Medals, fill = Country_Code)) +
  geom_col(position="dodge", colour="black")
test + scale_x_continuous(breaks=total$Year) +
  labs(x="Year",
       y="Medals Won",
       title= "Top Four Countries Total Medals Won per Year",
       fill="Country") +
  scale_fill_manual(values = c("gold", "green", "red", "blue"))

############## USA by year (gold, silver, & bronze) ########################################
USA <- filter(olympic_new, Country_Code == "USA")
USA <- group_by(USA, Year, Medal, All) %>% summarize(All_Medals = sum(Bronze)+
                                                       sum(Silver) + sum(Gold))
USA
USA$Medal <- factor(USA$Medal, levels = c("Bronze", "Silver", "Gold"))

US_bar <- ggplot(USA, aes(x = Year, y = All_Medals, fill = Medal)) +
  geom_col(position = "dodge", colour = "black")

US_bar + scale_x_continuous(breaks=USA$Year) +
  labs(x="Year",
       y="Number of Medals",
       fill="Medals") +
  scale_fill_manual(values = c("#E69F00", "grey", "gold")) +
  ggtitle("USA Summer Olympic Medals Won per Year (1976-2008)", 
          "*US did not participate in 1980 Summer Olympic Games") +
  theme_minimal() +
  theme(panel.border=element_rect(colour="black", fill=NA, size=1),
        legend.background = element_rect(colour="black", size=0.5),
        panel.grid.minor.x = element_blank())


############  US v Russia vs Germany facet wrap ###################################################
USA <- filter(olympic_new, Country_Code == "USA")
RUS <- filter(olympic_new, Country_Code == "RUS")



USA <- group_by(USA, Year, Country_Code, Medal, All) %>% summarize(All_Medals = sum(Bronze)+
                                                           sum(Silver) + sum(Gold))

RUS <- group_by(RUS, Year, Country_Code, Medal, All) %>% summarize(All_Medals = sum(Bronze)+
                                                                     sum(Silver) + sum(Gold))

USA_RUS <- rbind(USA, RUS)

USA_RUS$Medal <- factor(USA_RUS$Medal, levels = c("Bronze", "Silver", "Gold"))

USA_RUSbar <- ggplot(USA_RUS, aes(x=Year, y=All_Medals, fill=Medal))+
  geom_col(position = "dodge", colour = "black")
USA_RUSbar
  
USA_RUSbar + facet_wrap(Country_Code ~ .) +
  scale_x_continuous(breaks=USA_RUS$Year) +
  labs(x="Year",
       y="Number of Medals Won",
       fill="Medals") +
  scale_fill_manual(values = c("#E69F00", "grey", "gold")) +
  ggtitle("Russia vs United States Summer Olympic Medals Won per Year (1976-2008)") +
  theme_bw()+
  theme(panel.border=element_rect(colour="black", fill=NA, size=1),
        legend.background = element_rect(colour="black", size=0.5),
        panel.grid.minor.x = element_blank())


####################### (Treemap) ######################################################
library(ggplot2)
install.packages("treemapify")
library(treemapify)
install.packages("treemap")
library(treemap)
library(dplyr)

data <- group_by(olympic_new, Sport, Discipline) %>% summarize(Participants = sum(Bronze)+sum(Silver)+sum(Gold))
data
ggplot(data, aes(area=Participants, fill= Participants, label=Sport, subgroup=Discipline))+
  geom_treemap()+
  geom_treemap_subgroup_text(place="centre", grow=T, alpha=.9, colour="White", fontface="italic", min.size=0)+
  geom_treemap_text(colour="Red", place="topleft", reflow=T)+
  labs(fill="Number of Medalists")+
  ggtitle("Largest Sports by Discipline in Summer Olympic Games 1976-2008")+
  theme(plot.title = element_text(size = 16, face="bold"))+
  theme(panel.border=element_rect(colour="black", fill=NA, size=1),
        legend.background = element_rect(colour="black", size=0.5))+
  scale_fill_continuous(trans = 'reverse', guide = guide_colourbar(reverse=T))

############################# world map ###########################################
install.packages("mapproj")
library(mapproj)

world = map_data('world') 

ol <- olympics_II



ol <- group_by(olympic_new,Country) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))

ol$Country[which(ol$Country == "United States")] = "USA" 
ol$Country[which(ol$Country == "Czechoslovakia")] = "Czech Republic"
ol$Country[which(ol$Country == "United Kingdom")] = "UK"
ol$Country[which(ol$Country == "Korea, North")] = "North Korea"
ol$Country[which(ol$Country == "Korea, South")] = "South Korea"


olympic_map <- left_join(world, ol, by = c("region" = "Country"))

ggplot(olympic_map,
       aes(x=long, y=lat, group=group, fill= All_Medals))+
  geom_polygon(colour="black")+
  scale_fill_continuous(na.value="ivory",low = "ivory", high = "brown")+
  labs(x="Longitude",
       y="Latitude",
       fill="Number of Medals") +
  ggtitle("Total Number of Medals Won by Country in Summer Olympic Games 1976-2008")+
  theme_dark()+
  theme(plot.title = element_text(size = 16, face="bold"),
        panel.border=element_rect(colour="black", fill=NA, size=1),
        legend.background = element_rect(colour="black", size=0.5), 
        legend.position= c(0.1, 0.27))



########################## rolling total of medals won for top 6 countries  ##############################
da <- filter(olympic_new, Country_Code == "USA")
da <- group_by(da, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
da[, 3] <- cumsum(da[, 3])

db <- filter(olympic_new, Country_Code == "RUS")
db <- group_by(db, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
db[, 3] <- cumsum(db[, 3])

dc <- filter(olympic_new, Country_Code == "AUS")
dc <- group_by(dc, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
dc[, 3] <- cumsum(dc[, 3])

dd <- filter(olympic_new, Country_Code == "GER")
dd <- group_by(dd, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
dd[, 3] <- cumsum(dd[, 3])

df <- filter(olympic_new, Country_Code == "CHN")
df <- group_by(df, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
df[, 3] <- cumsum(df[, 3])

dg <- filter(olympic_new, Country_Code == "ITA") 
dg <- group_by(dg, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
dg[, 3] <- cumsum(dg[, 3])

six <- rbind(da, db,dc,dd, df, dg) 

lineplt <- ggplot(six, aes(x = Year, y = All_Medals)) + 
  geom_line(aes(color = Country_Code), size=1.25)+ 
  geom_point(aes(color = Country_Code), size=1.5)+ 
  scale_color_manual(values=c("#FFFF66","#E69F00", "#000000", "#33CC00","#CC0000", "#0000CC"), 
                     guide = guide_legend(reverse = TRUE))+ 
  scale_x_continuous(breaks=c(1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))+
  theme_dark() +
  theme(panel.grid.minor.x = element_blank(), 
        panel.border=element_rect(colour="black", fill=NA, size=1), 
        legend.background = element_rect(colour="black", size=0.5))+ 
  labs(y="Number of Medals Won", color = "Country")+ 
  ggtitle("Rolling Total of Medals Won for Top Six Countries")

  
lineplt


##################### Top 10 Athletes ###########################
library(ggflags)

phelps <- filter(olympic_new, Athlete == "PHELPS, Michael")
andrianov <- filter(olympic_new, Athlete == "ANDRIANOV, Nikolay")
fischer <- filter(olympic_new, Athlete == "FISCHER, Birgit")
nemov <- filter(olympic_new, Athlete == "NEMOV, Alexei")
thompson <- filter(olympic_new, Athlete == "THOMPSON, Jenny")
torres <- filter(olympic_new, Athlete == "TORRES, Dara")
biondi <- filter(olympic_new, Athlete == "BIONDI, Matthew")
coughlin <- filter(olympic_new, Athlete == "COUGHLIN, Natalie")
dittyatin <- filter(olympic_new, Athlete == "DITYATIN, Aleksandr")
hall <- filter(olympic_new, Athlete == "HALL, Gary Jr.")

ten <- rbind(phelps, andrianov, fischer, nemov, thompson, torres, biondi, coughlin, dittyatin, hall)

athlete <- group_by(ten, Athlete, Country_Code, Medal) %>% summarize(All_Medals = sum(Bronze)+
                                                                 sum(Silver) + sum(Gold))

athlete$Country_Code[which(athlete$Country_Code == "USA")] = "us"
athlete$Country_Code[which(athlete$Country_Code == "RUS")] = "ru"
athlete$Country_Code[which(athlete$Country_Code == "GER")] = "de"

athlete$Medal <- factor(athlete$Medal, levels = c("Bronze", "Silver", "Gold"))

athlete$test <- paste(athlete$Athlete, '     ')

athletes_barplt <- ggplot(athlete, aes(x = reorder(test, All_Medals), y = All_Medals, fill=Medal)) +
  geom_bar(stat="identity", colour= "black", size= .3, width= .4)+
  geom_flag(y = -1.1, aes(country = Country_Code), size = 5) +
  coord_flip() +
  scale_fill_manual(values = c("#E69F00", "grey", "gold"), guide = guide_legend(reverse = T)) +
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16))+
  labs(y="Number of Medals", x="Athlete")+
  ggtitle("Top Ten Athletes with Most Summer Olympic Medals, 1976-2008")+
  theme_bw()+
  theme(panel.border=element_rect(colour="black", fill=NA, size=1),
        legend.background = element_rect(colour="black", size=0.5),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                        colour = "grey"),
        panel.grid.major = element_line(size = 0.35, linetype = 'dashed',
                                        colour = "grey"))


athletes_barplt

############ mosaics - top five sports #############################################
dat <- olympics_II

install.packages("ggmosaic")
library(ggmosaic)

row <- filter(dat, Sport == "Rowing")
aquatics <- filter(dat, Sport == "Aquatics")
athletics <- filter(dat, Sport == "Athletics")
gymnastics <- filter(dat, Sport == "Gymnastics")
hockey <- filter(dat, Sport == "Hockey")

five <- rbind(row, aquatics, athletics, gymnastics, hockey) 



ggplot(data= five)+
  geom_mosaic(aes(x=product(Sport, Year), fill=Gender), na.rm=TRUE)+
  labs(x="Year", y="Sport")+
  annotate(geom="text",x=0.43,y=-0.02,label="            1976   1980   1984    1988    1992    1996     2000     2004      2008",
           color="black",size=3)  +
  annotate(geom="text",x=-0.02,y=0.5,label="Acquatics           Athletics           Gym    Hockey        Rowing",
           colour="black",size=3, angle=90)+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())+
  ggtitle("Top Five Summer Olympic Sports by Year & Gender")




mosaicplot(table(five$Year, five$Sport), color=TRUE, main="Top Five Summer Olympic Sports by Year")

############ test transition ########################################
install.packages("gganimate")
library(gganimate)
install.packages("gifski")
library(gifski)

u <- filter(olympics_V, Country_Code == "USA")
u <- group_by(u, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
u[, 3] <- cumsum(u[, 3])

r <- filter(olympics_V, Country_Code == "RUS")
r <- group_by(r, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
r[, 3] <- cumsum(r[, 3])

a <- filter(olympics_V, Country_Code == "AUS")
a <- group_by(a, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
a[, 3] <- cumsum(a[, 3])

g <- filter(olympics_V, Country_Code == "GER")
g <- group_by(g, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
g[, 3] <- cumsum(g[, 3])

c <- filter(olympics_V, Country_Code == "CHN")
c <- group_by(c, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
c[, 3] <- cumsum(c[, 3])

i <- filter(olympics_V, Country_Code == "ITA") 
i <- group_by(i, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
i[, 3] <- cumsum(i[, 3])

buddy <- rbind(u, r, a, g, c, i)

plot_test <- ggplot(buddy, aes(x = reorder(Country_Code, -All_Medals), y = All_Medals, fill=Country_Code))+
  geom_bar(stat="identity", colour="black") + scale_fill_brewer(palette="Pastel1")+
  coord_flip()
  

plot_test

plot_test + transition_time(Year) +
  labs(title = "Year: {frame_time}")


setwd("/Users/jonathanlynch/Desktop/DSC 465")
olympics_V <- read.csv("olympics_V.csv", header = TRUE, sep = ",")

library(gganimate)

library(gifski)

u <- filter(olympics_V, Country_Code == "USA")
u <- group_by(u, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
u[, 3] <- cumsum(u[, 3])

r <- filter(olympics_V, Country_Code == "RUS")
r <- group_by(r, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
r[, 3] <- cumsum(r[, 3])

a <- filter(olympics_V, Country_Code == "AUS")
a <- group_by(a, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
a[, 3] <- cumsum(a[, 3])

g <- filter(olympics_V, Country_Code == "GER")
g <- group_by(g, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
g[, 3] <- cumsum(g[, 3])

c <- filter(olympics_V, Country_Code == "CHN")
c <- group_by(c, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
c[, 3] <- cumsum(c[, 3])

i <- filter(olympics_V, Country_Code == "ITA") 
i <- group_by(i, Year, Country_Code) %>% summarize(All_Medals = sum(Bronze)+sum(Silver) + sum(Gold))
i[, 3] <- cumsum(i[, 3])

buddy <- rbind(u, r, a, g, c, i)

plot_test <- ggplot(buddy, aes(x = reorder(Country_Code, -All_Medals), y = All_Medals, fill=Country_Code))+
  geom_bar(stat="identity", colour="black") + scale_fill_brewer(palette="Pastel1")+
  coord_flip()

plot_test + transition_time(Year) +
  labs(title = "Year: {frame_time}")

############### ...from Final_Project.Rmd #############################################
ggplot(top_six, aes(x = reorder(Country_Code, -All_Medals), y = All_Medals, fill=Country_Code))+
  geom_bar(stat="identity", colour="black", size= .3, show.legend= FALSE) + scale_fill_brewer(palette="Pastel1") +
  coord_flip() + transition_time(Year) +
  theme_dark()+
  labs(title = "Year: {frame_time}", x="Country", y="Medals Won")
anim_save("slap.gif")
### (rolling tot line plt goes here)
anim_save("hit.gif")
bar_gif <- image_read("slap.gif", strip = TRUE)
line_gif <- image_read("hit.gif", strip = TRUE)

new_gif <- image_append(c(bar_gif[1], line_gif[1]))
for(i in 2:100){
  combined <- image_append(c(bar_gif[i], line_gif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif

```

