library(plyr)
library(dplyr)
library(ggplot2)

megatable <- read.csv("EPL_historical_table.csv", stringsAsFactors = FALSE)
summary(megatable)

filter(megatable, Pts > 100)

megatable$Pts <- sub("393", "39", megatable$Pts)
megatable$Pts <- sub("191", "19", megatable$Pts)
megatable$Pts <- as.numeric(megatable$Pts)

summary(megatable$Pts)


champs <- filter(megatable, Pos == 1)
reorder_size <- function(x) 
  {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

ggplot(data = champs, aes(reorder_size(Team))) + 
  geom_bar(width = 0.6, fill = c("red","midnightblue","firebrick","skyblue","black","midnightblue"), show.legend = FALSE) +
  xlab("") +
  ylab("Titles") +
  ggtitle("Premier League Titles by Club")

champs %>% count(Team, sort = TRUE)

install.packages("knitr")
library("knitr")

filter(champs, Pts %in% range(Pts)) %>% knitr::kable()


champs$T4Avg <- ddply(filter(megatable, Pos < 5), .(Year), summarise, Avg=round(mean(Pts)))[,2]
champs$GDAvg <- ddply(filter(megatable, Pos < 5), .(Year), summarise, Avg=round(mean(GF-GA)))[,2]
mutate(champs, STR=(Pts-T4Avg)+((GF-GA)-GDAvg)) %>%
  select(-GD, -Pos) %>% 
  arrange(desc(STR)) %>% 
  top_n(10) %>% 
  knitr::kable()

topfour <- megatable %>% filter(Pos < 5 & Year > 1994)
ggplot(data = topfour, aes(reorder_size(Team))) +
  geom_bar(width = 0.75, fill = "midnightblue") +
  xlab("") +
  ylab("Top 4 Finishes") +
  theme(axis.text.x = element_text(angle=90))

rlgtd <- megatable %>% filter(Year > 1994, Pos > 17)
rlgtd %>% count(Team, sort = TRUE) %>% top_n(5)

redemption <- function(team, year) {
  next_year = as.integer(year) + 2
  if (team %in% megatable[megatable$Year == next_year, "Team"]) {
    promoted = 1
  }
  else {
    promoted = 0
  }
  return(promoted)
}
redeemed <- mapply(redemption, rlgtd[,"Team"], rlgtd[, "Year"])
round(sum(redeemed)/nrow(rlgtd)*100, 2)

totgoals <- filter(megatable, Year > 1994)  %>% group_by(Year)  %>% summarise(Tot = sum(GF))
ggplot(data = totgoals, aes(x = as.numeric(Year), y = Tot)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_hline(yintercept = round(mean(totgoals$Tot)), linetype = 2) +
  xlab("Year") +
  ylab("Total Goals") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.9)) +
  scale_x_continuous(breaks = seq(1995,2015)) +
  ggtitle("Total Goals Scored per Season")

teams <- c("Leicester City",
           "Arsenal",
           "Manchester United",
           "Chelsea",
           "Liverpool",
           "Southampton",
           "Tottenham Hotspur",
           "West Ham United")

teamperf <- megatable %>% 
  filter(Year > 1994, Team %in% teams) %>% 
  group_by(Team) %>% 
  summarise(ptsavg=mean(Pts), gfavg=mean(GF)) %>% 
  arrange(Team)

mcavg <- megatable %>% 
  filter(Year > 2010, Team == "Manchester City") %>% 
  group_by(Team) %>% 
  summarise(ptsavg=mean(Pts), gfavg=mean(GF))

teamperf <- bind_rows(teamperf, mcavg)
teamperf <- teamperf[order(teamperf$Team),]

teams <- append(teams, "Manchester City")

epl15 <- megatable %>% 
  filter(Year == 2015, Team %in% teams) %>% 
  select(Year, Team, Pts, GF) %>% 
  arrange(Team)

epl15$PtsAvg <- round(teamperf$ptsavg, 2)
epl15$GFAvg <- round(teamperf$gfavg, 2)
epl15 <- epl15 %>%
  mutate(PtsDiff=Pts-PtsAvg, GFDiff=GF-GFAvg) %>%
  arrange(desc(PtsDiff))

epl15 %>% 
  mutate(PtsPctChng=(Pts-PtsAvg)/Pts*100, GFPctChng=(GF-GFAvg)/GF*100) %>%
  arrange(desc(PtsPctChng)) %>% 
  knitr::kable()


afc <- filter(megatable, Team == "Manchester United")
filter(afc, Pos > 4)

ggplot(data = afc, aes(x = as.numeric(Year), y = GF)) +
  geom_line(size = 2.0, alpha = 0.7, color = "firebrick") +
  geom_point(size = 1.0) +
  xlab("Year") + 
  ylab("Goals Scored") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.9)) +
  scale_x_continuous(breaks = seq(1992,2015)) +
  ggtitle("Manchester United's Goals Scored per Season")

nld <- megatable %>% filter(Team %in% c("Manchester United", "Chelsea"))

ggplot(data = nld, aes(x = as.numeric(Year), y = Pts)) + 
  geom_line(size = 2.0, alpha = 0.7, aes(color = Team)) +
  geom_point(data = subset(nld, Pos == 1), size = 2.0, alpha = 0.7) +
  scale_color_manual(values = c("firebrick","midnightblue")) +
  xlab("Year") +
  ylab("Points") +
  ggtitle("Manchester United vs. Chelsea EPL Comparison")
