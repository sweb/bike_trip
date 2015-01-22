require(dplyr)
require(lubridate)
require(ggplot2)

setwd("C:/Users/florian.mueller/Dropbox/data_analysis/bike_trip")

raw.trip_data <- read.csv("bike_trip_data.csv", header=TRUE, sep=",")

ana.trip_data <- mutate(raw.trip_data, 
                          Start.Date = mdy_hm(Start.Date), 
                          End.Date = mdy_hm(End.Date))

#Exercise 1
ana.trip_data %>% 
  group_by(Bike..) %>% 
  summarize(total = sum(Duration)) %>% 
  ungroup() %>% 
  summarize(mean(total)) / 60

#Exercise 2
head(ana.trip_data %>% mutate(curr_day = floor_date(Start.Date, "day")) %>%
       group_by(curr_day) %>% summarize(frequency = n()) %>% arrange(desc(frequency)),1)

head(ana.trip_data %>% mutate(curr_day = floor_date(End.Date, "day")) %>%
  group_by(curr_day) %>% summarize(frequency = n()) %>% arrange(desc(frequency)),1)

#Exercise 3
#transactions
ana.transactions <- rbind(ana.trip_data %>%
        mutate(Event.Date = Start.Date, Event.Terminal = Start.Terminal, Count = -1) %>% 
        select(Event.Date, Event.Terminal, Count), 
        ana.trip_data %>% 
        mutate(Event.Date = End.Date, Event.Terminal = End.Terminal, Count = 1) %>% 
        select(Event.Date, Event.Terminal, Count)) %>% 
  arrange(Event.Date)

#ana.transactions <- ana.transactions %>% 
#  arrange(Event.Date)

ana.terminals <- 
  data.frame(cbind(Terminal = unique(ana.transactions[,"Event.Terminal"]), 
                   Count = 30))
ana.collectedEvents <- 
  data.frame(Event.Date = floor_date(ana.transactions[1,"Event.Date"], "day"), Count = 30)

for (i in 1:nrow(ana.transactions)) {
  tmp_trans <- ana.transactions[i,]
  tmp_terminal_count <- ana.terminals %>% 
    filter(Terminal == tmp_trans[["Event.Terminal"]])
  tmp_terminal_count$Count <- tmp_terminal_count$Count + tmp_trans["Count"]
  
  isIdOfCurrentTerminal <- ana.terminals$Terminal == tmp_trans[["Event.Terminal"]]
  ana.terminals$Count[isIdOfCurrentTerminal] <- tmp_terminal_count$Count
  
  if (tmp_trans$Event.Terminal == 54) {
    tmp_df <- data.frame(Event.Date = tmp_trans$Event.Date, Count =tmp_terminal_count$Count)
    ana.collectedEvents <- rbind(ana.collectedEvents, tmp_df)
  }
  
  
  if (tmp_terminal_count$Count == 0) {
    print("Zero reached!")
    print(tmp_trans)
    break
  }
}

ggplot(ana.collectedEvents, aes(x=Event.Date, y=Count)) +
  geom_point(shape=3) 

summary(raw.trip_data)