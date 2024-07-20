## Graphs

#Load the data
attorneys <-read.csv("/Users/pivaldhingra/Desktop/University courses/Datafest 2023/DataFest 2023 Data For Distribution-2023-04-19/data/attorneys.csv")
attorneytimeentries <-read.csv("/Users/pivaldhingra/Desktop/University courses/Datafest 2023/DataFest 2023 Data For Distribution-2023-04-19/data/attorneytimeentries.csv")
categories <-read.csv("/Users/pivaldhingra/Desktop/University courses/Datafest 2023/DataFest 2023 Data For Distribution-2023-04-19/data/categories.csv")
clients <-read.csv("/Users/pivaldhingra/Desktop/University courses/Datafest 2023/DataFest 2023 Data For Distribution-2023-04-19/data/clients.csv")
questionposts <-read.csv("/Users/pivaldhingra/Desktop/University courses/Datafest 2023/DataFest 2023 Data For Distribution-2023-04-19/data/questionposts.csv")
questions <-read.csv("/Users/pivaldhingra/Desktop/University courses/Datafest 2023/DataFest 2023 Data For Distribution-2023-04-19/data/questions.csv")
statesites <-read.csv("/Users/pivaldhingra/Desktop/University courses/Datafest 2023/DataFest 2023 Data For Distribution-2023-04-19/data/statesites.csv")
subcategories <-read.csv("/Users/pivaldhingra/Desktop/University courses/Datafest 2023/DataFest 2023 Data For Distribution-2023-04-19/data/subcategories.csv")


#m1
library(usmap)
library(ggplot2)
df <- data.frame(table(clients[,"StateAbbr"]))
colnames(df) <- c("state", "values")
df[order(-df$values),]
us_map <- plot_usmap(data = df, values = "values", labels = TRUE) + 
  scale_fill_continuous(name = "Values", low = "white", high = "red") +
  theme_minimal() + # Use a minimal theme
  theme(legend.position = "right", legend.justification = "center") # Position the legend to the right

print(us_map)
#m2



#****
#plot 1
#Clients per state
df <- data.frame(table(clients[,"StateAbbr"]))
colnames(df) <- c("state", "values")
p <- plot_usmap(
  data = df, values = "values", labels = TRUE
) + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Clients", labels = scales::comma
  ) + 
  theme_minimal() +  # Use a minimal theme
  theme(
    legend.position = "right",
    legend.justification = c(1, 0),  # Justify legend to the right
    legend.background = element_blank(),  # Remove legend background
    legend.key = element_blank()  # Remove legend key background
  )

# Adjust the size of the labels if needed
p$layers[[2]]$aes_params$size <- 2.5

# Print the plot
print(p)


#plot 2
#Attorneys per State
df <- data.frame(table(attorneys[,"StateAbbr"]))
colnames(df) <- c("state", "values")
p <- plot_usmap(
  data = df, values = "values", labels = TRUE
) + 
  scale_fill_continuous(
    low = "white", high = "darkgreen", name = "Attorneys", labels = scales::comma
  ) + 
  theme_minimal() +  # Use a minimal theme
  theme(
    legend.position = "right",
    legend.justification = c(1, 0),  # Ensure correct justification
    legend.background = element_blank(),  # Remove legend background
    legend.key = element_blank()  # Remove legend key background
  )

# Adjust the size of the labels if needed
p$layers[[2]]$aes_params$size <- 2.5

# Print the plot
print(p)



#plot 5
#Client Attorney Ratio

clientState <- data.frame(table(clients[,"StateAbbr"]))
colnames(clientState) <- c("state", "values")

attorneyState <- data.frame(table(attorneys[,"StateAbbr"]))
colnames(attorneyState) <- c("state", "values")
attorneyClientState <- merge(clientState,attorneyState,by="state")
colnames(attorneyClientState) <- c("state", "clientCount", "attorneyCount")
ClientAttorneyRatio <- attorneyClientState$clientCount / attorneyClientState$attorneyCount
attorneyClientState <- cbind(attorneyClientState, ClientAttorneyRatio)
attorneyClientState <- attorneyClientState[order(-attorneyClientState$ClientAttorneyRatio),]
head(attorneyClientState)

df <- attorneyClientState[, c("state", "ClientAttorneyRatio")]
colnames(df) <- c("state", "values")
p <- plot_usmap(
  data = df, values = "values", color = "black", labels = TRUE
) + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "Attorney - Client Ratio", labels = scales::comma
  ) + 
  theme_minimal() +  # Use a minimal theme
  theme(
    legend.position = "right",
    legend.justification = "center"  # Adjust legend justification
  )

# Adjust the size of the labels if needed
p$layers[[2]]$aes_params$size <- 2

# Print the plot
print(p)

##
sum_hours <- aggregate(Hours ~ StateAbbr, data = attorneytimeentries, sum)
count_attorneys <- aggregate(AttorneyUno ~ StateAbbr, data = attorneytimeentries, function(x) length(unique(x)))


attorney_sum_hours <- aggregate(Hours ~ AttorneyUno, data = attorneytimeentries, FUN = sum)
attorney_sum_hours[order(-attorney_sum_hours$Hours),] 



# Convert EnteredOnUtc to year using as.POSIXlt
attorneytimeentries$Year <- as.POSIXlt(attorneytimeentries$EnteredOnUtc)$year + 1900

# Calculate total hours spent by each attorney for each year
total_hours_by_attorney_year <- aggregate(Hours ~ Year + StateAbbr, attorneytimeentries, sum)


# Plot total hours by attorney and year
ggplot(total_hours_by_attorney_year, aes(x = Year, y = Hours, color = StateAbbr)) +
  geom_point() + 
  labs(x = "Year", y = "Total hours", title = "Total hours by attorney and year")

####


clients$Age <- ifelse( clients$Age == "NULL", NA, clients$Age)
# Create a dataframe with Age column that has numeric values and NULL values
#my_df <- data.frame(Age = c(21, 34, 17, 43, NA, 27, 18, NA, 39, 28, 12))

# Create bins of width 10
bins <- seq(0, 100, by = 5)
clientAZ <- clients[clients$StateAbbr == "AZ",]
# Use the cut() function to create a factor variable that specifies the bin for each value
age_bins_AZ <- cut(as.integer(clientAZ$Age), breaks = bins, include.lowest = TRUE, right = FALSE)

# Use the table() function to count the frequencies of each bin, including NA values
freq_table <- table(age_bins_AZ, useNA = "ifany")

# Print the frequency table
print(freq_table)

#####

#plot 3
#waiting time
# m1
library(dplyr)
QuestionsNonWaitingdf <- questions
QuestionsNonWaitingdf$TakenOnUtc <- ifelse(QuestionsNonWaitingdf$TakenOnUtc == "NULL", NA, QuestionsNonWaitingdf$TakenOnUtc)
QuestionsNonWaitingdf <- subset(QuestionsNonWaitingdf, !is.na(TakenOnUtc))

QuestionsNonWaitingdf <- mutate(QuestionsNonWaitingdf, waitingUtc = difftime(TakenOnUtc, AskedOnUtc, units = "hours"))

# create a new column for year from the AskedOnUtc column
df <- QuestionsNonWaitingdf %>% mutate(year = lubridate::year(AskedOnUtc))

# calculate the average waiting time per state per year
df_avg <- df %>% 
  group_by(StateAbbr, year) %>% 
  summarise(avg_waiting_time = mean(waitingUtc, na.rm = TRUE))

# calculate maximum waiting time per state
max_waiting_time <- df_avg %>%
  group_by(StateAbbr) %>%
  summarize(max_wait_time = max(as.numeric(avg_waiting_time), na.rm = TRUE))

# filter out StateAbbr with max waiting time < 500
df_filtered <- df_avg %>%
  inner_join(max_waiting_time, by = "StateAbbr") %>%
  filter(max_wait_time >= 500) %>%
  select(-max_wait_time)


# plot the results
ggplot(df_filtered, aes(x = year, y = avg_waiting_time, color = StateAbbr)) +
  geom_point() +geom_line() +
  ggtitle("Top 10 Average Waiting Time for Questions per State per Year") +
  xlab("Year") + scale_x_continuous(breaks = seq(2012, 2022, 1), labels = seq(2012, 2022, 1)) +
  ylab("Average Waiting Time (in seconds)") +
  theme_bw()

######
#plot 4
#cases not selected count per state per year

library(dplyr)
QuestionsWaitingdf <- questions
QuestionsWaitingdf$TakenOnUtc <- ifelse(QuestionsWaitingdf$TakenOnUtc == "NULL", NA, QuestionsWaitingdf$TakenOnUtc)
QuestionsWaitingdf <- subset(QuestionsWaitingdf, is.na(TakenOnUtc))


# create a new column for year from the AskedOnUtc column
df <- QuestionsWaitingdf %>% mutate(year = lubridate::year(AskedOnUtc))

# Aggregate the data by StateAbbr, year, and get the count of QuestionUno
df_agg <- aggregate(QuestionUno ~ StateAbbr + year, data = df, FUN = length)

# calculate maximum waiting time per state
max_unans_q_df <- df_agg %>%
  group_by(StateAbbr) %>%
  summarize(max_QuestionUno = max(QuestionUno, na.rm = TRUE))

# filter out StateAbbr with max unanswered questions count  < 1000
df_agg <- df_agg %>%
  inner_join(max_unans_q_df, by = "StateAbbr") %>%
  filter(max_QuestionUno >= 1000) %>%
  select(-max_QuestionUno)

# Create a bar plot
ggplot(df_agg, aes(x = year, y = QuestionUno, color = StateAbbr)) +
  geom_point() +geom_line() +
  ggtitle("Top Unaswered question count per State per Year") +
  xlab("Year") + scale_x_continuous(breaks = seq(2012, 2022, 1), labels = seq(2012, 2022, 1)) +
  ylab("Unanswered Question Count") +
  theme_bw()

## Plots
library(tidyverse)
library(ggplot2)
library(dplyr)

## load data
attorneys <- read.csv(file = "/Users/pivaldhingra/Desktop/University courses/Datafest 2023/DataFest 2023 Data For Distribution-2023-04-19/data/attorneys.csv")
attorney_time_entries <- read.csv(file = "/Users/pivaldhingra/Desktop/University courses/Datafest 2023/DataFest 2023 Data For Distribution-2023-04-19/data/attorneytimeentries.csv")
categories <- read.csv(file = "/Users/pivaldhingra/Desktop/University courses/Datafest 2023/DataFest 2023 Data For Distribution-2023-04-19/data/categories.csv")
clients <- read.csv(file = "/Users/pivaldhingra/Desktop/University courses/Datafest 2023/DataFest 2023 Data For Distribution-2023-04-19/data/clients.csv")
questionposts <- read_csv(file = "/Users/pivaldhingra/Desktop/University courses/Datafest 2023/DataFest 2023 Data For Distribution-2023-04-19/data/questionposts.csv")
questions <- read.csv(file = "/Users/pivaldhingra/Desktop/University courses/Datafest 2023/DataFest 2023 Data For Distribution-2023-04-19/data/questions.csv")
statesites <- read.csv(file = "/Users/pivaldhingra/Desktop/University courses/Datafest 2023/DataFest 2023 Data For Distribution-2023-04-19/data/statesites.csv")
subcategories <- read.csv(file = "/Users/pivaldhingra/Desktop/University courses/Datafest 2023/DataFest 2023 Data For Distribution-2023-04-19/data/subcategories.csv")

## clean clients table
head(clients)
# drop irrelevant information
clients <- subset(clients, select = -c(County, StateName,
                                       PostalCode))
# remove NULL rows
clients[clients == "NULL"] = NA
clients <- clients[rowSums(is.na(clients)) < 9, ]
# numeric type
clients$AnnualIncome <- as.numeric(clients$AnnualIncome)

##  clean questions table
head(questions)
questions[questions == "NULL"] = NA

### VISUALIZATION

## Frequency of categories for each state
# Plot 1: Stacked Bar Graph
# group the data by state and category, count the number of occurrences
questions_subset <- subset(questions, StateAbbr %in% c("IN", "AZ", "GA"))
count <- questions_subset %>% group_by(StateAbbr, Category) %>% summarise(count = n())
# plot
ggplot(count, aes(x = count, y = StateAbbr, fill = Category)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Frequency", y = "State", fill = "Category", 
       title = "Frequency of Categories by State")

## Income Data for each State
clients_subset <- subset(clients, StateAbbr %in% c("IN", "AZ", "GA"))
#clients_subset <- clients_subset[clients_subset$AnnualIncome < 200000,]
ggplot(clients_subset, aes(x=StateAbbr, y=as.numeric(AnnualIncome), fill=StateAbbr)) + 
  geom_boxplot() +
  scale_fill_manual(values = rainbow(length(unique(clients_subset$StateAbbr)))) +
  xlab("State") + ylab("Annual Income") +
  ggtitle("Average Annual Income Distribution by State")

options(scipen=999)

## Waiting_Times

library(dplyr)
library(lubridate)
questions <- read.csv("/Users/pivaldhingra/Desktop/University courses/Datafest 2023/DataFest 2023 Data For Distribution-2023-04-19/data/questions.csv")
data <- data.frame(questions)
df <- select(data, "StateAbbr", "AskedOnUtc", "TakenOnUtc")


df$AskedOnUtc <- as.POSIXct(df$AskedOnUtc, format = "%Y-%m-%d %H:%M:%OS" )
df$TakenOnUtc <- as.POSIXct(df$TakenOnUtc, format = "%Y-%m-%d %H:%M:%OS")

df$Year <- format(df$AskedOnUtc, "%Y")

df_filtered <- df %>% 
  filter(!is.na(TakenOnUtc)) %>% 
  filter(Year %in% c("2016", "2017", "2018", "2019", "2020", "2021", "2022"))

# Calculate the waiting time for each question
df_filtered$WaitingTime <- difftime(df_filtered$TakenOnUtc, df_filtered$AskedOnUtc, units = "mins")

# Group the data by state and year, and calculate the average waiting time for each group
df_summary <- df_filtered %>% 
  group_by(StateAbbr, Year) %>% 
  summarize(AvgWaitingTime = mean(WaitingTime))

df_2016 <- df_summary %>% 
  filter(Year == 2016) %>% 
  select(StateAbbr, AvgWaitingTime) %>% 
  group_by(StateAbbr) %>% 
  summarise(avg_waiting_time = mean(AvgWaitingTime, na.rm = TRUE)) %>% 
  arrange(desc(avg_waiting_time))

top_5_states <- df_2016 %>% top_n(5, avg_waiting_time)

print(top_5_states)

df_2017 <- df_summary %>% 
  filter(Year == 2017) %>% 
  select(StateAbbr, AvgWaitingTime) %>% 
  group_by(StateAbbr) %>% 
  summarise(avg_waiting_time = mean(AvgWaitingTime, na.rm = TRUE)) %>% 
  arrange(desc(avg_waiting_time))

top_5_states <- df_2017 %>% top_n(5, avg_waiting_time)

print(top_5_states)

df_2018 <- df_summary %>% 
  filter(Year == 2018) %>% 
  select(StateAbbr, AvgWaitingTime) %>% 
  group_by(StateAbbr) %>% 
  summarise(avg_waiting_time = mean(AvgWaitingTime, na.rm = TRUE)) %>% 
  arrange(desc(avg_waiting_time))

top_5_states <- df_2018 %>% top_n(5, avg_waiting_time)

print(top_5_states)

df_2019 <- df_summary %>% 
  filter(Year == 2019) %>% 
  select(StateAbbr, AvgWaitingTime) %>% 
  group_by(StateAbbr) %>% 
  summarise(avg_waiting_time = mean(AvgWaitingTime, na.rm = TRUE)) %>% 
  arrange(desc(avg_waiting_time))

top_5_states <- df_2019 %>% top_n(5, avg_waiting_time)

print(top_5_states)

df_2020 <- df_summary %>% 
  filter(Year == 2020) %>% 
  select(StateAbbr, AvgWaitingTime) %>% 
  group_by(StateAbbr) %>% 
  summarise(avg_waiting_time = mean(AvgWaitingTime, na.rm = TRUE)) %>% 
  arrange(desc(avg_waiting_time))

top_5_states <- df_2020 %>% top_n(5, avg_waiting_time)

print(top_5_states)

df_2021 <- df_summary %>% 
  filter(Year == 2021) %>% 
  select(StateAbbr, AvgWaitingTime) %>% 
  group_by(StateAbbr) %>% 
  summarise(avg_waiting_time = mean(AvgWaitingTime, na.rm = TRUE)) %>% 
  arrange(desc(avg_waiting_time))

top_5_states <- df_2021 %>% top_n(5, avg_waiting_time)

print(top_5_states)

df_2022 <- df_summary %>% 
  filter(Year == 2022) %>% 
  select(StateAbbr, AvgWaitingTime) %>% 
  group_by(StateAbbr) %>% 
  summarise(avg_waiting_time = mean(AvgWaitingTime, na.rm = TRUE)) %>% 
  arrange(desc(avg_waiting_time))

top_5_states <- df_2022 %>% top_n(5, avg_waiting_time)

print(top_5_states)




