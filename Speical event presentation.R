library(tidyverse)
library(igraph)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(leaflet)
library(readxl)

### total fees for each year ###
#read one of the years csv files in the "audited spreadsheets" folder
selected_data <- read.csv("2019.csv",
                          stringsAsFactors = FALSE, header = TRUE)

# Calculate different fee types for each Facility Type (events)
fee_summary <- selected_data %>%
  group_by(Facility.Type) %>%
  summarize(
    Total_Application_Fee = sum(`Total.application.fee`),
    Total_Fire_Department_Fee = sum(`Total.fire.department.fee`),
    Total_Police_Security_Fee = sum(`Police.security`),
    Total_Traffic_Fee = sum(`Total.traffic.fee`),
    Total_General_Equipment_Fee = sum(`General.Equipment.fee`),
    Total_Electrical_Fee = sum(`Electrical.Fee`),
    Total_Solid_Waste_Fee = sum(`Solid.waste`)
  )

write.csv(fee_summary, "2019_total_fees_by_event_type.csv", row.names = FALSE)

### heat map for event type by month 2019 - 2022 ###
#heatmap for months, read the 4 years in the "audited spreadsheet" folder one by one
selected_data <- read.csv("2019.csv",
                          stringsAsFactors = FALSE, header = TRUE)

selected_data$Start.Date <- as.Date(selected_data$Start.Date, format = "%m/%d/%y %H:%M")

# Extract the month from 'Start Date'
selected_data$Month <- month(selected_data$Start.Date)

# Group by Month and Facility Type, then count occurrences of each event type
result2019 <- selected_data %>%
  group_by(Month, Facility.Type) %>%
  summarize(Event_Count = n())

#do the same for the 3 other years, read each year's csv file in first
result2020/21/22

#add year to the respectively dataframe
result2019$Year <- 2019
result2020$Year <- 2020
result2021$Year <- 2021
result2022$Year <- 2022

combined_heatmap <- rbind(result2019, result2020,result2021,result2022)

write.csv(combined_heatmap, "4 year events_by_month.csv", row.names = FALSE)

result2022$Month <- factor(result2022$Month, levels = 1:12, labels = month.abb[1:12])
#heatmap for 2022
ggplot(result2022, aes(x = Month, y = Facility.Type, fill = Event_Count)) +
  geom_tile(color = "white") +
  labs(x = "Month", y = "Event Type", title = "2022 Heatmap of Event Counts by Month and Event Type") +
  scale_x_discrete(labels = month.name[1:12]) +
  scale_fill_distiller(palette = "GnBu",trans = "reverse")+  # You can use other color scales (e.g., scale_fill_gradient) based on your preference
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_heatmap$Month <- factor(combined_heatmap$Month, levels = 1:12, labels = month.name[1:12])

#4 year map
ggplot(combined_heatmap, aes(x = Month, y = Facility.Type, fill = Event_Count)) +
  geom_tile(color = "white") +
  labs(title = "Heatmap of Event Counts by Month 2019 - 2022",
       x = "Month",
       y = "Event Type")+
  facet_grid(rows = vars(Year)) +
  scale_x_discrete(labels = month.name[1:12])+
  scale_fill_distiller(palette = "GnBu",trans = "reverse") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_classic()

### total fees trend line graph 2019 - 2022 ###
trend_data <- data.frame(year = c(2019,2020,2021,2022),
                       Total = c(808135.97,	249106.75,	70668,	368162.98),
                       Application.fees = c(86702.5,4682.5,30210,69878),
                       Fire.dept = c(69985,	35250,	16410,	82936.11),
                       BPD = c(544243.75,	164126,	6849,	156443.72))

data_long <- melt(trend_data, id = "year")
trend_plot <- ggplot(data_long,			
                   aes(x = year,
                       y = value,
                       color = variable)) + 
  geom_line() + 
  theme_minimal()+
  scale_y_continuous(labels = scales::comma)+
  labs(y= "Fees", x = "Year") 
trend_plot


### fee by zip code for each year ###
#do the following for each year's csv
data <- read.csv("2022.csv", header = TRUE, stringsAsFactors = FALSE)

# Group the data by Postal Code and calculate the sum of Event Fees Total
event_cost_by_zipcode <- data %>%
  group_by(`Zipcode`) %>%
  summarize(Total_Event_Cost = sum(`Total.with.system.fee`, na.rm = TRUE))

# Print the result
print(event_cost_by_zipcode)

bar_plot <- ggplot(event_cost_by_zipcode, aes(x = `Zipcode`, y = Total_Event_Cost)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Event Cost by Zipcode", x = "Zipcode", y = "Total Event Cost") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the bar plot
print(bar_plot)

# Convert From: Postal Code to character to ensure correct labeling
event_cost_by_zipcode$`Zipcode` <- as.character(event_cost_by_zipcode$`Zipcode`)

# Remove NA values
event_cost_by_zipcode <- na.omit(event_cost_by_zipcode)

# Sort the dataset by Total_Event_Cost in descending order
sorted_data <- event_cost_by_zipcode %>%
  arrange(desc(Total_Event_Cost))

# Create a bar plot with sorted x-axis labels
bar_plot <- ggplot(sorted_data, aes(x = factor(`Zipcode`, levels = sorted_data$`Zipcode`), y = Total_Event_Cost)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Event Cost by Zip Code - 2021", x = "Zip Code", y = "Total Event Cost") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the bar plot
print(bar_plot)

# do the above bar graph preparations for all 4 years
permits2019 <- read.csv("2019.csv", stringsAsFactors = FALSE, header = TRUE)
permits2020 <- read.csv("2020.csv", stringsAsFactors = F, header = T)
permits2021<- read.csv("2021.csv", stringsAsFactors = F, header = T)
permits2022 <- read.csv("2022.csv", stringsAsFactors = F, header = T)

#calculate total fee by zipcode
total_fee_by_zipcode <- permits2022 %>%
  group_by(Zipcode) %>%
  summarise(Total_Fee = sum(Total_with_system_fee))
total_fee_by_zipcode <- na.omit(total_fee_by_zipcode)
View(total_fee_by_zipcode)
write.csv(total_fee_by_zipcode,"2022_fees_by_zipcode.csv")

#calculate total event by zip code
total_events_by_zipcode <- permits2022 %>%
  group_by(Zipcode) %>%
  summarise(Frequency = n())
total_events_by_zipcode <- na.omit(total_events_by_zipcode)
write.csv(total_events_by_zipcode,"2022_events_by_zipcode.csv")

combined_data <- rbind(permits2019, permits2020, permits2021, permits2022)

combined_data$Zipcode <- as.character(combined_data$Zipcode)

# Remove NA values
combined_data <- na.omit(combined_data)

# Sort the dataset by Event_Fees_Total in descending order
sorted_data <- combined_data %>%
  arrange(desc(Total_with_system_fee))

sorted_data$Zipcode <- as.character(sorted_data$Zipcode)

# Create a bar plot with facet_wrap
facet_plot <- ggplot(sorted_data, aes(x = Zipcode, y = Total_with_system_fee)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Event Fees Total by Zip Code", x = "Zip Code", y = "Event Fees Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)+
  facet_wrap(~ Year, ncol = 1)

# Print the facet plot
print(facet_plot)


### free vs. charged ###

# Create a data frame from the data
data <- data.frame(
  Year = c("2019", "2020", "2021", "2022"),
  Charged = c(623, 150, 193, 327),
  Free = c(202, 116, 90, 49)
)

# Create a bar plot
bar_plot <- ggplot(data, aes(x = Year)) +
  geom_bar(aes(y = Charged, fill = "Charged"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Free, fill = "Free"), stat = "identity", position = "dodge") +
  geom_text(aes(y = Charged, label = Charged), vjust = -0.5, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = Free, label = Free), vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(title = "Bar Plot of Charged vs Free",
       x = "Year",
       y = "Count") +
  scale_fill_manual(values = c("Charged" = "lightblue", "Free" = "orange")) +
  theme_minimal()

# Print the bar plot
print(bar_plot)
