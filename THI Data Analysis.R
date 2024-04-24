library(readxl)
library(dplyr)  
library(ggplot2)
library(lubridate)

raw_data <- read_excel("THI Report Analyst Interview Data (1).xlsx", sheet = 1)

data <- read_excel("THI Report Analyst Interview Data (1).xlsx", sheet = 2)

library(Hmisc)

colSums(is.na(data)) # Only blanks in comment column

dim(data)

str(data)

describe(data)

summary(data)

library(tidyverse)



# Employee Satisfaction Overall

# Distribution of Score Across whole chain
ggplot(data, aes(x = Score)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Score", x = "Score", y = "Frequency")

# Average Score by Theme
ggplot(satisfaction_by_theme, aes(x = reorder(Theme, Score), y = Score)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(Score, 2)), vjust = -0.5, size = 3) +  
  labs(title = "Employee Satisfaction by Theme", x = "Theme", y = "Mean Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mean(data$Score) # 8.03, but let's get this to a higher score!

# By Gender
satisfaction_by_gender <- aggregate(Score ~ Gender, data = data, FUN = mean)

# By Departments
satisfaction_by_department1 <- aggregate(Score ~ `Department 1`, data = data, FUN = mean)

satisfaction_by_department2 <- aggregate(Score ~ `Department 2`, data = data, FUN = mean)



# By Start Dates
satisfaction_by_start_dates <- aggregate(Score ~ `Start date`, data=data, FUN = mean)

# By Location
satisfaction_by_location <- aggregate(Score ~ Location, data = data, FUN = mean)

# By Country
satisfaction_by_country <- aggregate(Score ~ Country, data = data, FUN = mean)




ggplot(satisfaction_by_gender, aes(x = Gender, y = Score)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(Score, 2)), vjust = -0.5, size = 3) +
  labs(title = "Employee Satisfaction by Gender", x = "Gender", y = "Mean Score")


# Bar plot for satisfaction by department
ggplot(satisfaction_by_department1, aes(x = reorder(`Department 1`, Score), y = Score)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(Score, 2)), vjust = -0.5, size = 3) + 
  labs(title = "Employee Satisfaction by Department", x = "Department", y = "Mean Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Bar plot for satisfaction by location
ggplot(satisfaction_by_location, aes(x = reorder(Location, Score), y = Score)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(Score, 2)), vjust = -0.5, size = 3) + 
  labs(title = "Employee Satisfaction by Location", x = "Location", y = "Mean Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# # We have found some incorrect rows
# # Identify rows where Country is Belgium and Location is Italy
# incorrect_rows <- data[data$Location == "Germany" & data$Location == "Italy", ]
# 
# # Print rows to inspect
# print(incorrect_rows)
# 
# # Correct the inconsistencies
# # For example, if the Location should be in Italy, update the Country to Italy
# data$Location[data$Country == "Belgium" & data$Location == "Italy"] <- "Brussels"
# 
# data$Location[data$Country == "Germany" & data$Location == "Germany"] <- "Munich"
# 
# 
# 
# satisfaction_by_theme <- aggregate(Score ~ Theme, data = data, FUN = mean)
# 
# 
# 
# unique(data$Location)
# 
# 
# # Check for rows where Country is Italy and Location is not an Italian city
# incorrect_italy_rows <- data[data$Country == "Italy" & !data$Location %in% c("Rome", "Sciacca", "Palermo", "Savelletri", "Florence"), ]
# 
# # Print rows to inspect
# print(incorrect_italy_rows)
# 
# incorrect_belgium_rows <- data[data$Country == "Belgium" & !data$Location == "Brussels", ]
# 
# print(incorrect_belgium_rows)
# 
# # Check for rows where Country is Germany and Location is not a German city
# incorrect_germany_rows <- data[data$Country == "Germany" & !data$Location %in% c("Berlin", "Munich"), ]
# 
# # Print rows to inspect
# print(incorrect_germany_rows)
# 



# Due to incorrect data, we're not going to look at location. Instead
# We will focus on Country

ggplot(satisfaction_by_country, aes(x = reorder(Country, Score), y = Score)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(Score, 2)), vjust = -0.5, size = 3) + 
  labs(title = "Employee Satisfaction by Country", x = "Country", y = "Mean Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Bar plot for satisfaction by department 2
ggplot(satisfaction_by_department2, aes(x = reorder(`Department 2`, Score), y = Score)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(Score, 2)), vjust = -0.5, size = 3) + 
  labs(title = "Employee Satisfaction by Department 2", x = "Department 2", y = "Mean Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot for satisfaction by start dates
ggplot(satisfaction_by_start_dates, aes(x= reorder(`Start date`, Score), y= Score)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = round(Score, 2)), vjust = -0.5, size = 3) + 
  labs(title = "Emplyee Satisfaction by Start Date", x= "Start Date", y = "Average Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Group the above. Look at New Hires (Anyone who started in 2023) and Existing Employees. 
# Remove the blanks too

# Gender composition
gender_composition <- data %>%
  group_by(Gender) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)



age_composition <- data %>%
  group_by(`Birth date`) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Country composition
location_composition <- data %>%
  group_by(Country) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Department 1 Composition
department_composition <- data %>%
  group_by(`Department 1`) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) *100)

# Department 2 Composition
department_composition2 <- data %>%
  group_by(`Department 2`) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) *100)

# Now you can print or visualize these compositions
print(gender_composition)
print(age_composition)
print(location_composition)
print(department_composition)
print(department_composition2)


ggplot(gender_composition, aes(x=reorder(Gender, count), y = percentage)) +
  geom_bar(stat= "identity", fill = "blue") +
  geom_text(aes(label = round(percentage, 2)), vjust = -0.5, size = 3) + 
  labs(title = "Respondants by Gender Split", x= "Gender", y = "Percentage") 


ggplot(age_composition, aes(x=reorder(`Birth date`, count), y = percentage)) +
  geom_bar(stat= "identity", fill = "blue") +
  geom_text(aes(label = round(percentage, 2)), vjust = -0.5, size = 3) + 
  labs(title = "Respondants by Birth Range", x= "Birth Range", y = "Percentage")


ggplot(location_composition, aes(x=reorder(Country, count), y = percentage)) +
  geom_bar(stat= "identity", fill = "blue") +
  geom_text(aes(label = round(percentage, 2)), vjust = -0.5, size = 3) + 
  labs(title = "Respondants by Country", x= "Country", y = "Percentage") 



### Departmental Analysis


ggplot(department_composition, aes(x=reorder(`Department 1`, count), y = percentage)) +
  geom_bar(stat= "identity", fill = "blue") +
  geom_text(aes(label = round(percentage, 2)), vjust = -0.5, size = 3) + 
  labs(title = "Respondants by Department", x= "Department 1", y = "Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(department_composition2, aes(x=reorder(`Department 2`, count), y = percentage)) +
  geom_bar(stat= "identity", fill = "blue") +
  geom_text(aes(label = round(percentage, 2)), vjust = -0.5, size = 3) + 
  labs(title = "Respondants by Department 2", x= "Department 2", y = "Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot for satisfaction by department 1
ggplot(satisfaction_by_department1, aes(x = reorder(`Department 1`, Score), y = Score)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(Score, 2)), vjust = -0.5, size = 3) + 
  labs(title = "Employee Satisfaction by Department", x = "Department", y = "Mean Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Now you can print or visualise these comparisons
print(satisfaction_by_department1)

print(satisfaction_by_department2)

worst_departments <- c("Other Operating", "Apprentice", "F&B")
departments_filtered_data <- data[data$`Department 1` %in% worst_departments, ]



satisfaction_by_theme_department <- aggregate(Score ~ Theme + `Department 1`, data = departments_filtered_data, FUN = mean)


# Let's dive into the worst performing departments
ggplot(satisfaction_by_theme_department, aes(x = reorder(Theme, Score), y = Score, fill = `Department 1`)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Employee Satisfaction by Theme in Worst Performing Departments",
       x = "Theme", y = "Mean Score", fill = "Department") +
  geom_text(aes(label = round(Score, 2)), position = position_dodge(width = 0.9), vjust = -0.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(satisfaction_by_theme_department)


# Filter for Department 2 based on worst-performing departments in Department 1
department2_data <- data[data$`Department 1` %in% worst_departments & data$`Department 2` != "-", ]

satisfaction_by_theme_department2 <- aggregate(Score ~ Theme + `Department 2`, data = department2_data, FUN = mean)



# Plotting
ggplot(satisfaction_by_theme_department2, aes(x = reorder(Theme, Score), y = Score, fill=`Department 2`)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Employee Satisfaction by Theme in Department 2 for Worst Performing Departments in Department 1",
       x = "Theme", y = "Mean Score", fill = "Department") +
  geom_text(aes(label = round(Score, 2)), position = position_dodge(width = 0.9), vjust = -0.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")




print(department2_data)






# # Other Operating Department:
# #   Improve satisfaction in the areas of Personal Growth and Safety.
# # Focus on enhancing acknowledgment across the department.
# 
# # F&B Department:
# #   Address issues related to Personal Growth.
# # Work on improving satisfaction levels regarding acknowledgment and Safety.
# 
# # Apprentice Department:
# #   Enhance satisfaction levels in Safety, Meaning & Purpose, and Freedom.
# # Place emphasis on improving acknowledgment within the department.


## New Hire Analysis (Anyone without a start date in 2023 will be an existing employee)
# Remove rows with "-" in Start date column
new_hire_data <- data[data$`Start date` != "-", ]

new_hire_data$Employee_Type <- ifelse(new_hire_data$`Start date` == "2023", "New Hire", "Existing Employee")

# Check unique values of Employee_Type
unique(new_hire_data$Employee_Type)
# Calculate mean satisfaction score for new hires and existing employees
satisfaction_by_employee_type <- aggregate(Score ~ Employee_Type, data = new_hire_data, FUN = mean)

# Print the result
print(satisfaction_by_employee_type)


# Function to group start dates
group_start_date <- function(start_date) {
  if (grepl("198", start_date)) {
    return("1980s")
  } else if (grepl("199", start_date)) {
    return("1990s")
  } else if (grepl("200", start_date)) {
    return("2000s")
  } else if (grepl("201", start_date)) {
    return("2010s")
  } else if (grepl("202", start_date)) {
    return("2020s")
  } else {
    return("Other")
  }
}

# Apply the grouping function to the Start date column
new_hire_data$Start_Decade <- sapply(new_hire_data$`Start date`, group_start_date)
data$Start_Decade <- sapply(data$`Start date`, group_start_date)
# Calculate satisfaction scores by start decade
satisfaction_by_start_decade <- aggregate(Score ~ Start_Decade, data = new_hire_data, FUN = mean)

# Print the result
print(satisfaction_by_start_decade)



test <- new_hire_data %>%
  group_by(Start_Decade) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

print(test)

ggplot(test, aes(x = Start_Decade, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Employee Respondents by Start Decade", x = "Start Decade", y = "Number of Responses") +
  theme_minimal() +
  geom_text(aes(label = count), vjust = -0.5, size = 3.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Visualise Satisfaction across start decades
ggplot(satisfaction_by_start_decade, aes(x = reorder(Start_Decade, Score), y = Score)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Employee Satisfaction by Start Decade", x = "Start Decade", y = "Mean Score") +
  theme_minimal() +
  geom_text(aes(label = round(Score, 2)), vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter data for the 1980s and 2010s
low_satisfaction_data <- new_hire_data[new_hire_data$Start_Decade %in% c("1980s", "2010s"), ]

print(low_satisfaction_data)
# Calculate average satisfaction score for each theme
satisfaction_by_theme_decade <- aggregate(Score ~ Theme + Start_Decade, data = low_satisfaction_data, FUN = mean)

# Print the result
print(satisfaction_by_theme_decade)

# Create bar plot
ggplot(satisfaction_by_theme_decade, aes(x = reorder(Theme, Score), y = Score, fill = Start_Decade)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Score, 2)), position = position_dodge(width = 0.9), vjust = -0.5) + 
  labs(title = "Average Satisfaction Score by Theme and Start Decade",
       x = "Theme", y = "Average Score", fill = "Start Decade") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("1980s" = "skyblue", "2010s" = "salmon"))

## Geographical Analysis

# Group the data by country and calculate mean satisfaction score
satisfaction_by_country <- aggregate(Score ~ Country, data = data, FUN = mean)

# Sort the data by mean satisfaction score in descending order
satisfaction_by_country <- satisfaction_by_country[order(-satisfaction_by_country$Score), ]

# Print the result
print(satisfaction_by_country)

# Visualise the results
ggplot(satisfaction_by_country, aes(x = reorder(Country, Score), y = Score)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(Score, 2)), vjust = -0.5, size = 3) + 
  labs(title = "Employee Satisfaction by Country", x = "Country", y = "Mean Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Let's look at worst countries
worst_countries <- c("Belgium", "Italy")
countries_filtered_data <- data[data$Country %in% worst_countries, ]

satisfaction_by_theme_country <- aggregate(Score ~ Theme + Country, data = countries_filtered_data, FUN = mean)

ggplot(satisfaction_by_theme_country, aes(x = reorder(Theme, Score), y = Score, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Employee Satisfaction by Theme in Worst Performing Countries",
       x = "Theme", y = "Mean Score", fill = "Country") +
  geom_text(aes(label = round(Score, 2)), position = position_dodge(width = 0.9), vjust = -0.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")


print(satisfaction_by_theme_country)

# Based on the output, here are some action points for both Belgium and Italy to improve employee satisfaction:
#   
#   Enhance Acknowledgement: Implement recognition programs, appreciate employees' efforts publicly, and provide regular feedback to recognize their contributions.
# 
# Foster Personal Growth: Offer training and development opportunities, career advancement programs, and mentorship initiatives to support employees' professional growth.
# 
# Promote Freedom: Encourage autonomy and decision-making authority within roles, provide flexibility in work arrangements where feasible, and create an open environment for sharing ideas and feedback.
# 
# Ensure Safety: Prioritize occupational health and safety measures, conduct regular safety training sessions, and maintain a safe and supportive work environment to address employees' safety concerns.
# 
# By focusing on these areas, both Belgium and Italy can work towards improving employee satisfaction and creating a more positive work environment conducive to productivity and employee well-being.
# 

### Clustering 

library(cluster)

# Convert Country into numerical format
data$Country_numeric <- as.integer(factor(data$Country))

# Convert Start_Decade into numerical format
data$Start_Decade_numeric <- as.integer(factor(data$Start_Decade))

# Convert Department 1 into numerical format
data$Department_1_numeric <- as.integer(factor(data$`Department 1`))

# Show the updated data
head(data[, c("Country_numeric", "Start_Decade_numeric", "Department_1_numeric")])



# Select the features you want to use for clustering
features <- data[, c("Score", "Start_Decade_numeric", "Country_numeric")]

# Standardize the features (optional but recommended)
scaled_features <- scale(features)

# Determine the number of clusters (k)
k <- 3

# Perform k-means clustering
kmeans_result <- kmeans(scaled_features, centers = k)

# Print the cluster centers
print(kmeans_result$centers)

# Assign cluster labels to the original data
cluster_labels <- kmeans_result$cluster

# Add the cluster labels to the original dataframe
data$cluster <- cluster_labels

# Visualize the clusters (if your data is 2D)
plot(features, col = cluster_labels, main = "K-means Clustering")
points(kmeans_result$centers, col = 1:k, pch = 8, cex = 2)
