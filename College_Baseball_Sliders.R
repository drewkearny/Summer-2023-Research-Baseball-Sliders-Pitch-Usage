# Importing statistics on individual pitchers Slider, with every pitcher
# in the data set having thrown at least 100 sliders in the season
# Reading in all the sheets from the excel workbook for 2021, 2022, 2023

# 2023 data
Slider_data_23 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Summer_Research\\Slider Data.xlsx",
                                            sheet = 1)
Batted_ball_23 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Summer_Research\\Slider Data.xlsx",
                                            sheet = 2)
# 2022 data
Slider_data_22 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Summer_Research\\Slider Data.xlsx",
                                    sheet = 3)
Batted_ball_22 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Summer_Research\\Slider Data.xlsx",
                                    sheet = 4)
# 2021 data
Slider_data_21 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Summer_Research\\Slider Data.xlsx",
                                    sheet = 5)
Batted_ball_21 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Summer_Research\\Slider Data.xlsx",
                                    sheet = 6)

# Merge the batted ball and slider data from each season together
# so that there is one data frame per year

# 2023 data frame
slider_statistics_23 <- merge.data.frame(Slider_data_23, Batted_ball_23,
                                        by = c("playerId", "abbrevName",
                                               "playerFullName", "player"))
# 2022 data frame
slider_statistics_22 = merge.data.frame(Slider_data_22, Batted_ball_22,
                                        by = c("playerId", "abbrevName",
                                               "playerFullName", "player"))
# 2021 data frame
slider_statistics_21 = merge.data.frame(Slider_data_21, Batted_ball_21,
                                        by = c("playerId", "abbrevName",
                                               "playerFullName", "player"))

# Remove Switch Pitchers and any pitcher without enough data
slider_statistics_23 <- slider_statistics_23[-c(307,242),]
slider_statistics_22 <- slider_statistics_22[-c(101,317,383,107,122),]
slider_statistics_21 <- slider_statistics_21[-c(156,118,137,176,263,281),]

# Install dplyr package to use pipes

library(dplyr)

# Data Cleaning
# Remove Duplicate and Unnecessary Data from the data frames
# Also Remove the player names for permission reasons

# Negate the selection of the columns that need to be removed for
# each year to remove the variables
slider_statistics_23 <- slider_statistics_23 %>% 
  select(-abbrevName, -player, -newestTeamLevel.x, -newestTeamLevel.y,
         -Rank.y, -pos.y, -newestTeamName.y, -newestTeamAbbrevName.y, -newestTeamId.x,
         -newestTeamId.y, -newestTeamLocation.y, -batsHand.y, -throwsHand.y, -`Ground#`,
         -`Fly#`, -`Line#`, -`Popup#`, -`Bunt#`)%>% 
  rename(Miss = `Miss%`, Rank = Rank.x, pos = pos.x, newestTeamName = newestTeamName.x,
         newestTeamAbbrevName = newestTeamAbbrevName.x, newestTeamLocation = newestTeamLocation.x,
         batsHand = batsHand.x, throwsHand = throwsHand.x, `Miss In Zone`= `Miss% In Zone`,
         Chase = `Chase%`, InZone = `InZone%`, Ground = `Ground%`, Fly = `Fly%`,
         Line = `Line%`, Popup = `Popup%`, Bunt = `Bunt%`)
 
# Change the numeric data from character values to numeric values
slider_statistics_23 <- slider_statistics_23 %>% 
  mutate_at(c('Middle HorzApprAngle', 'HorzApprAngle', 'VertApprAngle Middle Height'
              ,'VertApprAngle', 'Extension', 'ExitVel', 'Spin'
              ,'HorzBrk', 'IndVertBrk'), as.numeric)%>% 
  mutate(playerId = as.character(playerId)) %>% 
  mutate(Rank = as.character(Rank))%>% 
  mutate(year = "2023")%>% 
  arrange(playerId)

# Flag Pitchers with opposite throwing and pitching hands in 2023
slider_statistics_23$Flag = ""
slider_statistics_23$Flag[(slider_statistics_23$throwsHand == "R" & slider_statistics_23$batsHand == "L")] <- "Flagged"
slider_statistics_23$Flag[(slider_statistics_23$throwsHand == "L" & slider_statistics_23$batsHand == "R")] <- "Flagged"
slider_statistics_23$Flag[slider_statistics_23$batsHand == "S"] <- "Flagged"


# Repeat the previous cleaning steps for 2022
slider_statistics_22 <- slider_statistics_22 %>% 
  select(-abbrevName, -player, -newestTeamLevel.x, -newestTeamLevel.y,
         -Rank.y, -pos.y, -newestTeamName.y, -newestTeamAbbrevName.y, -newestTeamId.x,
         -newestTeamId.y, -newestTeamLocation.y, -batsHand.y, -throwsHand.y, -`Ground#`,
         -`Fly#`, -`Line#`, -`Popup#`, -`Bunt#`) %>% 
  rename(Miss = `Miss%`, Rank = Rank.x, pos = pos.x, newestTeamName = newestTeamName.x,
         newestTeamAbbrevName = newestTeamAbbrevName.x, newestTeamLocation = newestTeamLocation.x,
         batsHand = batsHand.x, throwsHand = throwsHand.x, `Miss In Zone`= `Miss% In Zone`,
         Chase = `Chase%`, InZone = `InZone%`, Ground = `Ground%`, Fly = `Fly%`,
         Line = `Line%`, Popup = `Popup%`, Bunt = `Bunt%`)

# Changing value types for 2022
slider_statistics_22 <- slider_statistics_22 %>% 
  mutate_at(c('Middle HorzApprAngle', 'HorzApprAngle', 'VertApprAngle Middle Height'
              ,'VertApprAngle', 'Extension', 'ExitVel', 'Spin'
              ,'HorzBrk', 'IndVertBrk'), as.numeric)%>% 
  mutate(playerId = as.character(playerId))%>% 
  mutate(Rank = as.character(Rank))%>% 
  mutate(year = "2022")%>% 
  arrange(playerId)

# Flag Pitchers with opposite throwing and pitching hands in 2022
slider_statistics_22$Flag = ""
slider_statistics_22$Flag[(slider_statistics_22$throwsHand == "R" & slider_statistics_22$batsHand == "L")] <- "Flagged"
slider_statistics_22$Flag[(slider_statistics_22$throwsHand == "L" & slider_statistics_22$batsHand == "R")] <- "Flagged"
slider_statistics_22$Flag[slider_statistics_22$batsHand == "S"] <- "Flagged"


# Repeat the previous cleaning steps for 2021
slider_statistics_21 <- slider_statistics_21 %>% 
  select(-abbrevName, -player, -newestTeamLevel.x, -newestTeamLevel.y,
         -Rank.y, -pos.y, -newestTeamName.y, -newestTeamAbbrevName.y, -newestTeamId.x,
         -newestTeamId.y, -newestTeamLocation.y, -batsHand.y, -throwsHand.y, -`Ground#`,
         -`Fly#`, -`Line#`, -`Popup#`, -`Bunt#`)%>% 
  rename(Miss = `Miss%`, Rank = Rank.x, pos = pos.x, newestTeamName = newestTeamName.x,
         newestTeamAbbrevName = newestTeamAbbrevName.x, newestTeamLocation = newestTeamLocation.x,
         batsHand = batsHand.x, throwsHand = throwsHand.x, `Miss In Zone`= `Miss% In Zone`,
         Chase = `Chase%`, InZone = `InZone%`, Ground = `Ground%`, Fly = `Fly%`,
         Line = `Line%`, Popup = `Popup%`, Bunt = `Bunt%`)

# Changing value types for 2021
slider_statistics_21 <- slider_statistics_21 %>% 
  mutate_at(c('Middle HorzApprAngle', 'HorzApprAngle', 'VertApprAngle Middle Height'
              ,'VertApprAngle', 'Extension', 'ExitVel', 'Spin'
              ,'HorzBrk', 'IndVertBrk'), as.numeric) %>% 
  mutate(playerId = as.character(playerId))%>% 
  mutate(Rank = as.character(Rank)) %>% 
  mutate(year = "2021") %>% 
  arrange(playerId)

# Flag Pitchers with opposite throwing and pitching hands for 2021
slider_statistics_21$Flag = ""
slider_statistics_21$Flag[(slider_statistics_21$throwsHand == "R" & slider_statistics_21$batsHand == "L")] <- "Flagged"
slider_statistics_21$Flag[(slider_statistics_21$throwsHand == "L" & slider_statistics_21$batsHand == "R")] <- "Flagged"
slider_statistics_21$Flag[slider_statistics_21$batsHand == "S"] <- "Flagged"


# Create a Data frame with each player included in every year with NA 
# values if they did not play in that year

# Combine all 3 years into one data frame
combined <- rbind(slider_statistics_21,slider_statistics_22,slider_statistics_23)
combined <- combined %>% 
  arrange(playerId)
uniques <- combined[!duplicated(combined$playerId), ]
uniques <- uniques %>% 
  arrange(playerId)

# Collect the players that did not play in 2021
missing_players_21 <- uniques[!uniques$playerId %in% slider_statistics_21$playerId, "playerId"]

# Use a for loop to add missing players to the 2021 data frame
for (player_id in missing_players_21) {
  playerFullName <- uniques$playerFullName[uniques$playerId == player_id]
  Rank <- uniques$Rank[uniques$playerId == player_id]
  pos <- uniques$pos[uniques$playerId == player_id]
  newestTeamName <- uniques$newestTeamName[uniques$playerId == player_id]
  newestTeamAbbrevName <- uniques$newestTeamAbbrevName[uniques$playerId == player_id]
  newestTeamLocation <- uniques$newestTeamLocation[uniques$playerId == player_id]
  batsHand <- uniques$batsHand[uniques$playerId == player_id]
  throwsHand <- uniques$throwsHand[uniques$playerId == player_id]
  Flag <- uniques$Flag[uniques$playerId == player_id]
  new_row <- data.frame(playerId = player_id, playerFullName = playerFullName,
                        Rank = Rank, pos = pos, newestTeamName = newestTeamName,
                        newestTeamAbbrevName = newestTeamAbbrevName,
                        newestTeamLocation = newestTeamLocation,
                        batsHand = batsHand, throwsHand = throwsHand,
                        `Middle HorzApprAngle` = NA, `HorzApprAngle` = NA, `VertApprAngle Middle Height` = NA, `VertApprAngle` = NA, Extension = NA, RelZ = NA, RelX = NA,
                        `Put Away Rate` = NA, `Miss In Zone` = NA, `Miss` = NA, `Chase` = NA, `InZone` = NA, P = NA, BA = NA, IP = NA, ExitVel = NA, Vel = NA, MxVel = NA,
                        Spin = NA, HorzBrk = NA, IndVertBrk = NA, G = NA, BF = NA, `GB/FB` = NA, `Ground` = NA, `Fly` = NA, `Line` = NA,
                        `Popup` = NA, `Bunt` = NA, year = NA, Flag = Flag, check.names = FALSE)
  slider_statistics_21 <- rbind(slider_statistics_21, new_row)
}
# Arrange by Player Id
slider_statistics_21 <- slider_statistics_21 %>% 
  arrange(playerId)



# Collect the players that did not play in 2022
missing_players_22 <- uniques[!uniques$playerId %in% slider_statistics_22$playerId, "playerId"]

# Use a for loop to add missing players to the 2021 data frame
for (player_id in missing_players_22) {
  playerFullName <- uniques$playerFullName[uniques$playerId == player_id]
  Rank <- uniques$Rank[uniques$playerId == player_id]
  pos <- uniques$pos[uniques$playerId == player_id]
  newestTeamName <- uniques$newestTeamName[uniques$playerId == player_id]
  newestTeamAbbrevName <- uniques$newestTeamAbbrevName[uniques$playerId == player_id]
  newestTeamLocation <- uniques$newestTeamLocation[uniques$playerId == player_id]
  batsHand <- uniques$batsHand[uniques$playerId == player_id]
  throwsHand <- uniques$throwsHand[uniques$playerId == player_id]
  Flag <- uniques$Flag[uniques$playerId == player_id]
  new_row <- data.frame(playerId = player_id, playerFullName = playerFullName,
                        Rank = Rank, pos = pos, newestTeamName = newestTeamName,
                        newestTeamAbbrevName = newestTeamAbbrevName,
                        newestTeamLocation = newestTeamLocation,
                        batsHand = batsHand, throwsHand = throwsHand,
                        `Middle HorzApprAngle` = NA, `HorzApprAngle` = NA, `VertApprAngle Middle Height` = NA, `VertApprAngle` = NA, Extension = NA, RelZ = NA, RelX = NA,
                        `Put Away Rate` = NA, `Miss In Zone` = NA, `Miss` = NA, `Chase` = NA, `InZone` = NA, P = NA, BA = NA, IP = NA, ExitVel = NA, Vel = NA, MxVel = NA,
                        Spin = NA, HorzBrk = NA, IndVertBrk = NA, G = NA, BF = NA, `GB/FB` = NA, `Ground` = NA, `Fly` = NA, `Line` = NA,
                        `Popup` = NA, `Bunt` = NA, year = NA, Flag = Flag, check.names = FALSE)
  slider_statistics_22 <- rbind(slider_statistics_22, new_row)
}

# Arrange by Player Id
slider_statistics_22 <- slider_statistics_22 %>% 
  arrange(playerId)


# Collect the players that did not play in 2023
missing_players_23 <- uniques[!uniques$playerId %in% slider_statistics_23$playerId, "playerId"]

# Use a for loop to add missing players to the 2021 data frame
for (player_id in missing_players_23) {
  playerFullName <- uniques$playerFullName[uniques$playerId == player_id]
  Rank <- uniques$Rank[uniques$playerId == player_id]
  pos <- uniques$pos[uniques$playerId == player_id]
  newestTeamName <- uniques$newestTeamName[uniques$playerId == player_id]
  newestTeamAbbrevName <- uniques$newestTeamAbbrevName[uniques$playerId == player_id]
  newestTeamLocation <- uniques$newestTeamLocation[uniques$playerId == player_id]
  batsHand <- uniques$batsHand[uniques$playerId == player_id]
  throwsHand <- uniques$throwsHand[uniques$playerId == player_id]
  Flag <- uniques$Flag[uniques$playerId == player_id]
  new_row <- data.frame(playerId = player_id, playerFullName = playerFullName,
                        Rank = Rank, pos = pos, newestTeamName = newestTeamName,
                        newestTeamAbbrevName = newestTeamAbbrevName,
                        newestTeamLocation = newestTeamLocation,
                        batsHand = batsHand, throwsHand = throwsHand,
                        `Middle HorzApprAngle` = NA, `HorzApprAngle` = NA, `VertApprAngle Middle Height` = NA, `VertApprAngle` = NA, Extension = NA, RelZ = NA, RelX = NA,
                        `Put Away Rate` = NA, `Miss In Zone` = NA, `Miss` = NA, `Chase` = NA, `InZone` = NA, P = NA, BA = NA, IP = NA, ExitVel = NA, Vel = NA, MxVel = NA,
                        Spin = NA, HorzBrk = NA, IndVertBrk = NA, G = NA, BF = NA, `GB/FB` = NA, `Ground` = NA, `Fly` = NA, `Line` = NA,
                        `Popup` = NA, `Bunt` = NA, year = NA, Flag = Flag, check.names = FALSE)
  slider_statistics_23 <- rbind(slider_statistics_23, new_row)
}

# Arrange by Player Id
slider_statistics_23 <- slider_statistics_23 %>% 
  arrange(playerId)




#Exploratory Data Analysis
# Identify rows with missing values in the dataset for 2023, 2022, and 2021

slider_statistics_23[!complete.cases(slider_statistics_23), ]
slider_statistics_22[!complete.cases(slider_statistics_22), ]
slider_statistics_21[!complete.cases(slider_statistics_21), ]

# Explore basic summary statistics
# Use a for loop to get the summary statistics of each numeric variable in 2023

# Separate the variables that are numeric for 2023
numeric_columns23 <- names(slider_statistics_23)[sapply(slider_statistics_23, is.numeric)]

# Loop through each numeric column and display the summary statistics to the Console
for (col in numeric_columns23) {
  cat("Summary statistics for", col, ":\n")
  print(summary(slider_statistics_23[[col]]))
  cat("\n")
}

# Use a for loop to get the standard deviation for each numeric variable

# Create an empty vector to store the standard deviations
std_deviations <- vector("double", length(numeric_columns23))

# Loop through each numeric column
for (i in 1:length(numeric_columns23)) {
  column <- slider_statistics_23[[numeric_columns23[i]]]
  std_deviations[i] <- sd(column, na.rm = TRUE)
}

# Display the standard deviations to the console
for (i in 1:length(numeric_columns23)) {
  cat("Standard deviation of", numeric_columns23[i], ":", std_deviations[i], "\n")
}



# Use a for loop to get the summary statistics of each numeric variable in 2022

# Separate the variables that are numeric for 2022
numeric_columns22 <- names(slider_statistics_22)[sapply(slider_statistics_22, is.numeric)]

# Loop through each numeric column and display the summary statistics to the Console
for (col in numeric_columns22) {
  cat("Summary statistics for", col, ":\n")
  print(summary(slider_statistics_22[[col]]))
  cat("\n")
}

# Use a for loop to get the standard deviation for each numeric variable

# Create an empty vector to store the standard deviations
std_deviations <- vector("double", length(numeric_columns22))

# Loop through each numeric column
for (i in 1:length(numeric_columns22)) {
  column <- slider_statistics_22[[numeric_columns22[i]]]
  std_deviations[i] <- sd(column, na.rm = TRUE)
}

# Display the standard deviations to the console
for (i in 1:length(numeric_columns22)) {
  cat("Standard deviation of", numeric_columns22[i], ":", std_deviations[i], "\n")
}



# Use a for loop to get the summary statistics of each numeric variable in 2022

# Separate the variables that are numeric for 2022
numeric_columns21 <- names(slider_statistics_21)[sapply(slider_statistics_21, is.numeric)]

# Loop through each numeric column and display the summary statistics to the Console
for (col in numeric_columns21) {
  cat("Summary statistics for", col, ":\n")
  print(summary(slider_statistics_21[[col]]))
  cat("\n")
}

# Use a for loop to get the standard deviation for each numeric variable

# Create an empty vector to store the standard deviations
std_deviations <- vector("double", length(numeric_columns21))

# Loop through each numeric column
for (i in 1:length(numeric_columns21)) {
  column <- slider_statistics_21[[numeric_columns21[i]]]
  std_deviations[i] <- sd(column, na.rm = TRUE)
}


# Display the standard deviations to the console
for (i in 1:length(numeric_columns21)) {
  cat("Standard deviation of", numeric_columns21[i], ":", std_deviations[i], "\n")
}

# Create Tabulations for each categorical variable

# 2023
table(slider_statistics_23$pos)
table(slider_statistics_23$newestTeamName)
table(slider_statistics_23$batsHand)
table(slider_statistics_23$throwsHand)

# 2022
table(slider_statistics_22$pos)
table(slider_statistics_22$newestTeamName)
table(slider_statistics_22$batsHand)
table(slider_statistics_22$throwsHand)

# 2021
table(slider_statistics_21$pos)
table(slider_statistics_21$newestTeamName)
table(slider_statistics_21$batsHand)
table(slider_statistics_21$throwsHand)


# Create Box plots and Histograms to analyze the numeric variables

# For the histogram check what the modes are and if the distribution is
# approximately normal

# 2023
# Use a for loop to get the box plots and histograms for each variable

# Save the box plots and histograms as a pdf
#pdf(file = "C:\\Users\\kearn\\R Projects\\Summer_Research\\Sliders23Plots.pdf")

for (col in numeric_columns23) {
  plot.new()
  boxplot(slider_statistics_23[[col]], horizontal = TRUE,
          main = paste("2023 Boxplot of", col))
  hist(slider_statistics_23[[col]], main = paste("2023 Histogram of", col))
}
# Creates the file
#dev.off

# See how many outliers exist for a given variable
outliers <- boxplot(slider_statistics_23$`Popup`,
                    plot = FALSE)$out
length(outliers)


# 2022
# Use a for loop to get the box plots and histograms for each variable

# Save the box plots and histograms as a pdf
#pdf(file = "C:\\Users\\kearn\\R Projects\\Summer_Research\\Sliders22Plots.pdf")

for (col in numeric_columns22) {
  plot.new()
  boxplot(slider_statistics_22[[col]], horizontal = TRUE,
          main = paste("2022 Boxplot of", col))
  hist(slider_statistics_22[[col]], main = paste("2022 Histogram of", col))
}
# Creates the file
#dev.off

# See how many outliers exist
outliers <- boxplot(slider_statistics_22$`Popup`,
                    plot = FALSE)$out
length(outliers)


# 2021
# Use a for loop to get the box plots and histograms for each variable

# Save the box plots and histograms as a pdf
#pdf(file = "C:\\Users\\kearn\\R Projects\\Summer_Research\\Sliders21Plots.pdf")

for (col in numeric_columns21) {
  plot.new()
  boxplot(slider_statistics_21[[col]], horizontal = TRUE,
          main = paste("2021 Boxplot of", col))
  hist(slider_statistics_21[[col]], main = paste("2021 Histogram of", col))
}
# Creates the file
#dev.off

# See how many outliers exist
outliers <- boxplot(slider_statistics_21$`Popup`,
                    plot = FALSE)$out
length(outliers)


# Starting Bivariate Exploratory Data Analysis

# Create cross tabulations for bivariate categorical variables

table(slider_statistics_21$throwsHand, slider_statistics_21$batsHand)
table(slider_statistics_21$throwsHand, slider_statistics_21$newestTeamName)

table(slider_statistics_22$throwsHand, slider_statistics_22$batsHand)
table(slider_statistics_22$throwsHand, slider_statistics_22$newestTeamName)

table(slider_statistics_23$throwsHand, slider_statistics_23$batsHand)
table(slider_statistics_23$throwsHand, slider_statistics_23$newestTeamName)


# Create pairwise scatter plots to analyze bivariate numeric variables
# using only columns with numeric variables
# Note: try different combinations of the variables

pairs(slider_statistics_23[, c(13:20,22,24,29,32:36)])
pairs(slider_statistics_22[, 9:20])
pairs(slider_statistics_21[,c(9,11,13:20,22,24:29)])


# Create Correlation Matrix for the numeric variables 
# to check for linear relationships

cor(slider_statistics_23[, 10:43], method = "pearson", use = "pairwise.complete.obs")
cor(slider_statistics_22[, 10:43], method = "pearson", use = "pairwise.complete.obs")
cor(slider_statistics_21[, 10:43], method = "pearson", use = "pairwise.complete.obs")

# Create and Visualize a correlation matrix that checks for 
# non-linear relationships within the numeric variables

# Load in the packages for use
library(corrplot)
library(Hmisc)

# Checks for non-linear association in each year
rcorr(as.matrix(slider_statistics_23[, 10:43]))
rcorr(as.matrix(slider_statistics_22[, 10:43]))
rcorr(as.matrix(slider_statistics_21[, 10:43]))

# Assign the Matrices for visualization
cor_23 <- rcorr(as.matrix(slider_statistics_23[, 10:43]))
cor_22 <- rcorr(as.matrix(slider_statistics_22[, 10:43]))
cor_21 <- rcorr(as.matrix(slider_statistics_21[, 10:43]))

# Visualize the Correlation matrix for all 3 years
corrplot(cor_23$r, method = "color", type = "full", tl.cex = .5)
corrplot(cor_22$r, method = "color", type = "full", tl.cex = .5)
corrplot(cor_21$r, method = "color", type = "full", tl.cex = .5)



# Create side-by-side box plots to analyze bivariate categorical & numeric variables
# Analyzing throwing hand categorical variables here by comparing left and
# right handed pitchers for each variable


# Save the plots as a pdf
#pdf(file = "C:\\Users\\kearn\\R Projects\\Summer_Research\\Slider Data 2021 Bivariate Side-by-Side Box Plots.pdf")

# For loop for 2021 to create every side by side box plot
for (col in numeric_columns21) {
  plot.new()
  boxplot(slider_statistics_21[[col]] ~ slider_statistics_21$throwsHand,
          horizontal = TRUE, col = c("blue", "red"),
          main = paste("2021 Throwing Hand Side-by-Side Boxplot of", col))
}
# Creates the file
#dev.off

# Save the plots as a pdf
#pdf(file = "C:\\Users\\kearn\\R Projects\\Summer_Research\\Slider Data 2022 Bivariate Side-by-Side Box Plots.pdf")

# For loop for 2022 to create every side by side box plot
for (col in numeric_columns22) {
  plot.new()
  boxplot(slider_statistics_22[[col]] ~ slider_statistics_22$throwsHand,
          horizontal = TRUE, col = c("blue", "red"),
          main = paste("2022 Throwing Hand Side-by-Side Boxplot of", col))
}
# Creates the file
#dev.off

# Save the plots as a pdf
#pdf(file = "C:\\Users\\kearn\\R Projects\\Summer_Research\\Slider Data 2023 Bivariate Side-by-Side Box Plots.pdf")

# For loop for 2023 to create every side by side box plot
for (col in numeric_columns23) {
  plot.new()
  boxplot(slider_statistics_23[[col]] ~ slider_statistics_23$throwsHand,
          horizontal = TRUE, col = c("blue", "red"),
          main = paste("2023 Throwing Hand Side-by-Side Boxplot of", col))
}
# Creates the file
#dev.off


# Side-by-side box plots for each variable in each year

# Save the plots as a pdf
pdf(file = "C:\\Users\\kearn\\R Projects\\Summer_Research\\Slider Data yearly comparison Box Plots.pdf")

# For loop to create the plots for each variable in 2023
for (col in numeric_columns23) {
  plot.new()
  # Create a list of the three years
  data <- list(slider_statistics_21[[col]], slider_statistics_22[[col]], slider_statistics_23[[col]])
  # Set the parameters to a 1 by 3
  par(mfrow = c(1, 3))
  # Set the same y-axis limits for all three boxplots
  ylim <- range(unlist(data), na.rm = TRUE)
  # Box plots for each year
  boxplot(slider_statistics_21[[col]], main = paste("2021", col), ylim = ylim)
  boxplot(slider_statistics_22[[col]], main = paste("2022", col), ylim = ylim)
  boxplot(slider_statistics_23[[col]], main = paste("2023", col), ylim = ylim)
}
# Creates the file
dev.off()


# Create Overlaid Density Plots to analyze bivariate categorical & numeric variable
# Read in ggplot package
library(ggplot2)

# Create ggplot with the throwing hand categorical variable
# and 1 numeric variable to compare density plots

ggplot(slider_statistics_21, aes(x = column, col = throwsHand))+
  geom_density()


ggplot(slider_statistics_22, aes(x = HorzBrk, col = throwsHand))+
  geom_density()

ggplot(slider_statistics_23, aes(x = HorzBrk, col = throwsHand))+
  geom_density()



# Create plots over time with the sample mean of each variable
# with medians overlaid


# Save the plots as a pdf
#pdf(file = "C:\\Users\\kearn\\R Projects\\Summer_Research\\Slider Data Variable Mean & Median Over Time.pdf")

# Create a for loop to graph each variable
for (col in numeric_columns23) {
  # Get the mean for each year
  means <- c(mean(slider_statistics_21[[col]], na.rm = TRUE),
             mean(slider_statistics_22[[col]], na.rm = TRUE),
             mean(slider_statistics_23[[col]], na.rm = TRUE))
  
  # Create a vector of the years
  year <- c(2021, 2022, 2023)
  
  # Get the median for each year
  medians <- c(median(slider_statistics_21[[col]], na.rm = TRUE),
               median(slider_statistics_22[[col]], na.rm = TRUE),
               median(slider_statistics_23[[col]], na.rm = TRUE))
  
  # Create a data frame
  frame <- data.frame(year, means, medians)
  
  # Plot the mean and median
  plot <- ggplot(frame, aes(x = year)) +
    geom_point(aes(y = means, color = "means")) +
    geom_line(aes(y = means, color = "means")) +
    geom_point(aes(y = medians, color = "medians")) +
    geom_line(aes(y = medians, color = "medians")) +
    labs(title = paste("Mean and Median of", col, "by Year"),
         x = "Year",
         y = "Value") +
    scale_color_manual(values = c("means" = "blue", "medians" = "green")) +
    scale_x_continuous(breaks = year, labels = year)
  
  # Print the plot
  print(plot)
}

# Save the pdf
#dev.off()



# Create a plot of the Mean and Median of
# the Pairwise Correlation over the 3 Years

# Create the Matrix and Assign the Lower Triangle for each year
cor_matrix23 <- cor(slider_statistics_23[, 10:43], method = "pearson", use = "pairwise.complete.obs")
lower_triangle23 <- cor_matrix23[lower.tri(cor_matrix23)]

cor_matrix22 <- cor(slider_statistics_22[, 10:43], method = "pearson", use = "pairwise.complete.obs")
lower_triangle22 <- cor_matrix22[lower.tri(cor_matrix22)]

cor_matrix21 <- cor(slider_statistics_21[, 10:43], method = "pearson", use = "pairwise.complete.obs")
lower_triangle21 <- cor_matrix21[lower.tri(cor_matrix21)]

# Create a vector with the means from each year
corMeans <- c(mean(lower_triangle21, na.rm = TRUE), mean(lower_triangle22, na.rm = TRUE),
              mean(lower_triangle23, na.rm = TRUE))

# Create a vector with the medians from each year
corMedians <- c(median(lower_triangle21, na.rm = TRUE), median(lower_triangle22, na.rm = TRUE)
                , median(lower_triangle23, na.rm = TRUE))
# Create a vector of the years
year <- c(2021, 2022, 2023)

# Create a data frame
cor_frame <- data.frame(year, corMeans, corMedians)

# Save the plot as a pdf
#pdf(file = "C:\\Users\\kearn\\R Projects\\Summer_Research\\Slider Data Mean of Pairwise Correlation.pdf")

# Plot the mean and median
cor_plot <- ggplot(cor_frame, aes(x = year)) +
  geom_point(aes(y = corMeans, color = "corMeans")) +
  geom_line(aes(y = corMeans, color = "corMeans")) +
  geom_point(aes(y = corMedians, color = "corMedians")) +
  geom_line(aes(y = corMedians, color = "corMedians")) +
  labs(title = paste("Mean and Median of the Pairwise Correlation by Year"),
       x = "Year",
       y = "Value") +
  scale_color_manual(values = c("corMeans" = "red", "corMedians" = "green")) +
  scale_x_continuous(breaks = year, labels = year)

# Print the plot
print(cor_plot)

# Save the pdf
#dev.off()


# For each pair of numeric variables plot the correlation estimates
# over the years

cor_matrix2023 <- cor(slider_statistics_23[, 10:43], method = "pearson", use = "pairwise.complete.obs")
cor_matrix2022 <- cor(slider_statistics_22[, 10:43], method = "pearson", use = "pairwise.complete.obs")
cor_matrix2021 <- cor(slider_statistics_21[, 10:43], method = "pearson", use = "pairwise.complete.obs")


# Extract variable names
variables <- colnames(cor_matrix2023)

# Save the plot as a pdf
pdf(file = "C:\\Users\\kearn\\R Projects\\Summer_Research\\Plots of Slider Data Correlations over the Years.pdf")

# Loop over each pair of variables
for (i in 1:(length(variables) - 1)) {
  for (j in (i + 1):length(variables)) {
    var1 <- variables[i]
    var2 <- variables[j]
    
    # Subset correlation matrix
    cor_values23 <- cor_matrix2023[i, j]
    cor_values22 <- cor_matrix2022[i,j]
    cor_values21 <- cor_matrix2021[i,j]
    
    # Specify years
    years <- c(2021, 2022, 2023)
    
    # Subset correlation estimates for the specific years
    cor_estimates <- c(cor_values21, cor_values22, cor_values23)
    
    # Create a data frame
    frame <- data.frame(years, cor_estimates)
    
    # Plot the Correlation Estimates
    plot <- ggplot(frame, aes(x = years)) +
      geom_point(aes(y = cor_estimates, color = "cor_estimates")) +
      geom_line(aes(y = cor_estimates, color = "cor_estimates")) +
      labs(title = paste("Correlation between", var1, "and", var2),
           x = "Year", 
           y = "Correlation") +
      scale_color_manual(values = c("cor_estimates" = "blue")) +
      scale_x_continuous(breaks = years, labels = years)
    
    # Print the plot
    print(plot)
  }
}

# Save the pdf
dev.off()

# Simplify the correlation matrix to only see significant values
is.na(cor_matrix2021) <- abs(cor_matrix2021) < 0.5

is.na(cor_matrix2022) <- abs(cor_matrix2022) < 0.5

is.na(cor_matrix2023) <- abs(cor_matrix2023) < 0.5


# Principal Component Analysis

# 2023
# Extract the numeric variables from the data frame
numeric_matrix23 <- na.omit(as.matrix(slider_statistics_23[,numeric_columns23]))
# Perform PCA
pca_result <- prcomp(numeric_matrix23, scale = TRUE, center = TRUE)
# Explore the PCA results
# Access the principal components (PCs)
pc_scores <- pca_result$x

# Access the standard deviations of the PCs (variances explained)
pc_variances <- pca_result$sdev^2

# Access the proportion of variance explained by each PC
pc_variances_prop <- pc_variances / sum(pc_variances)

# Access the correlations between variables and PCs
pc_loadings <- pca_result$rotation

# Get a Summary of the PCA
summary(pca_result)

# Interpret the PCA results
# Analyze the variance explained by each PC
cumulative_var <- cumsum(pc_variances_prop)
plot(1:length(pc_variances_prop), cumulative_var, type = "b", xlab = "Number of PCs", ylab = "Cumulative Variance Explained")

# Identify the number of PCs that explain a desired amount of variance
desired_variance <- 0.8
num_pcs <- sum(cumulative_var <= desired_variance) + 1

# Select the desired number of PCs to retain
selected_pcs <- pc_scores[, 1:num_pcs]


# Create an Updated Matrix with the PCs
na23 <- na.omit(slider_statistics_23)
na23 <- na23[,!sapply(na23, is.numeric)]
PCframe23 <- data.frame(na23, selected_pcs)



# 2022
# Extract the numeric variables from the data frame
numeric_matrix22 <- na.omit(as.matrix(slider_statistics_22[,numeric_columns22]))
# Perform PCA
pca_result <- prcomp(numeric_matrix22, scale = TRUE, center = TRUE)
# Explore the PCA results
# Access the principal components (PCs)
pc_scores <- pca_result$x

# Access the standard deviations of the PCs (variances explained)
pc_variances <- pca_result$sdev^2

# Access the proportion of variance explained by each PC
pc_variances_prop <- pc_variances / sum(pc_variances)

# Access the correlations between variables and PCs
pc_loadings <- pca_result$rotation

# Get a Summary of the PCA
summary(pca_result)

# Interpret the PCA results
# Analyze the variance explained by each PC
cumulative_var <- cumsum(pc_variances_prop)
plot(1:length(pc_variances_prop), cumulative_var, type = "b", xlab = "Number of PCs", ylab = "Cumulative Variance Explained")

# Identify the number of PCs that explain a desired amount of variance
desired_variance <- 0.8
num_pcs <- sum(cumulative_var <= desired_variance) + 1

# Select the desired number of PCs to retain
selected_pcs <- pc_scores[, 1:num_pcs]

# Create an Updated Matrix with the PCs
na22 <- na.omit(slider_statistics_22)
na22 <- na22[,!sapply(na22, is.numeric)]
PCframe22 <- data.frame(na22, selected_pcs)



# 2021
# Extract the numeric variables from the data frame
numeric_matrix21 <- na.omit(as.matrix(slider_statistics_21[,numeric_columns21]))
# Perform PCA
pca_result <- prcomp(numeric_matrix21, scale = TRUE, center = TRUE)
# Explore the PCA results
# Access the principal components (PCs)
pc_scores <- pca_result$x

# Access the standard deviations of the PCs (variances explained)
pc_variances <- pca_result$sdev^2

# Access the proportion of variance explained by each PC
pc_variances_prop <- pc_variances / sum(pc_variances)

# Access the correlations between variables and PCs
pc_loadings <- pca_result$rotation

# Get a Summary of the PCA
summary(pca_result)

# Interpret the PCA results
# Analyze the variance explained by each PC
cumulative_var <- cumsum(pc_variances_prop)
plot(1:length(pc_variances_prop), cumulative_var, type = "b", xlab = "Number of PCs", ylab = "Cumulative Variance Explained")

# Identify the number of PCs that explain a desired amount of variance
desired_variance <- 0.8
num_pcs <- sum(cumulative_var <= desired_variance) + 1

# Select the desired number of PCs to retain
selected_pcs <- pc_scores[, 1:num_pcs]

# Create an Updated Matrix with the PCs
na21 <- na.omit(slider_statistics_21)
na21 <- na21[,!sapply(na21, is.numeric)]
PCframe21<- data.frame(na21, selected_pcs)

plot(PCframe21$PC1, PCframe21$PC2)


#
# Another way to do PCA to confirm and further assess results with Eigenvalues
#

S <- var(numeric_matrix21)
egn_s <- eigen(S)
PC_S <- numeric_matrix21 %*% egn_s$vectors
R <- cor(numeric_matrix21)
egn_r <- eigen(R)

# Standardized
st <- scale(numeric_matrix21, center = TRUE, scale = TRUE)
PC <- st %*% egn_r$vectors

# Plot to confirm how many PC's to use

plot(1:length(pc_variances_prop), egn_r$values / sum(egn_r$values), type = "b")


#
# Cluster Analysis
#

# For 2023
#
# Hierarchical Clusters for PCA
# Interchange rows and columns to check for clusters both ways
# For this data set the columns show no need for clustering

# Create a filtered data frame
pcs23 <- PCframe23[,sapply(PCframe23, is.numeric)]

# Create a distance matrix for the rows and one for the columns
Drow23 <- dist(pcs23)
Dcol23 <- dist(t(pcs23))

# Create Hierarchical Cluster
hc_out23 <- hclust(Drow23)

# Cluster Dendrogram
plot(hc_out23)

# Scree plot to decide how many clusters
plot(hc_out23$height)

# K means clustering with PCA
clusters23 <- kmeans(pcs23, 2, iter.max = 100, nstart = 1)
plot(pcs23[,1], pcs23[,2], col = clusters23$cluster)


# Hierarchical Clustering with original data
# Create filtered Data frame
filtered23 <- na.omit(slider_statistics_23[,sapply(slider_statistics_23, is.numeric)])
filtered23 <- filtered23[,1:28]
filtered23$`Middle HorzApprAngle` <- abs(filtered23$`Middle HorzApprAngle`)
filtered23$HorzApprAngle <- abs(filtered23$HorzApprAngle)
filtered23$HorzBrk <- abs(filtered23$HorzBrk)
filtered23$RelX <- abs(filtered23$RelX)
# Create a distance matrix for the rows and one for the columns
Drows23 <- dist(filtered23)
Dcols23 <- dist(t(filtered23))

# Create Hierarchical Cluster
hc_outs23 <- hclust(Drows23)

# Cluster Dendrogram
plot(hc_outs23)

# Scree plot to decide how many clusters
plot(hc_outs23$height)

# K means clustering with original Data
# Read in factoextra to check the optimal clusters
library(factoextra)

# Plot the optimal clusters scree plot
fviz_nbclust(scale(filtered23), kmeans, nstart=100, method = "wss")

# Do kmeans clustering with suggested clusters
kmeans_fancy23 <- kmeans(scale(filtered23), 3, nstart = 100)

# Plot the clusters
fviz_cluster(kmeans_fancy23, data = scale(filtered23), geom = c("point"),ellipse.type = "euclid")



# For 2022
#
# Hierarchical Clusters for PCA
# Interchange rows and columns to check for clusters both ways
# For this data set the columns show no need for clustering

# Create a filtered data frame
pcs22 <- PCframe22[,sapply(PCframe22, is.numeric)]

# Create a distance matrix for the rows and one for the columns
Drow22 <- dist(pcs22)
Dcol22 <- dist(t(pcs22))

# Create Hierarchical Cluster
hc_out22 <- hclust(Drow22)

# Cluster Dendrogram
plot(hc_out22)

# Scree plot to decide how many clusters
plot(hc_out22$height)

# K means clustering with PCA
clusters22 <- kmeans(pcs22, 2, iter.max = 100, nstart = 1)
plot(pcs22[,1], pcs22[,2], col = clusters22$cluster)


# Hierarchical Clustering with original data
# Create filtered Data frame
slider_statistics_22 <- slider_statistics_22[-c(100,143),]
filtered22 <- na.omit(slider_statistics_22[,sapply(slider_statistics_22, is.numeric)])
filtered22 <- filtered22[,1:28]
filtered22$`Middle HorzApprAngle` <- abs(filtered22$`Middle HorzApprAngle`)
filtered22$HorzApprAngle <- abs(filtered22$HorzApprAngle)
filtered22$HorzBrk <- abs(filtered22$HorzBrk)
filtered22$RelX <- abs(filtered22$RelX)
# Create a distance matrix for the rows and one for the columns
Drows22 <- dist(filtered22)
Dcols22 <- dist(t(filtered22))

# Create Hierarchical Cluster
hc_outs22 <- hclust(Drows22)

# Cluster Dendrogram
plot(hc_outs22)

# Scree plot to decide how many clusters
plot(hc_outs22$height)

# K means clustering with original Data
# Read in factoextra to check the optimal clusters
library(factoextra)

# Plot the optimal clusters scree plot
fviz_nbclust(scale(filtered22), kmeans, nstart=100, method = "wss")

# Do kmeans clustering with suggested clusters
kmeans_fancy22 <- kmeans(scale(filtered22), 3, nstart = 100)

# Plot the clusters
fviz_cluster(kmeans_fancy22, data = scale(filtered22), geom = c("point"),ellipse.type = "euclid")



# For 2021
#
# Hierarchical Clusters for PCA
# Interchange rows and columns to check for clusters both ways
# For this data set the columns show no need for clustering

# Create a filtered data frame
pcs21 <- PCframe21[,sapply(PCframe21, is.numeric)]

# Create a distance matrix for the rows and one for the columns
Drow21 <- dist(pcs21)
Dcol21 <- dist(t(pcs21))

# Create Hierarchical Cluster
hc_out21 <- hclust(Drow21)

# Cluster Dendrogram
plot(hc_out21)

# Scree plot to decide how many clusters
plot(hc_out21$height)

# K means clustering with PCA
clusters21 <- kmeans(pcs21, 2, iter.max = 100, nstart = 1)
plot(pcs21[,1], pcs21[,2], col = clusters21$cluster)


# Hierarchical Clustering with original data
# Create filtered Data frame
slider_statistics_21 <- slider_statistics_21[-c(670,57,567),]
filtered21 <- na.omit(slider_statistics_21[,sapply(slider_statistics_21, is.numeric)])
filtered21 <- filtered21[,1:28]
filtered21$`Middle HorzApprAngle` <- abs(filtered21$`Middle HorzApprAngle`)
filtered21$HorzApprAngle <- abs(filtered21$HorzApprAngle)
filtered21$HorzBrk <- abs(filtered21$HorzBrk)
filtered21$RelX <- abs(filtered21$RelX)
# Create a distance matrix for the rows and one for the columns
Drows21 <- dist(filtered21)
Dcols21 <- dist(t(filtered21))

# Create Hierarchical Cluster
hc_outs21 <- hclust(Drows21)

# Cluster Dendrogram
plot(hc_outs21)

# Scree plot to decide how many clusters
plot(hc_outs21$height)

# K means clustering with original Data
# Read in factoextra to check the optimal clusters
library(factoextra)

# Plot the optimal clusters scree plot
fviz_nbclust(scale(filtered21), kmeans, nstart=100, method = "wss")

# Do kmeans clustering with suggested clusters
kmeans_fancy21 <- kmeans(scale(filtered21), 2, nstart = 100)


# Plot the clusters
fviz_cluster(kmeans_fancy21, data = scale(filtered21), geom = c("point"),ellipse.type = "euclid")


# Identify Cluster Differences
# 2023
cutree(hc_outs23, k = 3)
hc_labels23 <- cutree(hc_outs23, k = 3)
filtered23[hc_labels23 == 3, ]
summary(filtered23[hc_labels23 == 3, ])
filtered23$cluster <- hc_labels23

# 2022
cutree(hc_outs22, k = 3)
hc_labels22 <- cutree(hc_outs22, k = 3)
filtered22[hc_labels22 == 3, ]
summary(filtered22[hc_labels22 == 3, ])
filtered22$cluster <- hc_labels22

# 2021
cutree(hc_outs21, k = 2)
hc_labels21 <- cutree(hc_outs21, k = 2)
filtered21[hc_labels21 == 2, ]
summary(filtered21[hc_labels21 == 2, ])
filtered21$cluster <- hc_labels21


#
# Linear Discriminant Analysis
#

# Load in MASS Package
library(MASS)

# 2023
# Create the LDA Model to model the Clusters
filtered23 <- filtered23[,-(25:28)]
lda_model23 <- lda(cluster ~ ., data = filtered23)
lda_model23

# Stacked Histogram for Discriminant Function Values Checking Overlap
p <- predict(lda_model23, filtered23)
ldahist(data = p$x[,1], g = filtered23$cluster)

# Confusion Matrix and Accuracy

p1 <- predict(lda_model23, filtered23)$class
tab <- table(Predicted = p1, Actual = filtered23$cluster)
tab

# Accuracy of The Model
sum(diag(tab))/sum(tab)


#
# 2022
# Create the LDA Model to model the Clusters
filtered22 <- filtered22[,-(25:28)]

lda_model22 <- lda(cluster ~ ., data = filtered22)
lda_model22

# Stacked Histogram for Discriminant Function Values Checking Overlap
p <- predict(lda_model22, filtered22)
ldahist(data = p$x[,1], g = filtered22$cluster)

# Confusion Matrix and Accuracy

p1 <- predict(lda_model22, filtered22)$class
tab <- table(Predicted = p1, Actual = filtered22$cluster)
tab
p1
# Accuracy of The Model
sum(diag(tab))/sum(tab)


#
# 2021
# Create the LDA Model to model the Clusters
filtered21 <- filtered21[,-(25:28)]

lda_model21 <- lda(cluster ~ ., data = filtered21)
lda_model21

# Stacked Histogram for Discriminant Function Values Checking Overlap
p <- predict(lda_model21, filtered21)
ldahist(data = p$x[,1], g = filtered21$cluster)

# Confusion Matrix and Accuracy

p1 <- predict(lda_model21, filtered21)$class
tab <- table(Predicted = p1, Actual = filtered21$cluster)
tab

# Accuracy of The Model
sum(diag(tab))/sum(tab)



#
# Logistic & Multinomial Regression
#

# Make the Clusters Factors
filtered21$cluster <- factor(filtered21$cluster)


# Logistic Regression for 2 variables
logit_model <- glm(cluster ~ ., data = filtered21, family = "binomial")


# Load the nnet package
library(nnet)


# Perform multinomial logistic regression
multi_logit_model <- multinom(cluster ~ ., data = filtered21)

multi_logit_model


model_summary <- summary(multi_logit_model)

# Odds Ratio
exp(coefficients(multi_logit_model))

# Significance of Coefficients (p-values)
z <- model_summary$coefficients/model_summary$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2 
p


#
# Factor Analysis
#


# Perform Factor Analysis with 3 factors and a Varimax Rotation

factor_analysis <- factanal(filtered21, factors = 3, rotation = "varimax")
factor_analysis

# Access the factor loading matrix
factor_analysis$loadings  

# Access the commonalities
factor_analysis$communality


#
# Coding Horizontal Break into Categorical Variables
#

# Create a filtered data frame with every pitcher from all 3 years
combined$year <- as.numeric(combined$year)
filter_combined <- na.omit(combined[,sapply(combined, is.numeric)])
filter_combined <- filter_combined[,-(25:29)]
filter_combined$`Middle HorzApprAngle` <- abs(filter_combined$`Middle HorzApprAngle`)
filter_combined$HorzApprAngle <- abs(filter_combined$HorzApprAngle)
filter_combined$HorzBrk <- abs(filter_combined$HorzBrk)
filter_combined$RelX <- abs(filter_combined$RelX)
filter_combined$year <- ifelse(filter_combined$year == 2021, 1,
                               ifelse(filter_combined$year == 2022, 2,
                                      ifelse(filter_combined$year == 2023, 3, filter_combined$year)))

# Create the new Categorical Variable
slider_statistics_23$horz_break_category <- "NA"
slider_statistics_21$horz_break_category <- "NA"
slider_statistics_21$horz_break_category <- "NA"
filter_combined$horz_break_category <- "NA"

# Create Break point for Horizontal Approach Angle
breaks <- c(0,4,8,12, Inf)

# Create Labels for the four categories
labels <- c("Group 1 (Smallest)", "Group 2 (Small)", "Group 3 (Large)", "Group 4 (Largest)")

# Cut Horizontal Approach Angle into the new variable
slider_statistics_23$horz_break_category <- cut(abs(slider_statistics_23$HorzBrk),
                                                breaks = breaks, labels = labels)
slider_statistics_22$horz_break_category <- cut(abs(slider_statistics_22$HorzBrk),
                                                breaks = breaks, labels = labels)
slider_statistics_21$horz_break_category <- cut(abs(slider_statistics_21$HorzBrk),
                                                breaks = breaks, labels = labels)
filter_combined$horz_break_category <- cut(abs(filter_combined$HorzBrk),
                                    breaks = breaks, labels = labels, include.lowest = TRUE)

# Remove outliers
outlier <- boxplot.stats(filter_combined$`Put Away Rate`)$out

missInoutlier <- boxplot.stats(filter_combined$`Miss In Zone`)$out

missoutlier <- boxplot.stats(filter_combined$`Miss`)$out

chaseoutlier <- boxplot.stats(filter_combined$Chase)$out

BAoutlier <- boxplot.stats(filter_combined$BA)$out

filter_combined <- filter_combined[!filter_combined$`Put Away Rate` %in% outlier,]
filter_combined <- filter_combined[!filter_combined$`Miss In Zone` %in% missInoutlier,]
filter_combined <- filter_combined[!filter_combined$Miss %in% missoutlier,]
filter_combined <- filter_combined[!filter_combined$Chase %in% chaseoutlier,]
filter_combined <- filter_combined[!filter_combined$BA %in% BAoutlier,]




# Remove outliers From 2021
outlier <- boxplot.stats(filtered21$`Put Away Rate`)$out

missInoutlier <- boxplot.stats(filtered21$`Miss In Zone`)$out

missoutlier <- boxplot.stats(filtered21$`Miss`)$out

chaseoutlier <- boxplot.stats(filtered21$Chase)$out

BAoutlier <- boxplot.stats(filtered21$BA)$out

filtered21 <- filtered21[!filtered21$`Put Away Rate` %in% outlier,]
filtered21 <- filtered21[!filtered21$`Miss In Zone` %in% missInoutlier,]
filtered21 <- filtered21[!filtered21$Miss %in% missoutlier,]
filtered21 <- filtered21[!filtered21$Chase %in% chaseoutlier,]
filtered21 <- filtered21[!filtered21$BA %in% BAoutlier,]





# Outlier Investigation
outlierStats <- filter_combined
outlierStats <- outlierStats[outlierStats$`Put Away Rate` %in% outlier,]

outlierStats <- outlierStats[outlierStats$`Miss In Zone` %in% missInoutlier,]
outlierStats <- outlierStats[outlierStats$Miss %in% missoutlier,]
outlierStats <- outlierStats[outlierStats$Chase %in% chaseoutlier,]
outlierStats <- outlierStats[outlierStats$BA %in% BAoutlier,]

#
# Pulling All Pitchers with the Lowest Horizontal Break into One Data Frame
# Analyzing what makes a good slider pitcher with a Low Horizontal Break

SmallBreak <- filter_combined %>% 
  filter(horz_break_category == "Group 1 (Smallest)")

good_small <- SmallBreak %>% filter(BA < .11) %>% 
  filter(`Put Away Rate` > .39, Miss > .45, `Miss In Zone` > .26)

#
# Highest Horizontal Break Separation
#

LargeBreak <- filter_combined %>% 
  filter(horz_break_category == "Group 4 (Largest)")

BadLarge <- LargeBreak %>% filter(BA > .22, Miss < .37, `Put Away Rate` < .33)

#
# LDA for Horizontal Break Category
#

lda_model <- lda(horz_break_category ~ ., data = filter_combined)
lda_model

# Stacked Histogram for Discriminant Function Values Checking Overlap
p <- predict(lda_model, filter_combined)
p
ldahist(data = p$x[,1], g = filter_combined$horz_break_category)

# Confusion Matrix and Accuracy

p1 <- predict(lda_model, filter_combined)$class
tab <- table(Predicted = p1, Actual = filter_combined$horz_break_category)
tab

# Accuracy of The Model
sum(diag(tab))/sum(tab)


#
# LDA for Induced Vertical Break Category
#

filter_vertical <- na.omit(combined[,sapply(combined, is.numeric)])
filter_vertical <- filter_vertical[,1:28]
filter_vertical$`Middle HorzApprAngle` <- abs(filter_vertical$`Middle HorzApprAngle`)
filter_vertical$HorzApprAngle <- abs(filter_vertical$HorzApprAngle)
filter_vertical$HorzBrk <- abs(filter_vertical$HorzBrk)
filter_vertical$RelX <- abs(filter_vertical$RelX)

filter_vertical$IndVert_break_category <- "NA"

# Create Break point for Horizontal Approach Angle
vertbreaks <- c(-8.2,-2,2,5, Inf)

# Create Labels for the four categories
vertlabels <- c("Group 1 (Large Negative)", "Group 2 (Close to Zero)", "Group 3 (Large Positive)", "Group 4 (Largest)")

filter_vertical$IndVert_break_category <- cut(filter_vertical$IndVertBrk,
                                           breaks = vertbreaks, labels = vertlabels, include.lowest = TRUE)

lda_model2 <- lda(IndVert_break_category ~ ., data = filter_vertical)
lda_model2

# Stacked Histogram for Discriminant Function Values Checking Overlap
pred <- predict(lda_model2, filter_vertical)
pred
ldahist(data = pred$x[,1], g = filter_vertical$IndVert_break_category)

# Confusion Matrix and Accuracy

p2 <- predict(lda_model2, filter_vertical)$class
tabl <- table(Predicted = p2, Actual = filter_vertical$IndVert_break_category)
tabl

# Accuracy of The Model
sum(diag(tabl))/sum(tabl)


#
# Clustering By Spin Rate
#


filter_spin <- na.omit(combined[,sapply(combined, is.numeric)])
filter_spin <- filter_spin[,1:24]
filter_spin$`Middle HorzApprAngle` <- abs(filter_spin$`Middle HorzApprAngle`)
filter_spin$HorzApprAngle <- abs(filter_spin$HorzApprAngle)
filter_spin$HorzBrk <- abs(filter_spin$HorzBrk)
filter_spin$RelX <- abs(filter_spin$RelX)

filter_spin$Spin_category <- "NA"

# Remove outliers
outlier3 <- boxplot.stats(filter_spin$IndVertBrk)$out

filter_spin <- filter_spin[!filter_spin$IndVertBrk %in% outlier3,]

# Create Break point for Horizontal Approach Angle
spinbreaks <- c(1530,2000,2300,2550, Inf)

# Create Labels for the four categories
spinlabels <- c("Group 1 (Low)", "Group 2 (Below Average)", "Group 3 (Average)", "Group 4 (High)")

filter_spin$Spin_category <- cut(filter_spin$Spin, breaks = spinbreaks, 
                                 labels = spinlabels, include.lowest = TRUE)

lda_model3 <- lda(Spin_category ~ ., data = filter_spin)
lda_model3

# Stacked Histogram for Discriminant Function Values Checking Overlap
pred3 <- predict(lda_model3, filter_spin)
pred3
ldahist(data = pred$x[,1], g = filter_spin$Spin_category)

# Confusion Matrix and Accuracy

p3 <- predict(lda_model3, filter_spin)$class
table <- table(Predicted = p3, Actual = filter_spin$Spin_category)
table

# Accuracy of The Model
sum(diag(table))/sum(table)



#
# Testing Multivariate Regression Models
#

# Correlation Matrix
cor(filter_combined[,-25])

# Checking the appropriate amount of variables to use in each model
library(leaps)

X=cbind(filter_combined$`Middle HorzApprAngle`,filter_combined$HorzApprAngle,
        filter_combined$`VertApprAngle Middle Height`, filter_combined$VertApprAngle
        , filter_combined$Extension, filter_combined$RelZ, 
        filter_combined$RelX, filter_combined$Vel, filter_combined$Spin, 
        filter_combined$HorzBrk, filter_combined$IndVertBrk, filter_combined$year)

leaps(X,filter_combined$`Put Away Rate`,method='adjr2',nbest=1)

# Looking at BIC values
d <- regsubsets(`Put Away Rate` ~ `Middle HorzApprAngle` + HorzApprAngle +
             `VertApprAngle Middle Height` + VertApprAngle +
             Extension + RelZ + RelX + Vel + Spin +
             HorzBrk + IndVertBrk + year + selected_pcs + vselected_pcs, data = filter_combined)
rs <- summary(d)
plot(rs$bic, xlab="Parameter", ylab="BIC") 
summary(d)$which


#
# Miss Percentage Model
Model1 <- lm(Miss ~  HorzApprAngle + Spin + Vel  
             + VertApprAngle + selected_pcs, data = filter_combined)

summary(Model1)
BIC(Model1)

# Interaction Effects
iModel1 <- lm(Miss ~   Spin + Vel + HorzApprAngle
             + VertApprAngle  + HorzApprAngle:VertApprAngle, data = filter_combined)
summary(iModel1)
BIC(iModel1)
AIC(iModel1)
confint(iModel1)

# Checking if the Interaction is significant with ANOVA table
anova(Model1, iModel1)

# Use the Model to Predict Miss% and Compare with Actual
predMiss <- predict(iModel1)

plot(x = filter_combined$Miss, y = predMiss)

# Root Mean Squared Error
rmse <- sqrt(mean((filter_combined$Miss - predMiss)^2))

# Mean Absolute Error
mae <- mean(abs(filter_combined$Miss - predMiss))

# residual plot
residMiss <- filter_combined$Miss - predMiss
plot(predMiss, residMiss, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")

#
# Put Away Rate Model
Model2 <- lm(`Put Away Rate` ~ HorzBrk + Spin + Vel + year
           + `VertApprAngle Middle Height`
           , data = filter_combined)

summary(Model2)

# Interaction Effects
iModel2 <- lm(`Put Away Rate` ~ HorzBrk + Spin + Vel
             + `VertApprAngle Middle Height`  + year
              + HorzBrk:`VertApprAngle Middle Height`,
             data = filter_combined)
summary(iModel2)
confint(iModel2)

anova(Model2, iModel2)

# Use the Model to Predict Put Away Rate and Compare with Actual
predRate <- predict(iModel2)

plot(x = filter_combined$`Put Away Rate`, y = predRate)

# Root Mean Squared Error
rmse <- sqrt(mean((filter_combined$`Put Away Rate` - predRate)^2))

# Mean Absolute Error
mae <- mean(abs(filter_combined$`Put Away Rate` - predRate))

# residual plot
residRate <- filter_combined$`Put Away Rate` - predRate
plot(predRate, residRate, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")

# Shapiro-Wilk
shapiro <- shapiro.test(residuals(iModel2))
shapiro

#
# In Zone Percentage Model
# This looks like a bad model
Model3 <- lm(InZone ~ HorzApprAngle  + year +
               VertApprAngle + Vel, data = filter_combined)

summary(Model3)
confint(Model3)

# Use the Model to Predict in zone percent and Compare with Actual
predZone <- predict(Model3)

plot(x = filter_combined$InZone, y = predZone)

# Root Mean Squared Error
rmse <- sqrt(mean((filter_combined$InZone - predZone)^2))

# Mean Absolute Error
mae <- mean(abs(filter_combined$InZone - predZone))

# residual plot
residZone <- filter_combined$InZone - predZone
plot(predZone, residZone, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")


#
# Chase Percentage Model
Model4 <- lm(Chase ~ HorzApprAngle + Spin + IndVertBrk + vselected_pcs +
               VertApprAngle + Vel, data = filter_combined)

summary(Model4)

iModel4 <- lm(Chase ~ HorzApprAngle + Spin + IndVertBrk + 
               VertApprAngle + Vel + VertApprAngle:HorzApprAngle
             , data = filter_combined)

summary(iModel4)
confint(iModel4)

# Use the Model to Predict Chase percent and Compare with Actual
predChase <- predict(iModel4)

plot(x = filter_combined$Chase, y = predChase)

# Root Mean Squared Error
rmse <- sqrt(mean((filter_combined$Chase - predChase)^2))

# Mean Absolute Error
mae <- mean(abs(filter_combined$Chase - predChase))

# residual plot
residChase <- filter_combined$Chase - predChase
plot(predChase, residChase, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")


#
# Opponent Batting Average Model
Model5 <- lm(BA ~ `VertApprAngle Middle Height` + Spin + year 
                 + Vel + HorzBrk, data = filter_combined)

summary(Model5)

iModel5 <- lm(BA ~ `VertApprAngle Middle Height` + Spin + year
               + Vel + HorzBrk, data = filter_combined)

summary(iModel5)
confint(iModel5)

anova(Model5, iModel5)

# Use the Model to Predict Chase percent and Compare with Actual
predBA <- predict(iModel5)

plot(x = filter_combined$BA, y = predBA)

# Root Mean Squared Error
rmse <- sqrt(mean((filter_combined$BA - predBA)^2))

# Mean Absolute Error
mae <- mean(abs(filter_combined$BA - predBA))

# residual plot
residBA <- filter_combined$BA - predBA
plot(predBA, residBA, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")

#
# Checking Normality Assumptions for each model
hist(residMiss)
plot(density(residMiss), xlab = "Residuals", ylab = "Density", main = "Density Estimate of Residuals")
qqnorm(residMiss)
qqline(residMiss,col="RED",lwd=2)

# X-vars vs residuals on y-axis
plot(filter_combined$IndVertBrk, residChase)


# Inzone model looks like it does not meet requirements and Put Away Rate is
# the best model

# Trying to find the optimal statistics
Optimal <- filter_combined %>% 
  filter( Spin > 2800, Vel > 83, HorzBrk > 5.675, IndVertBrk < 2.5)

Best_stats <- filter_combined %>% 
  filter( `Put Away Rate` > .37, Miss > .41, BA < .14,
          `Miss In Zone` > .18)

Best_stats_removed <- filter_combined %>% 
  filter( `Put Away Rate` > .37, Miss > .41, BA < .14,
          `Miss In Zone` > .18, RelZ > 65, RelZ < 73, RelX > 17, RelX < 29
          , Extension > 5, Extension < 6)









#
# Testing Multivariate Regression Models
# For Each Year Individually

#
# 2021
#

# Correlation Matrix
cor(filtered21)

# Checking the appropriate amount of variables to use in each model
library(leaps)

X=cbind(filtered21$`Middle HorzApprAngle`,filtered21$HorzApprAngle,
        filtered21$`VertApprAngle Middle Height`, filtered21$VertApprAngle
        , filtered21$Extension, filtered21$RelZ, 
        filtered21$RelX, filtered21$Vel, filtered21$Spin, 
        filtered21$HorzBrk, filtered21$IndVertBrk)

leaps(X,filtered21$Miss,method='r2',nbest=1)

#
# Miss Percentage Model
Model1 <- lm(Miss ~   HorzApprAngle + Vel
             + VertApprAngle, data = filtered21)

summary(Model1)

# Interaction Effects
iModel1 <- lm(Miss ~  Vel + VertApprAngle + HorzApprAngle
                , data = filtered21)

summary(iModel1)
confint(iModel1)

# Checking if the Interaction is significant with ANOVA table
anova(Model1, iModel1)

# Use the Model to Predict Miss% and Compare with Actual
predMiss <- predict(iModel1)

plot(x = filtered21$Miss, y = predMiss)

# Root Mean Squared Error
rmse <- sqrt(mean((filtered21$Miss - predMiss)^2))

# Mean Absolute Error
mae <- mean(abs(filtered21$Miss - predMiss))

# residual plot
residMiss <- filtered21$Miss - predMiss
plot(predMiss, residMiss, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")

#
# Put Away Rate Model
Model2 <- lm(`Put Away Rate` ~ HorzBrk + Spin + Vel
             + `VertApprAngle Middle Height` + RelZ + `Middle HorzApprAngle`
             , data = filtered21)

summary(Model2)

# Interaction Effects
iModel2 <- lm(`Put Away Rate` ~ HorzBrk + Spin + Vel
              + `VertApprAngle Middle Height` +
                `VertApprAngle Middle Height`:HorzBrk,
              data = filtered21)
summary(iModel2)
confint(iModel2)

anova(Model2, iModel2)

# Use the Model to Predict Put Away Rate and Compare with Actual
predRate <- predict(iModel2)

plot(x = filtered21$`Put Away Rate`, y = predRate)

# Root Mean Squared Error
rmse <- sqrt(mean((filtered21$`Put Away Rate` - predRate)^2))

# Mean Absolute Error
mae <- mean(abs(filtered21$`Put Away Rate` - predRate))

# residual plot
residRate <- filtered21$`Put Away Rate` - predRate
plot(predRate, residRate, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")


#
# Chase Percentage Model
Model4 <- lm(Chase ~ Spin + IndVertBrk + HorzBrk +
               VertApprAngle + Vel, data = filtered21)

summary(Model4)

iModel4 <- lm(Chase ~ VertApprAngle + Spin 
                  + HorzBrk + Vel
              , data = filtered21)

summary(iModel4)
confint(iModel4)

# Use the Model to Predict Chase percent and Compare with Actual
predChase <- predict(iModel4)

plot(x = filtered21$Chase, y = predChase)

# Root Mean Squared Error
rmse <- sqrt(mean((filtered21$Chase - predChase)^2))

# Mean Absolute Error
mae <- mean(abs(filtered21$Chase - predChase))

# residual plot
residChase <- filtered21$Chase - predChase
plot(predChase, residChase, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")


#
# Opponent Batting Average Model
Model5 <- lm(BA ~  `Middle HorzApprAngle` + `VertApprAngle Middle Height` + Spin
                + Vel + `Middle HorzApprAngle`:Vel, data = filtered21)

summary(Model5)
confint(Model5)


# Use the Model to Predict Chase percent and Compare with Actual
predBA <- predict(Model5)

plot(x = filtered21$BA, y = predBA)

# Root Mean Squared Error
rmse <- sqrt(mean((filtered21$BA - predBA)^2))

# Mean Absolute Error
mae <- mean(abs(filtered21$BA - predBA))

# residual plot
residBA <- filtered21$BA - predBA
plot(predBA, residBA, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")


#
# 2022
#

# Correlation Matrix
cor(filtered22)

# Checking the appropriate amount of variables to use in each model
library(leaps)

X=cbind(filtered22$`Middle HorzApprAngle`,filtered22$HorzApprAngle,
        filtered22$`VertApprAngle Middle Height`, filtered22$VertApprAngle
        , filtered22$Extension, filtered22$RelZ, 
        filtered22$RelX, filtered22$Vel, filtered22$Spin, 
        filtered22$HorzBrk, filtered22$IndVertBrk)

leaps(X,filtered22$`Put Away Rate`,method='r2',nbest=1)

#
# Miss Percentage Model
Model1 <- lm(Miss ~   HorzApprAngle + Vel 
             + VertApprAngle, data = filtered22)

summary(Model1)

# Interaction Effects
iModel1 <- lm(Miss ~  Vel + `Middle HorzApprAngle`
              + VertApprAngle + `Middle HorzApprAngle`:VertApprAngle, data = filtered22)

summary(iModel1)
confint(iModel1)


# Checking if the Interaction is significant with ANOVA table
anova(model1, iModel1)

# Use the Model to Predict Miss% and Compare with Actual
predMiss <- predict(iModel1)

plot(x = filtered22$Miss, y = predMiss)

# Root Mean Squared Error
rmse <- sqrt(mean((filtered22$Miss - predMiss)^2))

# Mean Absolute Error
mae <- mean(abs(filtered22$Miss - predMiss))

# residual plot
residMiss <- filtered22$Miss - predMiss
plot(predMiss, residMiss, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")

#
# Put Away Rate Model
Model2 <- lm(`Put Away Rate` ~  Vel + Extension 
             + `VertApprAngle Middle Height` + HorzApprAngle
             , data = filtered22)

summary(Model2)

# Interaction Effects
iModel2 <- lm(`Put Away Rate` ~  Vel + Extension
              + `VertApprAngle Middle Height` + RelX
               + `VertApprAngle Middle Height`:Vel, data = filtered22)

summary(iModel2)
confint(iModel2)

anova(Model2, iModel2)

# Use the Model to Predict Put Away Rate and Compare with Actual
predRate <- predict(iModel2)

plot(x = filtered22$`Put Away Rate`, y = predRate)

# Root Mean Squared Error
rmse <- sqrt(mean((filtered22$`Put Away Rate` - predRate)^2))

# Mean Absolute Error
mae <- mean(abs(filtered22$`Put Away Rate` - predRate))

# residual plot
residRate <- filtered22$`Put Away Rate` - predRate
plot(predRate, residRate, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")


#
# Chase Percentage Model
Model4 <- lm(Chase ~ HorzApprAngle + IndVertBrk + RelZ +
               VertApprAngle  + Vel, data = filtered22)

summary(Model4)

iModel4 <- lm(Chase ~ HorzApprAngle  +
                 VertApprAngle  + Vel + VertApprAngle:HorzApprAngle, data = filtered22)

summary(iModel4)
confint(iModel4)

# Use the Model to Predict Chase percent and Compare with Actual
predChase <- predict(iModel4)

plot(x = filtered22$Chase, y = predChase)

# Root Mean Squared Error
rmse <- sqrt(mean((filtered22$Chase - predChase)^2))

# Mean Absolute Error
mae <- mean(abs(filtered22$Chase - predChase))

# residual plot
residChase <- filtered22$Chase - predChase
plot(predChase, residChase, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")


#
# Opponent Batting Average Model
Model5 <- lm(BA ~ `Middle HorzApprAngle` + VertApprAngle + Spin + Extension
                 + RelX + Vel, data = filtered22)

summary(Model5)

iModel5 <- lm(BA ~ `Middle HorzApprAngle` + VertApprAngle + Spin + Extension
               + Vel  + `Middle HorzApprAngle`:Vel, data = filtered22)

summary(iModel5)
confint(iModel5)

anova(Model5, iModel5)

# Use the Model to Predict Chase percent and Compare with Actual
predBA <- predict(iModel5)

plot(x = filtered22$BA, y = predBA)

# Root Mean Squared Error
rmse <- sqrt(mean((filtered22$BA - predBA)^2))

# Mean Absolute Error
mae <- mean(abs(filtered22$BA - predBA))

# residual plot
residBA <- filtered22$BA - predBA
plot(predBA, residBA, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")



#
# 2023
#

# Correlation Matrix
cor(filtered23)

# Checking the appropriate amount of variables to use in each model
library(leaps)

X=cbind(filtered23$`Middle HorzApprAngle`,filtered23$HorzApprAngle,
        filtered23$`VertApprAngle Middle Height`, filtered23$VertApprAngle
        , filtered23$Extension, filtered23$RelZ, 
        filtered23$RelX, filtered23$Vel, filtered23$Spin, 
        filtered23$HorzBrk, filtered23$IndVertBrk)

leaps(X,filtered23$BA,method='r2',nbest=1)

#
# Miss Percentage Model
Model1 <- lm(Miss ~  Spin + Vel + HorzApprAngle +
             + VertApprAngle, data = filtered23)

summary(Model1)

# Interaction Effects
iModel1 <- lm(Miss ~ Spin + Vel  +
              + VertApprAngle  + Vel:VertApprAngle, data = filtered23)
summary(iModel1)
confint(iModel1)

# Checking if the Interaction is significant with ANOVA table
anova(Model1, iModel1)

# Use the Model to Predict Miss% and Compare with Actual
predMiss <- predict(iModel1)

plot(x = filtered23$Miss, y = predMiss)

# Root Mean Squared Error
rmse <- sqrt(mean((filtered23$Miss - predMiss)^2))

# Mean Absolute Error
mae <- mean(abs(filtered23$Miss - predMiss))

# residual plot
residMiss <- filtered23$Miss - predMiss
plot(predMiss, residMiss, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")

#
# Put Away Rate Model
Model2 <- lm(`Put Away Rate` ~ HorzBrk + Spin + Vel
             + `VertApprAngle Middle Height`
             , data = filtered23)

summary(Model2)

# Interaction Effects
iModel2 <- lm(`Put Away Rate` ~ HorzBrk + Spin + Vel
              + `VertApprAngle Middle Height`
              + `VertApprAngle Middle Height`:Vel
              , data = filtered23)

summary(iModel2)
confint(iModel2)

anova(Model2, iModel2)

# Use the Model to Predict Put Away Rate and Compare with Actual
predRate <- predict(iModel2)

plot(x = filtered23$`Put Away Rate`, y = predRate)

# Root Mean Squared Error
rmse <- sqrt(mean((filtered23$`Put Away Rate` - predRate)^2))

# Mean Absolute Error
mae <- mean(abs(filtered23$`Put Away Rate` - predRate))

# residual plot
residRate <- filtered23$`Put Away Rate` - predRate
plot(predRate, residRate, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")



#
# Chase Percentage Model
Model4 <- lm(Chase ~ Spin + IndVertBrk + HorzBrk +
               VertApprAngle + RelZ  + Vel, data = filtered23)

summary(Model4)

iModel4 <- lm(Chase ~ Spin  + HorzBrk +
                VertApprAngle  + Vel + VertApprAngle:Vel, data = filtered23)

summary(iModel4)
confint(iModel4)

# Use the Model to Predict Chase percent and Compare with Actual
predChase <- predict(iModel4)

plot(x = filtered23$Chase, y = predChase)

# Root Mean Squared Error
rmse <- sqrt(mean((filtered23$Chase - predChase)^2))

# Mean Absolute Error
mae <- mean(abs(filtered23$Chase - predChase))

# residual plot
residChase <- filtered23$Chase - predChase
plot(predChase, residChase, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")


#
# Opponent Batting Average Model
Model5 <- lm(BA ~ IndVertBrk  + Vel + HorzBrk, data = filtered23)

summary(Model5)
confint(Model5)


# Use the Model to Predict Chase percent and Compare with Actual
predBA <- predict(Model5)

plot(x = filtered23$BA, y = predBA)

# Root Mean Squared Error
rmse <- sqrt(mean((filtered23$BA - predBA)^2))

# Mean Absolute Error
mae <- mean(abs(filtered23$BA - predBA))

# residual plot
residBA <- filtered23$BA - predBA
plot(predBA, residBA, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")



#
# Checking Normality Assumptions for each model
hist(residChase)
plot(density(residChase), xlab = "Residuals", ylab = "Density", main = "Density Estimate of Residuals")
qqnorm(residChase)
qqline(residBA,col="RED",lwd=2)

# X-vars vs residuals on y-axis
plot(filtered23$RelX, residChase)
plot(filtered22$RelX, residChase)
plot(filtered21$Vel, residChase)



#
# Predictive Modeling
#

# Load caret package
library(caret)

# Model For Put Away Rate
set.seed(123)  # For reproducibility

# Splitting data into training, validation,and test sets
trainIndex <- createDataPartition(filter_combined$`Put Away Rate`,
                                  p = 0.75, list = FALSE)
final_train_set <- filter_combined[trainIndex, ]
train_set <- filter_combined[-trainIndex, ]

# Second Split For Validation and Test Set
train_index <- sample(1:nrow(train_set), 0.5 * nrow(train_set))
validation_set <- train_set[-train_index, ]
test_set <- train_set[train_index, ]
# Train the Model
TrainModel <- train(`Put Away Rate` ~ HorzBrk + Spin + Vel
                    + `VertApprAngle Middle Height`  + year
                    + HorzBrk:`VertApprAngle Middle Height`, data = final_train_set, method = "lm",
                    trControl = trainControl(method = "repeatedcv", number = 10,
                                             repeats = 5))
summary(TrainModel)



# Model evaluation on the validation set
validation_predictions <- predict(TrainModel, newdata = validation_set)
summary(validation_predictions)


# Final model evaluation on the test set
test_predictions <- predict(TrainModel, newdata = test_set)
summary(test_predictions)


# Define a threshold for closeness
threshold <- 0.05  

# Calculate the absolute difference between predicted and actual values
validation_diff <- abs(validation_predictions - validation_set$`Put Away Rate`)
test_diff <- abs(test_predictions - test_set$`Put Away Rate`)

# Count the number of predictions within the threshold
validation_close <- sum(validation_diff <= threshold)
test_close <- sum(test_diff <= threshold)

# Calculate the percentage of close predictions
validation_accuracy <- validation_close / length(validation_predictions) * 100
test_accuracy <- test_close / length(test_predictions) * 100
validation_accuracy
test_accuracy

# Random Player Stats
new_player <- data.frame(
  HorzBrk = runif(1, 0, 19),
  Spin = runif(1, 1530, 3100),
  Vel = runif(1, 73.6, 87.7),
  `VertApprAngle Middle Height` = runif(1, -8.7, -4.7),
  year = sample(1:3, 1), check.names = FALSE
)

predicted_outcome <- predict(TrainModel, newdata = new_player)
predicted_outcome


#
# Model For Miss Percentage

set.seed(123)  # For reproducibility


# Splitting data into training, validation,and test sets
trainIndexMiss <- createDataPartition(filter_combined$Miss,
                                  p = 0.75, list = FALSE)
final_train_set_Miss <- filter_combined[trainIndexMiss, ]
train_set_Miss <- filter_combined[-trainIndexMiss, ]

# Second Split For Validation and Test Set
train_index_Miss <- sample(1:nrow(train_set_Miss), 0.5 * nrow(train_set_Miss))
test_set_Miss <- train_set_Miss[train_index_Miss, ]
validation_set_Miss <- train_set_Miss[-train_index_Miss, ]

# Train the Model
TrainModelMiss <- train(Miss ~ selected_pcs + Spin + Vel
                        + VertApprAngle  + selected_pcs:VertApprAngle
                        , data = final_train_set_Miss, method = "lm",
                    trControl = trainControl(method = "repeatedcv", number = 10,
                                             repeats = 5))
summary(TrainModelMiss)



# Model evaluation on the validation set
validation_predictions_Miss <- predict(TrainModelMiss, newdata = validation_set_Miss)
summary(validation_predictions_Miss)


# Final model evaluation on the test set
test_predictions_Miss <- predict(TrainModelMiss, newdata = test_set_Miss)
summary(test_predictions_Miss)


# Define a threshold for closeness
threshold <- 0.05  

# Calculate the absolute difference between predicted and actual values
validation_diff <- abs(validation_predictions_Miss - validation_set_Miss$Miss)
test_diff <- abs(test_predictions_Miss - test_set_Miss$Miss)

# Count the number of predictions within the threshold
validation_close <- sum(validation_diff <= threshold)
test_close <- sum(test_diff <= threshold)

# Calculate the percentage of close predictions
validation_accuracy <- validation_close / length(validation_predictions_Miss) * 100
test_accuracy <- test_close / length(test_predictions_Miss) * 100
validation_accuracy
test_accuracy

# Random Player Stats
new_player_Miss <- data.frame(
  HorzApprAngle = runif(1, .52, 6.41),
  Spin = runif(1, 1530, 3100),
  Vel = runif(1, 73.6, 87.7),
  VertApprAngle = runif(1, -9.6, -4.88), check.names = FALSE
)

predicted_outcome_Miss <- predict(TrainModelMiss, newdata = new_player_Miss)
predicted_outcome_Miss



#
# Model For Chase Percentage

set.seed(123)  # For reproducibility


# Splitting data into training, validation,and test sets
trainIndexChase <- createDataPartition(filter_combined$Chase,
                                      p = 0.75, list = FALSE)
final_train_set_Chase <- filter_combined[trainIndexChase, ]
train_set_Chase <- filter_combined[-trainIndexChase, ]

# Second Split For Validation and Test Set
train_index_Chase <- sample(1:nrow(train_set_Chase), 0.5 * nrow(train_set_Chase))
test_set_Chase <- train_set_Chase[train_index_Chase, ]
validation_set_Chase <- train_set_Chase[-train_index_Chase, ]

# Train the Model
TrainModelChase <- train(Chase ~ HorzApprAngle + Spin + IndVertBrk + 
                           VertApprAngle + Vel + VertApprAngle:HorzApprAngle,
                         data = final_train_set_Chase, method = "lm",
                        trControl = trainControl(method = "repeatedcv", number = 10,
                                                 repeats = 5))
summary(TrainModelChase)



# Model evaluation on the validation set
validation_predictions_Chase <- predict(TrainModelChase, newdata = validation_set_Chase)
summary(validation_predictions_Chase)


# Final model evaluation on the test set
test_predictions_Chase <- predict(TrainModelChase, newdata = test_set_Chase)
summary(test_predictions_Chase)


# Define a threshold for closeness
threshold <- 0.05  

# Calculate the absolute difference between predicted and actual values
validation_diff <- abs(validation_predictions_Chase - validation_set_Chase$Chase)
test_diff <- abs(test_predictions_Chase - test_set_Chase$Chase)

# Count the number of predictions within the threshold
validation_close <- sum(validation_diff <= threshold)
test_close <- sum(test_diff <= threshold)

# Calculate the percentage of close predictions
validation_accuracy <- validation_close / length(validation_predictions_Chase) * 100
test_accuracy <- test_close / length(test_predictions_Chase) * 100
validation_accuracy
test_accuracy

# Random Player Stats
new_player_Chase <- data.frame(
  HorzApprAngle = runif(1, .52, 6.41),
  Spin = runif(1, 1530, 3100),
  IndVertBrk = runif(1, -8.2, 10.9),
  Vel = runif(1, 73.6, 87.7),
  VertApprAngle = runif(1, -9.6, -4.88), check.names = FALSE
)

predicted_outcome_Chase <- predict(TrainModelChase, newdata = new_player_Chase)
predicted_outcome_Chase




#
# Model For Batting Average

set.seed(123)  # For reproducibility


# Splitting data into training, validation,and test sets
trainIndexBA <- createDataPartition(filter_combined$BA,
                                       p = 0.75, list = FALSE)
final_train_set_BA <- filter_combined[trainIndexBA, ]
train_set_BA <- filter_combined[-trainIndexBA, ]

# Second Split For Validation and Test Set
train_index_BA <- sample(1:nrow(train_set_BA), 0.5 * nrow(train_set_BA))
test_set_BA <- train_set_BA[train_index_BA, ]
validation_set_BA <- train_set_BA[-train_index_BA, ]

# Train the Model
TrainModelBA <- train(BA ~ `VertApprAngle Middle Height` + Spin + year
                      + Vel + HorzBrk,
                         data = final_train_set_BA, method = "lm",
                         trControl = trainControl(method = "repeatedcv", number = 10,
                                                  repeats = 5))
summary(TrainModelBA)



# Model evaluation on the validation set
validation_predictions_BA <- predict(TrainModelBA, newdata = validation_set_BA)
summary(validation_predictions_BA)


# Final model evaluation on the test set
test_predictions_BA <- predict(TrainModelBA, newdata = test_set_BA)
summary(test_predictions_BA)


# Define a threshold for closeness
threshold <- 0.05  

# Calculate the absolute difference between predicted and actual values
validation_diff <- abs(validation_predictions_BA - validation_set_BA$BA)
test_diff <- abs(test_predictions_BA - test_set_BA$BA)

# Count the number of predictions within the threshold
validation_close <- sum(validation_diff <= threshold)
test_close <- sum(test_diff <= threshold)

# Calculate the percentage of close predictions
validation_accuracy <- validation_close / length(validation_predictions_BA) * 100
test_accuracy <- test_close / length(test_predictions_BA) * 100
validation_accuracy
test_accuracy

# Random Player Stats
new_player_BA <- data.frame(
  `VertApprAngle Middle Height` = runif(1, -8.77, -4.77),
  Spin = runif(1, 1530, 3100),
  year = sample(1:3, 1),
  Vel = runif(1, 73.6, 87.7),
  HorzBrk = runif(1, 0, 19.1), check.names = FALSE
)

predicted_outcome_BA <- predict(TrainModelBA, newdata = new_player_BA)
predicted_outcome_BA


# Test for Multilinearity
# Calculate correlation matrix
cor_matrix <- cor(filter_combined[,-(25:26)])

# Visualize correlation matrix
library(corrplot)
corrplot(cor_matrix, method = "color")

# load the car package
library(car)

# Calculate VIF values
vif_values <- vif(Model1)
vif_values
variance_prop <- 1/(1 - vif_values^2)

# VIF for interactions
ivif_values <- vif(iModel1, type = 'predictor')


#
# Principal Component Analysis For Horizontal and Vertical Variables

# Extract the numeric variables from the data frame
horizontal_vars <- filter_combined[,c(1,2,7,20)]
# Perform PCA
pca_result <- prcomp(horizontal_vars, scale = TRUE, center = TRUE)
# Explore the PCA results
# Access the principal components (PCs)
pc_scores <- pca_result$x

# Access the standard deviations of the PCs (variances explained)
pc_variances <- pca_result$sdev^2

# Access the proportion of variance explained by each PC
pc_variances_prop <- pc_variances / sum(pc_variances)

# Access the correlations between variables and PCs
pc_loadings <- pca_result$rotation

# Get a Summary of the PCA
summary(pca_result)

# Interpret the PCA results
# Analyze the variance explained by each PC
cumulative_var <- cumsum(pc_variances_prop)
plot(1:length(pc_variances_prop), cumulative_var, type = "b", xlab = "Number of PCs", ylab = "Cumulative Variance Explained")

# Identify the number of PCs that explain a desired amount of variance
desired_variance <- 0.8
num_pcs <- sum(cumulative_var <= desired_variance) 

# Select the desired number of PCs to retain
selected_pcs <- pc_scores[, 1:num_pcs]


# Create an Updated Matrix with the PCs

Horz_PCframe <- data.frame(horizontal_vars, selected_pcs)



# Extract the numeric variables from the data frame
vertical_vars <- filter_combined[,c(3,4,6)]
# Perform PCA
pca_result <- prcomp(vertical_vars, scale = TRUE, center = TRUE)
# Explore the PCA results
# Access the principal components (PCs)
pc_scores <- pca_result$x

# Access the standard deviations of the PCs (variances explained)
pc_variances <- pca_result$sdev^2

# Access the proportion of variance explained by each PC
pc_variances_prop <- pc_variances / sum(pc_variances)

# Access the correlations between variables and PCs
pc_loadings <- pca_result$rotation

# Get a Summary of the PCA
summary(pca_result)

# Interpret the PCA results
# Analyze the variance explained by each PC
cumulative_var <- cumsum(pc_variances_prop)
plot(1:length(pc_variances_prop), cumulative_var, type = "b", xlab = "Number of PCs", ylab = "Cumulative Variance Explained")

# Identify the number of PCs that explain a desired amount of variance
desired_variance <- 0.8
num_pcs <- sum(cumulative_var <= desired_variance) 

# Select the desired number of PCs to retain
vselected_pcs <- pc_scores[, 1:num_pcs]


# Create an Updated Matrix with the PCs

Vert_PCframe <- data.frame(vertical_vars, vselected_pcs)

# Add the PCs for Horizontal and Vertical to the Filter Combined Frame
filter_combined <- data.frame(filter_combined, selected_pcs, vselected_pcs, check.names = FALSE)




#
# Random Forest Modeling
#
library(randomForest)

# Fixing Column Names so the random forest function can interpret it
colnames(filter_combined) <- gsub(" ", "", colnames(filter_combined))
colnames(filter_combined) <- gsub("/", "", colnames(filter_combined))

# Split the data into train and test sets
set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(filter_combined), 0.7 * nrow(filter_combined))  # 70% for training
train_data <- filter_combined[train_indices, ]
test_data <- filter_combined[-train_indices, ]

# Creating random forest model for Miss
rf <- randomForest(Miss ~ MiddleHorzApprAngle + HorzApprAngle
                   + VertApprAngleMiddleHeight + VertApprAngle
                   + Extension + RelZ + RelX + Vel + Spin + HorzBrk
                   + IndVertBrk  + year, data = filter_combined)
rf

# Predict using the model
predictions <- predict(rf, newdata = test_data)
predictions

# Check the importance of each variable
importance(rf)

# Calculate mse, mae, and rsquared
mse <- mean((predictions - test_data$Miss) ^ 2)
mse
mae <- mean(abs(predictions - test_data$Miss))
mae

ssr <- sum((predictions - mean(test_data$Miss)) ^ 2)
sst <- sum((test_data$Miss - mean(test_data$Miss)) ^ 2)

rsquared <- (1 - (ssr / sst))
rsquared


# Creating random forest model for Put Away Rate
rf <- randomForest(PutAwayRate ~ MiddleHorzApprAngle + HorzApprAngle
                   + VertApprAngleMiddleHeight + VertApprAngle
                   + Extension + RelZ + RelX + Vel + Spin + HorzBrk
                   + IndVertBrk  + year, data = filter_combined)
rf

# Predict using the model
predictions <- predict(rf, newdata = test_data)
predictions

# Check the importance of each variable
importance(rf)

# Calculate mse, mae, and rsquared
mse <- mean((predictions - test_data$PutAwayRate) ^ 2)
mse
mae <- mean(abs(predictions - test_data$PutAwayRate))
mae

ssr <- sum((predictions - mean(test_data$PutAwayRate)) ^ 2)
sst <- sum((test_data$PutAwayRate - mean(test_data$PutAwayRate)) ^ 2)

rsquared <- (1 - (ssr / sst))
rsquared


# Creating random forest model for Chase
rf <- randomForest(Chase ~ MiddleHorzApprAngle + HorzApprAngle
                   + VertApprAngleMiddleHeight + VertApprAngle
                   + Extension + RelZ + RelX + Vel + Spin + HorzBrk
                   + IndVertBrk  + year, data = filter_combined)
rf

# Predict using the model
predictions <- predict(rf, newdata = test_data)
predictions

# Check the importance of each variable
importance(rf)

# Calculate mse, mae, and rsquared
mse <- mean((predictions - test_data$Chase) ^ 2)
mse
mae <- mean(abs(predictions - test_data$Chase))
mae

ssr <- sum((predictions - mean(test_data$Chase)) ^ 2)
sst <- sum((test_data$Chase - mean(test_data$Chase)) ^ 2)

rsquared <- (1 - (ssr / sst))
rsquared



# Creating random forest model for Batting Average
rf <- randomForest(BA ~ MiddleHorzApprAngle + HorzApprAngle
                   + VertApprAngleMiddleHeight + VertApprAngle
                   + Extension + RelZ + RelX + Vel + Spin + HorzBrk
                   + IndVertBrk  + year, data = filter_combined)
rf

# Predict using the model
predictions <- predict(rf, newdata = test_data)
predictions

# Check the importance of each variable
importance(rf)

# Calculate mse, mae, and rsquared
mse <- mean((predictions - test_data$BA) ^ 2)
mse
mae <- mean(abs(predictions - test_data$BA))
mae

ssr <- sum((predictions - mean(test_data$BA)) ^ 2)
sst <- sum((test_data$BA - mean(test_data$BA)) ^ 2)

rsquared <- (1 - (ssr / sst))
rsquared
