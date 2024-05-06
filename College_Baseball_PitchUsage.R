# Importing Power 5 College Baseball Pitch Usage data from 2021, 2022, 2023
# at the team level

# Reading in all the sheets from the excel workbook

# 2023 data
Pitch_Usage23 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Summer_Research\\Pitch Usage Rates Power 5 Schools.xlsx",
                                   sheet = 1)
# 2022 data
Pitch_Usage22 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Summer_Research\\Pitch Usage Rates Power 5 Schools.xlsx",
                                   sheet = 2)
# 2021 data
Pitch_Usage21 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Summer_Research\\Pitch Usage Rates Power 5 Schools.xlsx",
                                   sheet = 3)
# Import Win, Loss, RPI(rating percentage index) data for the 61 teams 
# We have Pitch Usage Data on, so we can see if different pitch
# usage correlates to more wins

# 2023 wins
win_data_23 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Summer_Research\\College Baseball Win Loss Data.xlsx",
                                  sheet = 1)
# 2022 wins
win_data_22 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Summer_Research\\College Baseball Win Loss Data.xlsx",
                                  sheet = 2)
# 2021 wins
win_data_21 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Summer_Research\\College Baseball Win Loss Data.xlsx",
                                  sheet = 3)

# Import Hits allowed per nine innings data

# Merge the pitch usage data with the win data by team abbreviation name
# Have each years data in one data frame

# 2023
pitch_win_data23 <- merge.data.frame(Pitch_Usage23, win_data_23, 
                                     by = "teamAbbrevName")
# 2022
pitch_win_data22 <- merge.data.frame(Pitch_Usage22, win_data_22, 
                                     by = "teamAbbrevName")
# 2021
pitch_win_data21 <- merge.data.frame(Pitch_Usage21, win_data_21, 
                                     by = "teamAbbrevName")

# Install dplyr package to use pipes

library(dplyr)

# Remove columns with variables we do not need with the select function
# and negate the selection of the columns that need to be removed

# Also use the mutate function to 
# create new variable for each year calculating win percentage
# and convert the teamId variable to character type from numeric


# 2023 cleaned data frame
pitch_win_data23 <- pitch_win_data23 %>% 
  select(-Rank, -teamName, -team, -teamLevel, -synergyTeamId, 
         -newestTeamId, -newestTeamLevel, -newestTeamName,
         -newestTeamAbbrevName, -newestTeamLocation) %>% 
  mutate(win_percentage = wins / (wins + losses))%>% 
  mutate(teamId = as.character(teamId)) %>% 
  mutate(year = "2023") %>% 
  rename(`SL K` = `SL K%`, `BreakingBall K` = `BreakingBall K%`, BreakingBall = `BreakingBall%`,
         `CB K` = `CB K%`, `CH K` = `CH K%`, `FB K` = `FB K%`, FB = `FB%`)

# 2022 cleaned data frame
pitch_win_data22 <- pitch_win_data22 %>% 
  select(-Rank, -teamName, -team, -teamLevel, -synergyTeamId, 
         -newestTeamId, -newestTeamLevel, -newestTeamName,
         -newestTeamAbbrevName, -newestTeamLocation) %>% 
  mutate(win_percentage = wins / (wins + losses))%>% 
  mutate(teamId = as.character(teamId)) %>% 
  mutate(year = "2022") %>% 
  rename(`SL K` = `SL K%`, `BreakingBall K` = `BreakingBall K%`, BreakingBall = `BreakingBall%`,
         `CB K` = `CB K%`, `CH K` = `CH K%`, `FB K` = `FB K%`, FB = `FB%`)

# 2021 cleaned data frame
pitch_win_data21 <- pitch_win_data21 %>% 
  select(-Rank, -teamName, -team, -teamLevel, -synergyTeamId, 
         -newestTeamId, -newestTeamLevel, -newestTeamName,
         -newestTeamAbbrevName, -newestTeamLocation) %>% 
  mutate(win_percentage = wins / (wins + losses))%>% 
  mutate(teamId = as.character(teamId)) %>% 
  mutate(year = "2021") %>% 
  rename(`SL K` = `SL K%`, `BreakingBall K` = `BreakingBall K%`, BreakingBall = `BreakingBall%`,
         `CB K` = `CB K%`, `CH K` = `CH K%`, `FB K` = `FB K%`, FB = `FB%`)


#Exploratory Data Analysis

# Identify rows with missing values in the dataset for 2023, 2022, and 2021

pitch_win_data23[!complete.cases(pitch_win_data23), ]
pitch_win_data22[!complete.cases(pitch_win_data22), ]
pitch_win_data21[!complete.cases(pitch_win_data21), ]

# Explore Basic Summary Statistics
# Use a for loop to get the summary statistics of each numeric variable in 2023

# Separate the variables that are numeric for 2023
numeric_columns23 <- names(pitch_win_data23)[sapply(pitch_win_data23, is.numeric)]

# Loop through each numeric column and display the summary statistics to the Console
for (col in numeric_columns23) {
  cat("Summary statistics for", col, ":\n")
  print(summary(pitch_win_data23[[col]]))
  cat("\n")
}

# Use a for loop to get the standard deviation for each numeric variable

# Create an empty vector to store the standard deviations
std_deviations <- vector("double", length(numeric_columns23))

# Loop through each numeric column
for (i in 1:length(numeric_columns23)) {
  column <- pitch_win_data23[[numeric_columns23[i]]]
  std_deviations[i] <- sd(column, na.rm = TRUE)
}

# Display the standard deviations to the console
for (i in 1:length(numeric_columns23)) {
  cat("Standard deviation of", numeric_columns23[i], ":", std_deviations[i], "\n")
}



# Use a for loop to get the summary statistics of each numeric variable in 2022

# Separate the variables that are numeric for 2022
numeric_columns22 <- names(pitch_win_data22)[sapply(pitch_win_data22, is.numeric)]

# Loop through each numeric column and display the summary statistics to the Console
for (col in numeric_columns22) {
  cat("Summary statistics for", col, ":\n")
  print(summary(pitch_win_data22[[col]]))
  cat("\n")
}

# Use a for loop to get the standard deviation for each numeric variable

# Create an empty vector to store the standard deviations
std_deviations <- vector("double", length(numeric_columns22))

# Loop through each numeric column
for (i in 1:length(numeric_columns22)) {
  column <- pitch_win_data22[[numeric_columns22[i]]]
  std_deviations[i] <- sd(column, na.rm = TRUE)
}

# Display the standard deviations to the console
for (i in 1:length(numeric_columns22)) {
  cat("Standard deviation of", numeric_columns22[i], ":", std_deviations[i], "\n")
}



# Use a for loop to get the summary statistics of each numeric variable in 2022

# Separate the variables that are numeric for 2022
numeric_columns21 <- names(pitch_win_data21)[sapply(pitch_win_data21, is.numeric)]

# Loop through each numeric column and display the summary statistics to the Console
for (col in numeric_columns21) {
  cat("Summary statistics for", col, ":\n")
  print(summary(pitch_win_data21[[col]]))
  cat("\n")
}

# Use a for loop to get the standard deviation for each numeric variable

# Create an empty vector to store the standard deviations
std_deviations <- vector("double", length(numeric_columns21))

# Loop through each numeric column
for (i in 1:length(numeric_columns21)) {
  column <- pitch_win_data21[[numeric_columns21[i]]]
  std_deviations[i] <- sd(column, na.rm = TRUE)
}


# Display the standard deviations to the console
for (i in 1:length(numeric_columns21)) {
  cat("Standard deviation of", numeric_columns21[i], ":", std_deviations[i], "\n")
}


# Create Box plots and Histograms to analyze the numeric variables

# For the histogram check what the modes are and if the distribution is
# approximately normal

# 2023
# Use a for loop to get the box plots and histograms for each variable

# Save the box plots and histograms as a pdf
#pdf(file = "C:\\Users\\kearn\\R Projects\\Summer_Research\\Usage23Plots.pdf")

for (col in numeric_columns23) {
  plot.new()
  boxplot(pitch_win_data23[[col]], horizontal = TRUE,
          main = paste("2023 Boxplot of", col))
  hist(pitch_win_data23[[col]], main = paste("2023 Histogram of", col))
}
# Creates the file
#dev.off


# See how many outliers exist for any variable in 2023
outliers <- boxplot(pitch_win_data23$`Exit Velo BreakingBall`,
                    plot = FALSE)$out
length(outliers)


# 2022
# Use a for loop to get the box plots and histograms for each variable

# Save the box plots and histograms as a pdf
#pdf(file = "C:\\Users\\kearn\\R Projects\\Summer_Research\\Usage22Plots.pdf")

for (col in numeric_columns22) {
  plot.new()
  boxplot(pitch_win_data22[[col]], horizontal = TRUE,
          main = paste("2022 Boxplot of", col))
  hist(pitch_win_data22[[col]], main = paste("2022 Histogram of", col))
}
# Creates the file
#dev.off

# See how many outliers exist for 2022
outliers <- boxplot(pitch_win_data22$`Exit Velo BreakingBall`,
                    plot = FALSE)$out
length(outliers)


# 2021
# Use a for loop to get the box plots and histograms for each variable

# Save the box plots and histograms as a pdf
#pdf(file = "C:\\Users\\kearn\\R Projects\\Summer_Research\\Usage21Plots.pdf")

for (col in numeric_columns21) {
  plot.new()
  boxplot(pitch_win_data21[[col]], horizontal = TRUE,
          main = paste("2021 Boxplot of", col))
  hist(pitch_win_data21[[col]], main = paste("2021 Histogram of", col))
}
# Creates the file
#dev.off

# See how many outliers exist for 2021
outliers <- boxplot(pitch_win_data21$`Exit Velo BreakingBall`,
                    plot = FALSE)$out
length(outliers)


# Starting Bivariate Exploratory Data Analysis

# Create pairwise scatter plots to analyze bivariate numeric variables
# using only columns with numeric variables

pairs(pitch_win_data23[, c(13:24,30)])
pairs(pitch_win_data22[, c(9:24,27)])
pairs(pitch_win_data21[, c(9:24,27)])

# Create correlation Matrices

cor(pitch_win_data23[, 5:30], method = "pearson", use = "pairwise.complete.obs")
cor(pitch_win_data22[, 5:30], method = "pearson", use = "pairwise.complete.obs")
cor(pitch_win_data21[, 5:30], method = "pearson", use = "pairwise.complete.obs")


# Create and Visualize a correlation matrix that checks for 
# non-linear relationships within the numeric variables

# Load in the packages for use
library(corrplot)
library(Hmisc)

# Checks for non-linear association in each year
rcorr(as.matrix(pitch_win_data23[, 5:30]))
rcorr(as.matrix(pitch_win_data22[, 5:30]))
rcorr(as.matrix(pitch_win_data21[, 5:30]))

# Assign the Matrices for visualization
cor_23 <- rcorr(as.matrix(pitch_win_data23[, 5:30]))
cor_22 <- rcorr(as.matrix(pitch_win_data22[, 5:30]))
cor_21 <- rcorr(as.matrix(pitch_win_data21[, 5:30]))

# Visualize the Correlation matrix for all 3 years
corrplot(cor_23$r, method = "color", type = "full", tl.cex = .5)
corrplot(cor_22$r, method = "color", type = "full", tl.cex = .5)
corrplot(cor_21$r, method = "color", type = "full", tl.cex = .5)



# Side-by-side box plots for each variable in each year

# Save the plots as a pdf
pdf(file = "C:\\Users\\kearn\\R Projects\\Summer_Research\\Pitch Usage yearly comparison Box Plots.pdf")

# For loop to create the plots for each variable in 2023
for (col in numeric_columns23) {
  plot.new()
  # Create a list of the three years
  data <- list(pitch_win_data21[[col]], pitch_win_data22[[col]], pitch_win_data23[[col]])
  # Set the parameters to a 1 by 3
  par(mfrow = c(1, 3))
  # Set the same y-axis limits for all three boxplots
  ylim <- range(unlist(data))
  # Box plots for each year
  boxplot(pitch_win_data21[[col]], main = paste("2021", col), ylim = ylim)
  boxplot(pitch_win_data22[[col]], main = paste("2022", col), ylim = ylim)
  boxplot(pitch_win_data23[[col]], main = paste("2023", col), ylim = ylim)
}
# Creates file
dev.off()

# Create plots over time with the sample mean of each variable with medians overlaid
# Load in ggplot
library(ggplot2)

# Save the plots as a pdf
#pdf(file = "C:\\Users\\kearn\\R Projects\\Summer_Research\\Pitch Usage Variable Mean & Median Over Time.pdf")

# Create a for loop to graph each variable
for (col in numeric_columns23) {
  # Get the mean for each year
  means <- c(mean(pitch_win_data21[[col]]),
             mean(pitch_win_data22[[col]]),
             mean(pitch_win_data23[[col]]))
  
  # Create a vector of the years
  year <- c(2021, 2022, 2023)
  
  # Get the median for each year
  medians <- c(median(pitch_win_data21[[col]]),
               median(pitch_win_data22[[col]]),
               median(pitch_win_data23[[col]]))
  
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



# For each pair of numeric variables plot the correlation estimates
# over the years

cor_matrix2023 <- cor(pitch_win_data23[, 5:30], method = "pearson", use = "pairwise.complete.obs")
cor_matrix2022 <- cor(pitch_win_data22[, 5:30], method = "pearson", use = "pairwise.complete.obs")
cor_matrix2021 <- cor(pitch_win_data21[, 5:30], method = "pearson", use = "pairwise.complete.obs")


# Extract variable names
variables <- colnames(cor_matrix2023)

# Save the plot as a pdf
#pdf(file = "C:\\Users\\kearn\\R Projects\\Summer_Research\\Plots of Pitch Usage Data Correlations over the Years.pdf")

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
#dev.off()


# Simplify the correlation matrix to only see significant values
is.na(cor_matrix2021) <- abs(cor_matrix2021) < 0.5

is.na(cor_matrix2022) <- abs(cor_matrix2022) < 0.5

is.na(cor_matrix2023) <- abs(cor_matrix2023) < 0.5


#
# Principal Component Analysis
#

# 2023
# Extract the numeric variables from the data frame
numeric_matrix23 <- as.matrix(pitch_win_data23[,numeric_columns23])
# Remove playoff finish
numeric_matrix23 <- numeric_matrix23[,-25]
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

na23 <- pitch_win_data23[,!sapply(pitch_win_data23, is.numeric)]
PCframe23 <- data.frame(na23, selected_pcs)



# 2022
# Extract the numeric variables from the data frame
numeric_matrix22 <- as.matrix(pitch_win_data22[,numeric_columns22])
# remove playoff finish
numeric_matrix22 <- numeric_matrix22[,-25]
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

na22 <- pitch_win_data22[,!sapply(pitch_win_data22, is.numeric)]
PCframe22 <- data.frame(na22, selected_pcs)



# 2021
# Extract the numeric variables from the data frame
numeric_matrix21 <- as.matrix(pitch_win_data21[,numeric_columns21])
# Remove playoff finish
numeric_matrix21 <- numeric_matrix21[,-25]
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

na21 <- pitch_win_data21[,!sapply(pitch_win_data21, is.numeric)]
PCframe21<- data.frame(na21, selected_pcs)



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
filtered23 <- na.omit(pitch_win_data23[,sapply(pitch_win_data23, is.numeric)])
filtered23[is.na(filtered23)] <- 100

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
clusters22 <- kmeans(pcs22, 4, iter.max = 100, nstart = 1)
plot(pcs22[,1], pcs22[,2], col = clusters22$cluster)


# Hierarchical Clustering with original data
# Create filtered Data frame
filtered22 <- pitch_win_data22[,sapply(pitch_win_data22, is.numeric)]
filtered22[is.na(filtered22)] <- 100

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
filtered21 <- pitch_win_data21[,sapply(pitch_win_data21, is.numeric)]
filtered21[is.na(filtered21)] <- 100
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
kmeans_fancy21 <- kmeans(scale(filtered21), 4, nstart = 100)

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
cutree(hc_outs22, k = 4)
hc_labels22 <- cutree(hc_outs22, k = 4)
filtered22[hc_labels22 == 4, ]
summary(filtered22[hc_labels22 == 4, ])
filtered22$cluster <- hc_labels22

# 2021
cutree(hc_outs21, k = 4)
hc_labels21 <- cutree(hc_outs21, k = 4)
filtered21[hc_labels21 == 4, ]
summary(filtered21[hc_labels21 == 4, ])
filtered21$cluster <- hc_labels21


#
# Linear Discriminant Analysis
#

# Load in MASS Package
library(MASS)

# 2023
# Create the LDA Model to model the Clusters

lda_model <- lda(cluster ~ ., data = filtered23)
lda_model

# Stacked Histogram for Discriminant Function Values Checking Overlap
p <- predict(lda_model, filtered23)
ldahist(data = p$x[,1], g = filtered23$cluster)

# Confusion Matrix and Accuracy

p1 <- predict(lda_model, filtered23)$class
tab <- table(Predicted = p1, Actual = filtered23$cluster)
tab

# Accuracy of The Model
sum(diag(tab))/sum(tab)


#
# 2022
# Create the LDA Model to model the Clusters

lda_model <- lda(cluster ~ ., data = filtered22)
lda_model

# Stacked Histogram for Discriminant Function Values Checking Overlap
p <- predict(lda_model, filtered22)
ldahist(data = p$x[,1], g = filtered22$cluster)

# Confusion Matrix and Accuracy

p1 <- predict(lda_model, filtered22)$class
tab <- table(Predicted = p1, Actual = filtered22$cluster)
tab
p1
# Accuracy of The Model
sum(diag(tab))/sum(tab)


#
# 2021
# Create the LDA Model to model the Clusters

lda_model <- lda(cluster ~ ., data = filtered21)
lda_model

# Stacked Histogram for Discriminant Function Values Checking Overlap
p <- predict(lda_model, filtered21)
ldahist(data = p$x[,1], g = filtered21$cluster)

# Confusion Matrix and Accuracy

p1 <- predict(lda_model, filtered21)$class
tab <- table(Predicted = p1, Actual = filtered21$cluster)
tab

# Accuracy of The Model
sum(diag(tab))/sum(tab)


#
# Multinomial Logistic Regression
#

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


# Perform Factor Analysis with 4 factors and a Varimax Rotation

factor_analysis <- factanal(filtered21, factors = 4, rotation = "varimax")
factor_analysis

# Access the factor loading matrix
factor_analysis$loadings  

# Access the commonalities
factor_analysis$communality

#
# Multivariate Regression
#

cor(pitch_win_data21[,5:30])

# Finding Optimal Variables
library(leaps)

X=cbind(combined[,5:24])
X = cbind(combined$BreakingBall, combined$`SL Usage Rate`, combined$`CB Usage Rate`
          , combined$`CH Usage Rate`, combined$FB)
  
  
leaps(X,combined$win_percentage,method='adjr2',nbest=1)

m <- lm(RPI ~ `BreakingBall` + `CH Usage Rate` 
        , data = combined)
summary(m)
# Looking at BIC values
d <- regsubsets(RPI ~ FB, data = combined)
rs <- summary(d)
plot(rs$bic, xlab="Parameter", ylab="BIC") 
summary(d)$which


# Building a Linear Regression Model
model1 <- lm(RPI ~ `CB Swing-Miss` + `CB K` , data = combined)

summary(model1)

# Interaction Model
iModel1 <- lm(RPI ~ `CB Swing-Miss` + `CB K` + `CB Swing-Miss`:`CB K` , 
              data = combined)

summary(iModel1)

# Checking if the Interaction is significant with ANOVA table
anova(Model1, iModel1)


# Use the Model to Predict Miss% and Compare with Actual
pred <- predict(iModel1)

plot(x = combined$RPI, y = pred)

# Root Mean Squared Error
rmse <- sqrt(mean((combined$RPI - pred)^2))

# Mean Absolute Error
mae <- mean(abs(combined$RPI - pred))

# residual plot
resid <- combined$RPI - pred
plot(pred, resid, xlab = "Fitted Values (Yhat)", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")

# Check for Normality in the Model

hist(resid)
plot(density(resid), xlab = "Residuals", ylab = "Density", main = "Density Estimate of Residuals")
qqnorm(resid)
qqline(resid,col="RED",lwd=2)

# X-vars vs residuals on y-axis
plot(combined$`CB K`, resid)




#
# Remove Outliers
#


# Combined Frame for all 3 year

combined <- rbind(pitch_win_data21, pitch_win_data22, pitch_win_data23)
combined <- combined %>% 
  arrange(teamAbbrevName)

# Get outliers

# Outliers in Combined Data Frame

outlier_FBSwing <- boxplot.stats(combined$`FB Swing-Miss`)$out
combinedRemoved <- combined[!combined$`FB Swing-Miss` %in% outlier_FBSwing,]

outlier_BBExit <- boxplot.stats(combined$`Exit Velo BreakingBall`)$out
combinedRemoved <- combinedRemoved[!combinedRemoved$`Exit Velo BreakingBall` %in% outlier_BBExit,]

outlier_BBK <- boxplot.stats(combined$`BreakingBall K`)$out
combinedRemoved <- combinedRemoved[!combinedRemoved$`BreakingBall K` %in% outlier_BBK,]

outlier_CHSwing <- boxplot.stats(combined$`CH Swing-Miss`)$out
combinedRemoved <- combinedRemoved[!combinedRemoved$`CH Swing-Miss` %in% outlier_CHSwing,]

outlier_CHK <- boxplot.stats(combined$`CH K`)$out
combinedRemoved <- combinedRemoved[!combinedRemoved$`CH K` %in% outlier_CHK,]

outlier_CHExit <- boxplot.stats(combined$`Exit Velo CH`)$out
combinedRemoved <- combinedRemoved[!combinedRemoved$`Exit Velo CH` %in% outlier_CHExit,]

outlier_FBExit <- boxplot.stats(combined$`Exit Velo FB`)$out
combinedRemoved <- combinedRemoved[!combinedRemoved$`Exit Velo FB` %in% outlier_FBExit,]

outlier_FBSwing <- boxplot.stats(combined$`FB Swing_Miss`)$out
combinedRemoved <- combinedRemoved[!combinedRemoved$`FB Swing-Miss` %in% outlier_FBSwing,]

outlier_FBK <- boxplot.stats(combined$`FB K`)$out
combinedRemoved <- combinedRemoved[!combinedRemoved$`FB K` %in% outlier_FBK,]

# Making Slider comparison over the years
library(gridExtra)
library(ggplot2)
library(scales)
highlight_team <- "Wake Forest University"


# Create individual scatter plots for each year
plot_2021 <- ggplot(pitch_win_data21, aes(x = `SL Usage Rate`, y = win_percentage)) +
  geom_point(shape = ifelse(pitch_win_data21$teamFullName == highlight_team, 16, 1), color = ifelse(pitch_win_data21$teamFullName == highlight_team, "red", "black")) +
  geom_smooth(method = "loess", color = "black", size = .4, se = FALSE) +
  labs(x = "Slider %", y = "Win %", title = "2021: p = .50") +
  theme_bw()+
  theme(panel.border = element_rect(color = "black", size = 1),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))+
  scale_x_continuous(labels = percent_format(scale = 100), limits = c(.05, .4)) +
  scale_y_continuous(labels = percent_format(scale = 100), limits = c(.1, .9))


plot_2022 <- ggplot(pitch_win_data22, aes(x = `SL Usage Rate`, y = win_percentage)) +
  geom_point(shape = ifelse(pitch_win_data22$teamFullName == highlight_team, 16, 1), color = ifelse(pitch_win_data22$teamFullName == highlight_team, "red", "black")) +
  geom_smooth(method = "loess", color = "black", size = .4, se = FALSE) +
  labs(x = "Slider %", y = "Win %", title = "2022: p = .03") +
  theme_bw()+
  theme(panel.border = element_rect(color = "black", size = 1),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))+
  scale_x_continuous(labels = percent_format(scale = 100), limits = c(.05, .4)) +
  scale_y_continuous(labels = percent_format(scale = 100), limits = c(.1, .9))


plot_2023 <- ggplot(pitch_win_data23, aes(x = `SL Usage Rate`, y = win_percentage)) +
  geom_point(shape = ifelse(pitch_win_data23$teamFullName == highlight_team, 16, 1), color = ifelse(pitch_win_data23$teamFullName == highlight_team, "red", "black")) +
  geom_smooth(method = "loess", color = "black", size = .4, se = FALSE) +
  labs(x = "Slider %", y = "Win %", title = "2023: p = .81") +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", size = 1),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))+
  scale_x_continuous(labels = percent_format(scale = 100), limits = c(.05, .4)) +
  scale_y_continuous(labels = percent_format(scale = 100), limits = c(.1, .9))

  
  
# Arrange the plots side by side
grid.arrange(plot_2021, plot_2022, plot_2023, ncol = 3)