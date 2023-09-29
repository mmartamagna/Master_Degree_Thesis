#############################################################################
##  SCRIPT 2: Descriptive statistics, item-level stats, outliers detection,##
##            density plots, composite-level stats, non-parametric tests,  ##
##            box plots                                                    ##
##  Author: Marta Magnani                                                  ##                                        
##  Last: Sep 10th, 2023                                                   ##
#############################################################################


##Load the data
#Packages
library(readxl)
library(dplyr)
library(ggplot2)
library(readr)
library(writexl)  # Load the writexl package
library(RColorBrewer)
library(ggpubr)
library(tidyr)
library(MVN)
library(gridExtra)
library(psych)
library(moments)
library(corrr)
library(qgraph)
library(MVN)
library(gplots)
library(corrplot)
library(dplyr)
library(plotly)
library(RColorBrewer)
library(purrr)


#Load the data
data_ready <- read.csv("C:/Users/magna/OneDrive/Tesi Magistrale DSE/Analisi Script R/data_ready.csv")
attach(data_ready)


############ ORGANIZATIONS STATISTICS ###########

# Frequency of organizations by dimension
total_organizations <- 22  # Total number of organizations
total_rows <- nrow(data_ready)  # Total number of rows in the dataset

result0 <- data_ready %>%
  group_by(region) %>%
  summarise(
    unique_organizations = n_distinct(organization_code),
    percentage_org = (n_distinct(organization_code) / total_organizations) * 100,
    frequency_rows = n(),
    percentage_rows = (n() / total_rows) * 100
  )

# Print the result
print(result0)

result1 <- data_ready %>%
  group_by(dimension) %>%
  summarise(
    unique_organizations = n_distinct(organization_code),
    percentage_org = (n_distinct(organization_code) / total_organizations) * 100,
    frequency_rows = n(),
    percentage_rows = (n() / total_rows) * 100
  )

# Print the result
print(result1)

# Frequency of organizations by sector
result2 <- data_ready %>%
  group_by(sector) %>%
  summarise(
    unique_organizations = n_distinct(organization_code),
    percentage_org = (n_distinct(organization_code) / total_organizations) * 100,
    frequency_rows = n(),
    percentage_rows = (n() / total_rows) * 100
  )

# Print the result
print(result2)

# Frequency of organizations by juridical form
result3 <- data_ready %>%
  group_by(juridical_form) %>%
  summarise(
    unique_organizations = n_distinct(organization_code),
    percentage_org = (n_distinct(organization_code) / total_organizations) * 100,
    frequency_rows = n(),
    percentage_rows = (n() / total_rows) * 100
  )

# Print the result
print(result3)

############### RESPONDENTS STATISTICS

data_ready %>%
  group_by(dimension) %>%
  summarise(
    frequency_rows = n(),
    percentage_rows = (n() / total_rows) * 100
  )

data_ready %>%
  group_by(role) %>%
  summarise(
    frequency_rows = n(),
    percentage_rows = (n() / total_rows) * 100
  )

data_ready %>%
  group_by(gender) %>%
  summarise(
    frequency_rows = n(),
    percentage_rows = (n() / total_rows) * 100
  )

data_ready %>%
  group_by(age) %>%
  summarise(
    frequency_rows = n(),
    percentage_rows = (n() / total_rows) * 100
  )

data_ready %>%
  group_by(seniority) %>%
  summarise(
    frequency_rows = n(),
    percentage_rows = (n() / total_rows) * 100
  )

############ CONTINGENCY TABLES OF RESPONDENTS CHARACTERISTICS BY DIMENSION

# Calculate the percentages
data_ready <- data_ready %>%
  group_by(dimension, role) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

# Plot
data_ready %>%
  ggplot(aes(x = dimension, y = count, fill = role)) + 
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = sprintf("%.1f%%", percentage), y = count / 2), position = position_fill(vjust = 0.5), size = 3) + 
  labs(
    x= ' ', 
    y= 'Count (%)',
    fill = "Role"
  ) + 
  scale_fill_brewer(palette="Paired") + 
  theme_minimal() + 
  theme(text=element_text(size=11)) + 
  ggtitle("Role frequency in each dimension")

# Plot
## Calculate the percentages
data_ready <- data_ready %>%
  group_by(dimension, gender) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

data_ready %>%
  ggplot(aes(x = dimension, y = count, fill = gender)) + 
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = sprintf("%.1f%%", percentage), y = count / 2), position = position_fill(vjust = 0.5), size = 3) + 
  labs(
    x= ' ', 
    y= 'Count (%)',
    fill = "Gender"
  ) + 
  scale_fill_brewer(palette="Paired") + 
  theme_minimal() + 
  theme(text=element_text(size=11)) + 
  ggtitle("Gender frequency in each dimension")


####DENSITY DISTRIBUTION OF THE MAIN COMPOSITE MEASURES
d1 <- ggplot(data_ready, aes(bes_wellbeing)) +
  geom_histogram(aes(y = ..density..), colour = 1, fill = "white") +
  geom_density(lwd = 0.5, colour = 4, fill = 4, alpha = 0.15)  + 
  labs(x= 'Workers well-being', y= 'Density') + 
  theme_minimal() + theme(text=element_text(size=10)) + ggtitle("Workers' well-being distribution")

d2 <- ggplot(data_ready, aes(participation_level)) +
  geom_histogram(aes(y = ..density..), colour = 1, fill = "white") +
  geom_density(lwd = 0.5, colour = 4, fill = 4, alpha = 0.15)+
  labs(x= 'Participation level', y= 'Density') +
  theme_minimal() + theme(text=element_text(size=10)) + ggtitle("Participation level distribution")

d3 <- ggplot(data_ready, aes(health_organization)) +
  geom_histogram(aes(y = ..density..), colour = 1, fill = "white") +
  geom_density(lwd = 0.5, colour = 4, fill = 4, alpha = 0.15) + 
  labs(x= 'Organizational health', y= 'Density') +
  theme_minimal() + theme(text=element_text(size=10)) + ggtitle("Organizational health distribution")

d4 <- ggplot(data_ready, aes(subjective_wellbeing)) +
  geom_histogram(aes(y = ..density..), colour = 1, fill = "white") +
  geom_density(lwd = 0.5, colour = 4, fill = 4, alpha = 0.15) + 
  labs(x= 'Subjective well-being', y= 'Density') +
  theme_minimal() + theme(text=element_text(size=10)) + ggtitle("Subjective well-being distribution")

d5 <- ggplot(data_ready, aes(subjective_malaise)) +
  geom_histogram(aes(y = ..density..), colour = 1, fill = "white") +
  geom_density(lwd = 0.5, colour = 4, fill = 4, alpha = 0.15) + 
  labs(x= 'Subjective malaise', y= 'Density') +
  theme_minimal() + theme(text=element_text(size=10)) + ggtitle("Subjective malaise distribution")

d6 <- ggplot(data_ready, aes(stress)) +
  geom_histogram(aes(y = ..density..), colour = 1, fill = "white") +
  geom_density(lwd = 0.5, colour = 4, fill = 4, alpha = 0.15) + 
  labs(x= 'Absence of work-related stress', y= 'Density') +
  theme_minimal() + theme(text=element_text(size=10)) + ggtitle("Absence of work-related stress distribution")


#Create a unique plot containing these three
distrib_plot1 <- ggarrange(d1, d2,
                          labels = c("A", "B"),
                          ncol = 2, nrow = 1)
distrib_plot1

distrib_plot2 <- ggarrange(d3, d4,
                           labels = c("C", "D"),
                           ncol = 2, nrow = 1)
distrib_plot2

distrib_plot3 <- ggarrange(d5, d6, 
                           labels = c("E", "F"),
                           ncol = 2, nrow = 1)
distrib_plot3

 
########################## ITEM-LEVEL STATISTICS
##Table of Summary univariate statistics (n, mean, sd, min, max, range, se, skew, kurtosis)

relevant_columns_bes <- grep("^Q[1-9][0-9]*$", names(data_ready), value = TRUE)
relevant_columns_wellbeing <- grep("^bes_", names(data_ready), value = TRUE)
relevant_columns_part <- grep("^part_", names(data_ready), value = TRUE)
relevant_columns_health <- grep("^health_", names(data_ready), value = TRUE)
relevant_columns_subjwb <- grep("^subjective_wb_", names(data_ready), value = TRUE)
relevant_columns_subjma <- grep("^subjective_ma_", names(data_ready), value = TRUE)
relevant_columns_stress <- grep("^stress_", names(data_ready), value = TRUE)


# Subset the data
data_subset_bes <- data_ready[, relevant_columns_bes]
data_subset_wellbeing <- data_ready[, relevant_columns_wellbeing]
data_subset_part <- data_ready[, relevant_columns_part]
data_subset_health <- data_ready[, relevant_columns_health]
data_subset_subjwb <- data_ready[, relevant_columns_subjwb]
data_subset_subjma <- data_ready[, relevant_columns_subjma]
data_subset_stress <- data_ready[, relevant_columns_stress]


# Get the basic statistics using the describe function
desc_stats_bes <- describe(data_subset_bes, type=2, fast=T)
desc_stats_wellbeing <- describe(data_subset_wellbeing, type=2, fast=T)
desc_stats_part <- describe(data_subset_part,type=2,fast=T)
desc_stats_health <- describe(data_subset_health, type=2, fast=T)
desc_stats_subjwb <- describe(data_subset_subjwb,type=2,fast=T)
desc_stats_subjma <- describe(data_subset_subjma, type=2, fast=T)
desc_stats_stress <- describe(data_subset_stress,type=2,fast=T)

# Calculate skewness and kurtosis for each variable
skewness_vals_bes <- apply(data_subset_bes, 2, skewness)
kurtosis_vals_bes <- apply(data_subset_bes, 2, kurtosis)

skewness_vals_wellbeing <- apply(data_subset_wellbeing, 2, skewness)
kurtosis_vals_wellbeing <- apply(data_subset_wellbeing, 2, kurtosis)

skewness_vals_part <- apply(data_subset_part, 2, skewness)
kurtosis_vals_part <- apply(data_subset_part, 2, kurtosis)

skewness_vals_health <- apply(data_subset_health, 2, skewness)
kurtosis_vals_health <- apply(data_subset_health, 2, kurtosis)

skewness_vals_subjwb <- apply(data_subset_subjwb, 2, skewness)
kurtosis_vals_subjwb <- apply(data_subset_subjwb, 2, kurtosis)

skewness_vals_subjma <- apply(data_subset_subjma, 2, skewness)
kurtosis_vals_subjma <- apply(data_subset_subjma, 2, kurtosis)

skewness_vals_stress <- apply(data_subset_stress, 2, skewness)
kurtosis_vals_stress <- apply(data_subset_stress, 2, kurtosis)


# Add the skewness and kurtosis to the describe output
desc_stats_bes$skew <- skewness_vals_bes
desc_stats_bes$kurtosis <- kurtosis_vals_bes

desc_stats_wellbeing$skew <- skewness_vals_wellbeing
desc_stats_wellbeing$kurtosis <- kurtosis_vals_wellbeing

desc_stats_part$skew <- skewness_vals_part
desc_stats_part$kurtosis <- kurtosis_vals_part

desc_stats_health$skew <- skewness_vals_health
desc_stats_health$kurtosis <- kurtosis_vals_health

desc_stats_subjwb$skew <- skewness_vals_subjwb
desc_stats_subjwb$kurtosis <- kurtosis_vals_subjwb

desc_stats_subjma$skew <- skewness_vals_subjma
desc_stats_subjma$kurtosis <- kurtosis_vals_subjma

desc_stats_stress$skew <- skewness_vals_stress
desc_stats_stress$kurtosis <- kurtosis_vals_stress

# Show the extended table
View(desc_stats_bes)
View(desc_stats_wellbeing)
View(desc_stats_part)
View(desc_stats_health)
View(desc_stats_subjwb)
View(desc_stats_subjma)
View(desc_stats_stress)

###Summary stats of the main measures

############# MULTIVARIATE  OUTLIERS WELL-BEING
# Calculate Mahalanobis distance
mahalanobis_values_bes <- mahalanobis(data_subset_bes, colMeans(data_subset_bes, na.rm = TRUE), solve(var(data_subset_bes, na.rm = TRUE)))

# Create a data frame for plotting
plot_data_bes <- data.frame(Index = 1:length(mahalanobis_values_bes), Mahalanobis = mahalanobis_values_bes)

# Calculate a threshold (e.g., a percentile or a statistical cutoff)
threshold_bes <- quantile(mahalanobis_values_bes, 0.99)  # 99th percentile

# Calculate outliers
is_outlier_bes <- plot_data_bes$Mahalanobis > threshold_bes

# Count outliers and non-outliers
n_outliers_bes <- sum(is_outlier_bes)
n_non_outliers_bes <- nrow(plot_data_bes) - n_outliers_bes

# Add a 'status' column to the data
plot_data_bes$status <- ifelse(is_outlier_bes, "Outlier", "Non-Outlier")

# Create the plot
ggplot(plot_data_bes, aes(x = Index, y = Mahalanobis, color = status)) +
  geom_point() +
  geom_hline(yintercept = threshold_bes, color = "red", linetype = "dashed") +
  ggtitle("Mahalanobis Distance for Well-being Items Multivariate Outliers") +
  xlab("Index") +
  ylab("Mahalanobis Distance") +
  theme_minimal() +
  scale_color_manual(values = c("black", "red")) +
  annotate("text", x = Inf, y = Inf, 
           label = paste("Outliers: ", n_outliers_bes, "Non-Outliers: ", n_non_outliers_bes),
           vjust = 1, hjust = 1, color = "black", size = 4)


########### MULTIVARIATE OUTLIERS PARTICIPATION
# Calculate Mahalanobis distance
mahalanobis_values_part <- mahalanobis(data_subset_part, colMeans(data_subset_part, na.rm = TRUE), solve(var(data_subset_part, na.rm = TRUE)))

# Create a data frame for plotting
plot_data_part <- data.frame(Index = 1:length(mahalanobis_values_part), Mahalanobis = mahalanobis_values_part)

# Calculate a threshold (e.g., a percentile or a statistical cutoff)
threshold_part <- quantile(mahalanobis_values_part, 0.99)  # 99th percentile


# Calculate outliers
is_outlier_part <- plot_data_part$Mahalanobis > threshold_part

# Count outliers and non-outliers
n_outliers_part <- sum(is_outlier_part)
n_non_outliers_part <- nrow(plot_data_part) - n_outliers_part

# Add a 'status' column to the data
plot_data_part$status <- ifelse(is_outlier_part, "Outlier", "Non-Outlier")

# Create the plot
ggplot(plot_data_part, aes(x = Index, y = Mahalanobis, color = status)) +
  geom_point() +
  geom_hline(yintercept = threshold_part, color = "red", linetype = "dashed") +
  ggtitle("Mahalanobis Distance for Participation Items Multivariate Outliers") +
  xlab("Index") +
  ylab("Mahalanobis Distance") +
  theme_minimal() +
  scale_color_manual(values = c("black", "red")) +
  annotate("text", x = Inf, y = Inf, 
           label = paste("Outliers: ", n_outliers_part, "Non-Outliers: ", n_non_outliers_part),
           vjust = 1, hjust = 1, color = "black", size = 4)


############ CORRECTED ITEM TOTAL CORRELATION OF SINGLE ITEMS (127 WELL-BEING) AND 21 (PARTICIPATION)
# well-being
# Initialize a vector to store the corrected item-total correlations
rit_values <- numeric(length(relevant_columns_bes))

# Loop through each question to calculate the corrected item-total correlation
for (i in seq_along(relevant_columns_bes)) {
  # Calculate the total score without the current item
  total_score_without_current_item <- rowSums(data_subset_bes[, -i, drop = FALSE], na.rm = TRUE)
  
  # Calculate the correlation for the current item with the total score
  # Note: Use 'use="complete.obs"' to handle missing data if necessary
  rit_values[i] <- cor(data_subset_bes[, i], total_score_without_current_item, use="complete.obs")
}

# Create a data frame to show the corrected item-total correlations
rit_df <- data.frame(Question = relevant_columns_bes, rit = rit_values)

# Display or save rit_df as needed
print(rit_df)


############## COMPOSITE LEVEL SUMMARY STATISTICS
########################## ITEM-LEVEL STATISTICS
##Table of Summary univariate statistics (n, mean, sd, min, max, range, se, skew, kurtosis)

# Specify the relevant columns directly by name
relevant_columns <- c("bes_wellbeing", "participation_level", "health_organization", 
                      "subjective_wellbeing", "subjective_malaise", "stress")

data_subset <- data_ready[, relevant_columns]

# Get the basic statistics using the describe function
desc_stats <- psych::describe(data_subset, type=2, fast=T)

# Calculate skewness and kurtosis for each variable
skewness_vals <- sapply(data_subset, skewness)
kurtosis_vals <- sapply(data_subset, kurtosis)

# Add the skewness and kurtosis to the describe output
desc_stats$skew <- skewness_vals
desc_stats$kurtosis <- kurtosis_vals

# View the extended table
View(desc_stats)


# Show the extended table
View(desc_stats_bes)


#########################################
####CORRELATION OF 12 WELL-BEING DOMAINS
##########################################
library(corrplot)
data_wellbeing <- data_subset_wellbeing[, !names(data_subset_wellbeing) %in% "bes_wellbeing"]

# Rename columns with numbers
colnames(data_wellbeing) <- as.character(1:ncol(data_wellbeing))

data_wellbeing <- data_subset_wellbeing[, !names(data_subset_wellbeing) %in% "bes_wellbeing"]

# Rename columns with numbers
colnames(data_wellbeing) <- as.character(1:ncol(data_wellbeing))

colnames(data_subset_part) <- as.character(1:ncol(data_subset_part))
colnames(data_subset_health) <- as.character(1:ncol(data_subset_health))


# Compute the correlation matrix
corr_matrix_wellbeing <- cor(data_wellbeing, use = "pairwise.complete.obs")
corr_matrix_participation <- cor(data_subset_part, use = "pairwise.complete.obs")
corr_matrix_subjwb <- cor(data_subset_subjwb, use = "pairwise.complete.obs")
corr_matrix_subjma <- cor(data_subset_subjma, use = "pairwise.complete.obs")
corr_matrix_stress <- cor(data_subset_stress, use = "pairwise.complete.obs")
corr_matrix_health <- cor(data_subset_health, use = "pairwise.complete.obs")

# If you want to round the values to 3 decimal places and print them
print(round(corr_matrix_wellbeing, 3))
print(round(corr_matrix_participation, 3))
print(round(corr_matrix_health, 3))
print(round(corr_matrix_subjwb, 3))
print(round(corr_matrix_subjma, 3))
print(round(corr_matrix_stress, 3))


#### CORRELATION PLOT OF WELL-BEING DOMAINS ####

########## Define new variable names
new_var_list <- c(
  "Politics",
  "Security",
  "Health",
  "Social relationships",
  "Work-Life balance",
  "Nat-cult heritage",
  "Quality of services",
  "Economic wellbeing",
  "Innovation",
  "Education",
  "Environment",
  "Subj. well-being"
)

# Assign the new variable names to the columns of data_wellbeing
colnames(data_wellbeing) <- new_var_list

# Compute the correlation matrix
corr_matrix_wellbeing <- cor(data_wellbeing, use = "pairwise.complete.obs")

# Assign numbers as column names and actual variable names as row names in your correlation matrix
colnames(corr_matrix_wellbeing) <- as.character(1:ncol(corr_matrix_wellbeing))
rownames(corr_matrix_wellbeing) <- new_var_list

# Define color palette
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# Create the correlation plot with correlation coefficients
corrplot(corr_matrix_wellbeing, method="color", col=col(200),
         type="upper",
         order="original",
         addCoef.col = "black",number.cex = 0.9,
         tl.col="black", tl.srt=0, tl.cex = 0.9,
         #tl.pos="lt", # to place text labels on the left and top
         diag=TRUE) # to show the diagonal


####  CORRELATION PLOT OF PARTICIPATION  ###
# New names you want to give to these variables
new_var_list <- c("Org. welfare", "Org. development", "H&R", "Work organiz.",
                  "Remuneration", "Int. communication", "Ext. communication",
                  "Skilss", "Health prot.", "Economic status",
                  "Suppl. selection", "Innovation", "Investments", "Employment", "Gen. planning", "Ar. planning", "Emp. share ownership", "Int. conflicts", "Ext. conflicts", "Stak. involv.", "Env. sustain.")

relevant_columns_part <- grep("^part_", names(data_ready), value = TRUE)
# Subset the data
data_subset_part <- data_ready[, relevant_columns_part]

# Assign the new variable names to the columns of data_wellbeing
colnames(data_subset_part) <- new_var_list

# Compute the correlation matrix
corr_matrix_part <- cor(data_subset_part, use = "pairwise.complete.obs")

# Assign numbers as column names and actual variable names as row names in your correlation matrix
colnames(corr_matrix_part) <- as.character(1:ncol(corr_matrix_part))
rownames(corr_matrix_part) <- new_var_list

# Define color palette
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# Create the correlation plot with correlation coefficients
corrplot(corr_matrix_part, method="color", col=col(200),
         type="upper",
         order="original",
         addCoef.col = "black",number.cex = 0.9,
         tl.col="black", tl.srt=0, tl.cex = 0.9,
         #tl.pos="lt", # to place text labels on the left and top
         diag=TRUE) # to show the diagonal


##### ORGANIZATIONAL HEALTH
relevant_columns_health <- grep("^health_", names(data_ready), value = TRUE)
data_subset_health <- data_ready[, relevant_columns_health]

data_health <- data_subset_health[, !names(data_subset_health) %in% "health_organization"]

new_var_list <- c("Accidents prevention", "Healthy environment", "Active listening", "Equity",
                  "Adm. efficiency", "Job sustainability", "Innovation",
                  "Conflicts management", "Stress control", "Sharing of info",
                  "Clear objectives", "Skilss enhancement", "Usefulness", "Collaborative environment", "Org. welfare")

# Assign the new variable names to the columns of data_wellbeing
colnames(data_health) <- new_var_list

# Compute the correlation matrix
corr_matrix_health <- cor(data_health, use = "pairwise.complete.obs")

# Assign numbers as column names and actual variable names as row names in your correlation matrix
colnames(corr_matrix_health) <- as.character(1:ncol(corr_matrix_health))
rownames(corr_matrix_health) <- new_var_list

# Define color palette
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# Create the correlation plot with correlation coefficients
corrplot(corr_matrix_health, method="color", col=col(200),
         type="upper",
         order="original",
         addCoef.col = "black",number.cex = 0.9,
         tl.col="black", tl.srt=0, tl.cex = 0.8,
         #tl.pos="lt", # to place text labels on the left and top
         diag=TRUE) # to show the diagonal


##### SUBJECTIVE WELLBEING
relevant_columns_subjwb <- grep("^subjective_wb_", names(data_ready), value = TRUE)
data_subjwb <- data_ready[, relevant_columns_subjwb]

new_var_list <- c(
  "Manag. esteem", "Changeability","Self-realization", "Interp. relationships",
  "Manag. credibility","Teamwork","Org. success",
  "Work-life balance","Values","Desire to go to work","Job satisfaction","Commitment to work"
)

# Assign the new variable names to the columns of data_wellbeing
colnames(data_subjwb) <- new_var_list

# Compute the correlation matrix
corr_matrix_subjwb <- cor(data_subjwb, use = "pairwise.complete.obs")

# Assign numbers as column names and actual variable names as row names in your correlation matrix
colnames(corr_matrix_subjwb) <- as.character(1:ncol(corr_matrix_subjwb))
rownames(corr_matrix_subjwb) <- new_var_list

# Define color palette
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# Create the correlation plot with correlation coefficients
corrplot(corr_matrix_subjwb, method="color", col=col(200),
         type="upper",
         order="original",
         addCoef.col = "black",number.cex = 0.9,
         tl.col="black", tl.srt=0, tl.cex = 0.8,
         #tl.pos="lt", # to place text labels on the left and top
         diag=TRUE) # to show the diagonal


##### SUBJECTIVE MALAISE
relevant_columns_subjma <- grep("^subjective_ma_", names(data_ready), value = TRUE)
data_subjma <- data_ready[, relevant_columns_subjma]

new_var_list <- c(
"Org. confusion","Absenteeism","Change job","Irrelevance","Unrecognized","Unusefulness","Emotional detachment","Lack of proactivity","Lack of interest","Resentment","Slowness in work","Aggressiveness","Gossiping"
)

# Assign the new variable names to the columns of data_wellbeing
colnames(data_subjma) <- new_var_list

# Compute the correlation matrix
corr_matrix_subjma <- cor(data_subjma, use = "pairwise.complete.obs")
print(corr_matrix_subjma)

# Assign numbers as column names and actual variable names as row names in your correlation matrix
colnames(corr_matrix_subjma) <- as.character(1:ncol(corr_matrix_subjma))
rownames(corr_matrix_subjma) <- new_var_list
# Define color palette
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# Create the correlation plot with correlation coefficients
corrplot(corr_matrix_subjma, method="color", col=col(200),
         type="upper",
         order="original",
         addCoef.col = "black",number.cex = 0.85,
         tl.col="black", tl.srt=0, tl.cex = 0.8,
         #tl.pos="lt", # to place text labels on the left and top
         diag=TRUE) # to show the diagonal


##### WORK-RELATED STRESS
relevant_columns_stress <- grep("^stress_", names(data_ready), value = TRUE)
data_stress <- data_ready[, relevant_columns_stress]

new_var_list <- c("Change","Colleagues","Control","Demand","Management","Relations","Role")

# Assign the new variable names to the columns of data_wellbeing
colnames(data_stress) <- new_var_list

# Compute the correlation matrix
corr_matrix_stress <- cor(data_stress, use = "pairwise.complete.obs")

# Assign numbers as column names and actual variable names as row names in your correlation matrix
colnames(corr_matrix_stress) <- as.character(1:ncol(corr_matrix_stress))
rownames(corr_matrix_stress) <- new_var_list

# Define color palette
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# Create the correlation plot with correlation coefficients
corrplot(corr_matrix_stress, method="color", col=col(200),
         type="upper",
         order="original",
         addCoef.col = "black",number.cex = 0.9,
         tl.col="black", tl.srt=0, tl.cex = 0.9,
         #tl.pos="lt", # to place text labels on the left and top
         diag=TRUE) # to show the diagonal

################################################
###### NON PARAMETRIC TESTS AND BOXPLOTS #######
################################################
################################################

library(ggplot2)

p1 <- ggplot(data_ready, aes(x=dimension, y=bes_wellbeing, fill=dimension)) +
  geom_boxplot(outlier.shape = 1, outlier.size = 2) +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size=10)) +  # Increase x-axis text size
  labs(title='Boxplot of Workers Well-being by Dimension', y='Workers Well-being', x='Dimension') +  
  scale_fill_brewer(palette="Set3") +
  stat_summary(fun=median, geom="text", vjust=-0.5, aes(label=sprintf("%.2f", ..y..)), size=3.5) +  # Decrease median text size
  theme(legend.position = "none")

# Display the plot
print(p1)

View(data_ready$gender)

p2 <- ggplot(data_ready, aes(x=dimension, y=bes_wellbeing, fill=gender)) +
  geom_boxplot(outlier.shape = 1, outlier.size = 2) +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size=10)) +  # Increase x-axis text size
  labs(title='Boxplot of Workers Well-being by Role', y='Workers Well-being', x='Role') +  
  scale_fill_brewer(palette="Set3") +
  #stat_summary(fun=median, geom="text", vjust=-0.5, aes(label=sprintf("%.2f", ..y..)), size=3.5) +  # Decrease median text size
  theme(legend.position = "none")

# Display the plot
print(p2)


############ TEST
# Load necessary libraries
library(tidyverse)
library(car)  # for Levene's Test
library(dunn.test) 


# Filter the data to include only rows where gender is 'M' or 'F'
data <- data_ready %>% filter(gender %in% c('M', 'F'))

# For each categorical variable, perform the following steps
categorical_vars <- c('dimension', 'role', 'gender', 'age', 'seniority')

for(cat_var in categorical_vars){
  print(paste('Analyzing', cat_var))
  
  # Ensure that the categorical variable is a factor
  data[[cat_var]] <- as.factor(data[[cat_var]])
  
  # Group data by the categorical variable
  grouped_data <- data %>% group_by(data[[cat_var]])
  
  # Shapiro-Wilk Test for normality within each group
  shapiro_results <- grouped_data %>% summarise(p_value = shapiro.test(bes_wellbeing)$p.value)
  print('Shapiro-Wilk Test Results:')
  print(shapiro_results)
  
  # Levene's Test for homogeneity of variance
  levene_results <- leveneTest(bes_wellbeing ~ data[[cat_var]], data = data)
  print('Levene’s Test Results:')
  print(levene_results)
  
  # Depending on the results of the above tests, perform either ANOVA, Kruskal-Wallis Test, or Mann-Whitney U Test
  if(all(shapiro_results$p_value > 0.05) & levene_results[1, 'Pr(>F)'] > 0.05){
    anova_results <- aov(bes_wellbeing ~ data[[cat_var]], data = data) %>% summary()
    print('ANOVA Results:')
    print(anova_results)
  } else {
    if(cat_var == 'gender' && length(unique(data$gender)) == 2) {
      mann_whitney_results <- wilcox.test(bes_wellbeing ~ gender, data = data)
      print('Mann-Whitney U Test Results:')
      print(mann_whitney_results)
    } else {
      kruskal_results <- kruskal.test(bes_wellbeing ~ data[[cat_var]], data = data)
      print('Kruskal-Wallis Test Results:')
      print(kruskal_results)
      
      if(kruskal_results$p.value < 0.05) {
        # If Kruskal-Wallis is significant, perform Dunn's Test for pairwise comparisons
        dunn_results <- dunn.test(x = data$bes_wellbeing, g = data[[cat_var]], method = "bonferroni")
        print('Dunn’s Test Results:')
        print(dunn_results)
      }
    }
  }
}

########### PARTICIPATION TEST

for(cat_var in categorical_vars){
  print(paste('Analyzing', cat_var))
  
  # Ensure that the categorical variable is a factor
  data[[cat_var]] <- as.factor(data[[cat_var]])
  
  # Group data by the categorical variable
  grouped_data <- data %>% group_by(data[[cat_var]])
  
  # Shapiro-Wilk Test for normality within each group
  shapiro_results <- grouped_data %>% summarise(p_value = shapiro.test(participation_level)$p.value)
  print('Shapiro-Wilk Test Results:')
  print(shapiro_results)
  
  # Levene's Test for homogeneity of variance
  levene_results <- leveneTest(participation_level ~ data[[cat_var]], data = data)
  print('Levene’s Test Results:')
  print(levene_results)
  
  # Depending on the results of the above tests, perform either ANOVA, Kruskal-Wallis Test, or Mann-Whitney U Test
  if(all(shapiro_results$p_value > 0.05) & levene_results[1, 'Pr(>F)'] > 0.05){
    anova_results <- aov(participation_level ~ data[[cat_var]], data = data) %>% summary()
    print('ANOVA Results:')
    print(anova_results)
  } else {
    if(cat_var == 'gender' && length(unique(data$gender)) == 2) {
      mann_whitney_results <- wilcox.test(participation_level ~ gender, data = data)
      print('Mann-Whitney U Test Results:')
      print(mann_whitney_results)
    } else {
      kruskal_results <- kruskal.test(participation_level ~ data[[cat_var]], data = data)
      print('Kruskal-Wallis Test Results:')
      print(kruskal_results)
      
      if(kruskal_results$p.value < 0.05) {
        # If Kruskal-Wallis is significant, perform Dunn's Test for pairwise comparisons
        dunn_results <- dunn.test(x = data$participation_level, g = data[[cat_var]], method = "bonferroni")
        print('Dunn’s Test Results:')
        print(dunn_results)
      }
    }
  }
}

########### ORGANIZATIONAL HEALTH TEST

for(cat_var in categorical_vars){
  print(paste('Analyzing', cat_var))
  
  # Ensure that the categorical variable is a factor
  data[[cat_var]] <- as.factor(data[[cat_var]])
  
  # Group data by the categorical variable
  grouped_data <- data %>% group_by(data[[cat_var]])
  
  # Shapiro-Wilk Test for normality within each group
  shapiro_results <- grouped_data %>% summarise(p_value = shapiro.test(health_organization)$p.value)
  print('Shapiro-Wilk Test Results:')
  print(shapiro_results)
  
  # Levene's Test for homogeneity of variance
  levene_results <- leveneTest(health_organization ~ data[[cat_var]], data = data)
  print('Levene’s Test Results:')
  print(levene_results)
  
  # Depending on the results of the above tests, perform either ANOVA, Kruskal-Wallis Test, or Mann-Whitney U Test
  if(all(shapiro_results$p_value > 0.05) & levene_results[1, 'Pr(>F)'] > 0.05){
    anova_results <- aov(health_organization ~ data[[cat_var]], data = data) %>% summary()
    print('ANOVA Results:')
    print(anova_results)
  } else {
    if(cat_var == 'gender' && length(unique(data$gender)) == 2) {
      mann_whitney_results <- wilcox.test(health_organization ~ gender, data = data)
      print('Mann-Whitney U Test Results:')
      print(mann_whitney_results)
    } else {
      kruskal_results <- kruskal.test(health_organization ~ data[[cat_var]], data = data)
      print('Kruskal-Wallis Test Results:')
      print(kruskal_results)
      
      if(kruskal_results$p.value < 0.05) {
        # If Kruskal-Wallis is significant, perform Dunn's Test for pairwise comparisons
        dunn_results <- dunn.test(x = data$health_organization, g = data[[cat_var]], method = "bonferroni")
        print('Dunn’s Test Results:')
        print(dunn_results)
      }
    }
  }
}

###########UBJECTIVE WELLBEING TEST

for(cat_var in categorical_vars){
  print(paste('Analyzing', cat_var))
  
  # Ensure that the categorical variable is a factor
  data[[cat_var]] <- as.factor(data[[cat_var]])
  
  # Group data by the categorical variable
  grouped_data <- data %>% group_by(data[[cat_var]])
  
  # Shapiro-Wilk Test for normality within each group
  shapiro_results <- grouped_data %>% summarise(p_value = shapiro.test(subjective_wellbeing)$p.value)
  print('Shapiro-Wilk Test Results:')
  print(shapiro_results)
  
  # Levene's Test for homogeneity of variance
  levene_results <- leveneTest(subjective_wellbeing ~ data[[cat_var]], data = data)
  print('Levene’s Test Results:')
  print(levene_results)
  
  # Depending on the results of the above tests, perform either ANOVA, Kruskal-Wallis Test, or Mann-Whitney U Test
  if(all(shapiro_results$p_value > 0.05) & levene_results[1, 'Pr(>F)'] > 0.05){
    anova_results <- aov(subjective_wellbeing ~ data[[cat_var]], data = data) %>% summary()
    print('ANOVA Results:')
    print(anova_results)
  } else {
    if(cat_var == 'gender' && length(unique(data$gender)) == 2) {
      mann_whitney_results <- wilcox.test(subjective_wellbeing ~ gender, data = data)
      print('Mann-Whitney U Test Results:')
      print(mann_whitney_results)
    } else {
      kruskal_results <- kruskal.test(subjective_wellbeing ~ data[[cat_var]], data = data)
      print('Kruskal-Wallis Test Results:')
      print(kruskal_results)
      
      if(kruskal_results$p.value < 0.05) {
        # If Kruskal-Wallis is significant, perform Dunn's Test for pairwise comparisons
        dunn_results <- dunn.test(x = data$subjective_wellbeing, g = data[[cat_var]], method = "bonferroni")
        print('Dunn’s Test Results:')
        print(dunn_results)
      }
    }
  }
}

###########SUBJECTIVE MALAISE TEST

for(cat_var in categorical_vars){
  print(paste('Analyzing', cat_var))
  
  # Ensure that the categorical variable is a factor
  data[[cat_var]] <- as.factor(data[[cat_var]])
  
  # Group data by the categorical variable
  grouped_data <- data %>% group_by(data[[cat_var]])
  
  # Shapiro-Wilk Test for normality within each group
  shapiro_results <- grouped_data %>% summarise(p_value = shapiro.test(subjective_malaise)$p.value)
  print('Shapiro-Wilk Test Results:')
  print(shapiro_results)
  
  # Levene's Test for homogeneity of variance
  levene_results <- leveneTest(subjective_malaise ~ data[[cat_var]], data = data)
  print('Levene’s Test Results:')
  print(levene_results)
  
  # Depending on the results of the above tests, perform either ANOVA, Kruskal-Wallis Test, or Mann-Whitney U Test
  if(all(shapiro_results$p_value > 0.05) & levene_results[1, 'Pr(>F)'] > 0.05){
    anova_results <- aov(subjective_malaise ~ data[[cat_var]], data = data) %>% summary()
    print('ANOVA Results:')
    print(anova_results)
  } else {
    if(cat_var == 'gender' && length(unique(data$gender)) == 2) {
      mann_whitney_results <- wilcox.test(subjective_malaise ~ gender, data = data)
      print('Mann-Whitney U Test Results:')
      print(mann_whitney_results)
    } else {
      kruskal_results <- kruskal.test(subjective_malaise ~ data[[cat_var]], data = data)
      print('Kruskal-Wallis Test Results:')
      print(kruskal_results)
      
      if(kruskal_results$p.value < 0.05) {
        # If Kruskal-Wallis is significant, perform Dunn's Test for pairwise comparisons
        dunn_results <- dunn.test(x = data$subjective_malaise, g = data[[cat_var]], method = "bonferroni")
        print('Dunn’s Test Results:')
        print(dunn_results)
      }
    }
  }
}

###########WORK RELATED STRESS TEST

for(cat_var in categorical_vars){
  print(paste('Analyzing', cat_var))
  
  # Ensure that the categorical variable is a factor
  data[[cat_var]] <- as.factor(data[[cat_var]])
  
  # Group data by the categorical variable
  grouped_data <- data %>% group_by(data[[cat_var]])
  
  # Shapiro-Wilk Test for normality within each group
  shapiro_results <- grouped_data %>% summarise(p_value = shapiro.test(stress)$p.value)
  print('Shapiro-Wilk Test Results:')
  print(shapiro_results)
  
  # Levene's Test for homogeneity of variance
  levene_results <- leveneTest(stress ~ data[[cat_var]], data = data)
  print('Levene’s Test Results:')
  print(levene_results)
  
  # Depending on the results of the above tests, perform either ANOVA, Kruskal-Wallis Test, or Mann-Whitney U Test
  if(all(shapiro_results$p_value > 0.05) & levene_results[1, 'Pr(>F)'] > 0.05){
    anova_results <- aov(stress ~ data[[cat_var]], data = data) %>% summary()
    print('ANOVA Results:')
    print(anova_results)
  } else {
    if(cat_var == 'gender' && length(unique(data$gender)) == 2) {
      mann_whitney_results <- wilcox.test(stress ~ gender, data = data)
      print('Mann-Whitney U Test Results:')
      print(mann_whitney_results)
    } else {
      kruskal_results <- kruskal.test(stress ~ data[[cat_var]], data = data)
      print('Kruskal-Wallis Test Results:')
      print(kruskal_results)
      
      if(kruskal_results$p.value < 0.05) {
        # If Kruskal-Wallis is significant, perform Dunn's Test for pairwise comparisons
        dunn_results <- dunn.test(x = data$stress, g = data[[cat_var]], method = "bonferroni")
        print('Dunn’s Test Results:')
        print(dunn_results)
      }
    }
  }
}


##### BOX PLOTS

# Define a color palette with the number of colors you need
color_palette <- brewer.pal(5, "Set1")  # Here, 8 is just an example; you can choose the number you need based on your categorical variable's levels


# Define a list of continuous variables and their respective user-friendly names
continuous_vars <- c("participation_level", "bes_wellbeing", "health_organization", 
                     "subjective_wellbeing", "subjective_malaise", "stress")

continuous_vars <- c("bes_wellbeing")
names_labels <- c("Workers' well-being")

names_labels <- c("Participation level", "Workers' well-being", "Organizational health", 
                  "Subjective well-being", "Subjective malaise", "Absence of stress")

names(continuous_vars) <- names_labels

# Define a list with the specific order for each categorical variable
ordered_levels <- list(
  role = c("Director", "Manager", "Employee", "Labourer"),
  seniority = c("<1 year", "1-3 years", "4-10 years", ">10 years")
)


# Define a list of categorical variables
categorical_vars <- c("dimension", "role", "gender", "age", "seniority")

create_plot <- function(continuous_var, categorical_var, data) {
  
  # Check if the categorical_var has a specific order
  if(categorical_var %in% names(ordered_levels)) {
    data[[categorical_var]] <- factor(data[[categorical_var]], levels = ordered_levels[[categorical_var]], ordered = TRUE)
  }
  
  # Filter data
  data_filtered <- data %>% select(all_of(c(continuous_var, categorical_var))) %>%
    filter(!(categorical_var == "gender" & !data[[categorical_var]] %in% c("M", "F")))
  
  # Convert to long format
  data_long <- data_filtered %>% 
    gather(key = "Variable", value = "Value", -all_of(categorical_var))
  
  # Identify outliers and calculate medians
  data_long <- data_long %>%
    group_by(Variable, !!sym(categorical_var)) %>%
    mutate(
      median = round(median(Value), 2),
      mean = round(mean(Value), 2),
      Q1 = round(quantile(Value, 0.25), 2),
      Q3 = round(quantile(Value, 0.75), 2),
      IQR = round(Q3 - Q1, 2),
      Lower_Bound = Q1 - 1.5 * IQR,
      Upper_Bound = Q3 + 1.5 * IQR,
      Outlier = Value < Lower_Bound | Value > Upper_Bound
    )
  
  # Create median annotations
  median_annotations <- data_long %>%
    summarise(
      x = unique(get(categorical_var)),
      y = median,
      text = as.character(median)
    )
  
  # Create the plot
  user_friendly_name <- names(continuous_vars)[continuous_vars == continuous_var]
  p <- plot_ly(data = data_long, y = ~Value, x = ~get(categorical_var), type = "box",
               color = ~factor(get(categorical_var), levels = unique(get(categorical_var))),
               colors = color_palette,
               text = ~paste0("Variable: ", user_friendly_name,
                              "<br>", toupper(categorical_var), ": ", get(categorical_var),
                              "<br>Mean: ", mean,
                              "<br>Median: ", median,
                              "<br>Q1: ", Q1,
                              "<br>Q3: ", Q3),
               hoverinfo = "text") %>%
    layout(title = paste("Boxplots for", user_friendly_name, "by", categorical_var),
           yaxis = list(title = user_friendly_name),
           xaxis = list(title = toupper(categorical_var)),
           showlegend = FALSE,
           annotations = map2(median_annotations$x, median_annotations$y, ~list(x = .x, y = .y, text = .y, showarrow = FALSE, yshift = 10)))
  
  return(p)
}


# Loop over combinations of continuous and categorical variables and create plots
for (cont_var in continuous_vars) {
  for (cat_var in categorical_vars) {
    plot <- create_plot(cont_var, cat_var, data_ready)
    print(plot)
  }
}

