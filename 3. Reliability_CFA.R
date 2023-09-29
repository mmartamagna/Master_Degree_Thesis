#############################################################################
##  SCRIPT 3: Cronbach's Alpha, Confirmative Factor Analysis               ##
##  Author: Marta Magnani                                                  ##                                        
##  Last: Sep 15th, 2023                                                   ##
#############################################################################


#####Evaluating the reliability of questions
#Cronbach’s Alpha

#### CONFIRMATIVE FACTOR ANALYSIS
#### https://stats.oarc.ucla.edu/r/seminars/rcfa/ (Introduction to SAS. UCLA: Statistical Consulting Group. 
#from https://stats.oarc.ucla.edu/sas/modules/introduction-to-the-features-of-sas/ (accessed August 22, 2021))
##https://bookdown.org/bean_jerry/using_r_for_social_work_research/confirmatory-factor-analysis.html
##
##Load Libraries
library(lavaan)
library(dplyr) 
library(tidyr)
library(knitr)
library(MVN)
library(readxl)
library(semPlot)
library(psych)
library(ggplot2)


data_factor <- read.csv("C:/Users/magna/OneDrive/Tesi Magistrale DSE/Analisi Script R/data_ready.csv")
summary(data_factor)

####### CRONBACH'S ALPHA ENTIRE QUESTIONNAIRE
library(ltm)

relevant_columns_bes <- grep("^Q[1-9][0-9]*$", names(data_ready), value = TRUE)
data_subset_bes <- data_ready[, relevant_columns_bes]
cronbach.alpha(data_subset_bes)

relevant_columns_part <- grep("^part_", names(data_ready), value = TRUE)
data_subset_part <- data_ready[, relevant_columns_part]
cronbach.alpha(data_subset_part)



######## CRONBACH'S ALPHA FOR 12 WELL-BEING DOMAINS##################
library(psych)

# Define the domains and corresponding questions
cronbach_list_bes <- list(
  politics = c("Q1", "Q17", "Q19", "Q21", "Q29", "Q35", "Q72", "Q78", "Q79", "Q80", "Q81", "Q83", "Q84", "Q115", "Q117"),
  security = c("Q2", "Q3", "Q7", "Q62"),
  health = c("Q4", "Q6", "Q8", "Q10", "Q67", "Q68", "Q69", "Q70", "Q76"),
  socialrelationships = c("Q5", "Q16", "Q22", "Q36", "Q37", "Q40", "Q42", "Q44", "Q54", "Q56", "Q60", "Q61", "Q85", "Q86", "Q93", "Q94", "Q95", "Q98", "Q99", "Q101", "Q102", "Q114", "Q118", "Q119"),
  worklifebalance = c("Q9", "Q11", "Q12", "Q23", "Q25", "Q27", "Q28", "Q30", "Q31", "Q32", "Q33", "Q34", "Q38", "Q39", "Q41", "Q43", "Q45", "Q46", "Q49", "Q52", "Q59", "Q63", "Q64", "Q65", "Q71", "Q75", "Q82","Q87", "Q88","Q92", "Q103", "Q104", "Q105", "Q107", "Q109", "Q110", "Q112", "Q116"),
  naturalculturalheritage = c("Q13", "Q14"),
  qualityofservices = c("Q15", "Q20", "Q73", "Q120", "Q122"),
  economicwellbeing = c("Q24", "Q26", "Q89", "Q123", "Q124", "Q125", "Q96", "Q97"),
  innovationresearchcreativity = c("Q47", "Q50", "Q51"),
  education = c("Q48", "Q100", "Q121"),
  environment = c("Q108", "Q113"),
  subjectivewellbeing = c("Q18", "Q53", "Q55", "Q57", "Q58", "Q66", "Q74", "Q77", "Q90", "Q91", "Q106", "Q111", "Q126", "Q127"))


# Initialize a list to store the results
cronbach_alpha_results <- list()

# Loop over each domain and calculate Cronbach's Alpha
for (domain in names(cronbach_list_bes)) {
  questions <- cronbach_list_bes[[domain]]
  
  # Get the column indices of the specified questions
  relevant_columns <- match(questions, names(data_ready))
  relevant_columns <- relevant_columns[!is.na(relevant_columns)]  # Remove NA values
  
  # If there are at least two questions, calculate Cronbach's Alpha
  if (length(relevant_columns) >= 2) {
    domain_data <- data_ready[, relevant_columns, drop = FALSE]
    cronbach_alpha_results[[domain]] <- psych::alpha(domain_data)$total[1]  # $total[1] extracts the alpha value
  } else {
    cronbach_alpha_results[[domain]] <- NA
  }
}

# Print the Cronbach's Alpha for each domain
cronbach_alpha_results


######### CRONBACH ALPHA FOR HEALTH DOMAINS #########
# Define the domains and corresponding questions
cronbach_list_health <-list(
  prevention =c("Q1", "Q2", "Q3", "Q4", "Q5"),
  workenvironment =c("Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15"),
  activelistening =c("Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23"),
  equity =c("Q24", "Q25", "Q26", "Q27", "Q28", "Q29"),
  administrativeefficiency =c("Q30", "Q31", "Q32", "Q33", "Q34", "Q35", "Q36", "Q37"),
  jobsustainability =c("Q38", "Q39", "Q40", "Q41", "Q42", "Q43", "Q44", "Q45", "Q46"),
  openinnovation =c("Q47", "Q48", "Q49", "Q50", "Q51", "Q52", "Q43"),
  conflicts =c("Q54", "Q55", "Q56", "Q57", "Q58", "Q59", "Q60", "Q61", "Q62", "Q63", "Q64"),
  stresslevels =c("Q65", "Q66", "Q67", "Q68", "Q69", "Q70", "Q72", "Q73", "Q74", "Q75", "Q76", "Q77"),
  information =c("Q78", "Q79", "Q80", "Q81"),
  clearobjectives =c("Q82", "Q83", "Q84", "Q85", "Q86", "Q87", "Q88", "Q89", "Q90"),
  skillsenhancement =c("Q90", "Q91", "Q92", "Q93", "Q94", "Q95", "Q96", "Q97", "Q98", "Q99", "Q100", "Q101", "Q102"),
  meaningfulness =c("Q102", "Q103", "Q104", "Q105", "Q106", "Q107", "Q108", "Q109", "Q110", "Q111", "Q112", "Q113"),
  relationalenvironment =c("Q114", "Q115", "Q116", "Q117", "Q118", "Q119", "Q126", "Q127"),
  organizationalwelfare=c("Q120", "Q121", "Q122", "Q123", "Q124", "Q125"))

# Initialize a list to store the results
cronbach_alpha_results <- list()

# Loop over each domain and calculate Cronbach's Alpha
for (domain in names(cronbach_list_health)) {
  questions <- cronbach_list_health[[domain]]
  
  # Get the column indices of the specified questions
  relevant_columns <- match(questions, names(data_ready))
  relevant_columns <- relevant_columns[!is.na(relevant_columns)]  # Remove NA values
  
  # If there are at least two questions, calculate Cronbach's Alpha
  if (length(relevant_columns) >= 2) {
    domain_data <- data_ready[, relevant_columns, drop = FALSE]
    cronbach_alpha_results[[domain]] <- psych::alpha(domain_data)$total[1]  # $total[1] extracts the alpha value
  } else {
    cronbach_alpha_results[[domain]] <- NA
  }
}

# Print the Cronbach's Alpha for each domain
cronbach_alpha_results

######### CRONBACH ALPHA FOR SUBJECTIVE WELL-BEING DOMAINS #########

# Define the domains and corresponding questions
cronbach_list_subjwb <-list(managementesteem =c("Q1", "Q5", "Q20", "Q21", "Q29", "Q59", "Q72"),
changeability =c("Q4", "Q32"),
selfrealization =c("Q18", "Q48", "Q105", "Q108"),
interpersonal =c("Q22", "Q60", "Q61", "Q62", "Q64", "Q94", "Q99", "Q101", "Q118"),
managementcredibility =c("Q30", "Q41", "Q42", "Q44", "Q81"),
team=c("Q35", "Q36", "Q40", "Q54", "Q57"),
organizationsuccess =c("Q37", "Q53"),
worklifebalance =c("Q38", "Q39", "Q52", "Q67", "Q69", "Q70", "Q71", "Q75", "Q77"),
values =c("Q78", "Q79", "Q80", "Q116"),
gotowork =c("Q110"),
satisfaction =c("Q103", "Q107"),
commitment =c("Q112"))

# Initialize a list to store the results
cronbach_alpha_results <- list()

# Loop over each domain and calculate Cronbach's Alpha
for (domain in names(cronbach_list_subjwb)) {
  questions <- cronbach_list_subjwb[[domain]]
  
  # Get the column indices of the specified questions
  relevant_columns <- match(questions, names(data_ready))
  relevant_columns <- relevant_columns[!is.na(relevant_columns)]  # Remove NA values
  
  # If there are at least two questions, calculate Cronbach's Alpha
  if (length(relevant_columns) >= 2) {
    domain_data <- data_ready[, relevant_columns, drop = FALSE]
    cronbach_alpha_results[[domain]] <- psych::alpha(domain_data)$total[1]  # $total[1] extracts the alpha value
  } else {
    cronbach_alpha_results[[domain]] <- NA
  }
}

# Print the Cronbach's Alpha for each domain
cronbach_alpha_results

######### CRONBACH ALPHA FOR SUBJECTIVE MALAISE DOMAINS #########

# Define the domains and corresponding questions
cronbach_list_subjma <-list(organizationalconfusion =c("Q27", "Q45", "Q46", "Q87"),
absenteeism =c("Q65"),
changejob =c("Q104"),
irrelevance =c("Q85", "Q86", "Q109"),
unrecognized =c("Q24", "Q26", "Q90", "Q91", "Q96", "Q97"),
unusefulness=c("Q92", "Q95"),
rulesadherence =c("Q88"),
proactivity =c("Q17", "Q102"),
jobinterest =c("Q23", "Q111"),
resentment =c("Q28", "Q63", "Q89"),
slowness =c("Q31", "Q34"),
aggressive =c("Q55"),
gossip =c("Q114", "Q119"))

# Initialize a list to store the results
cronbach_alpha_results <- list()

# Loop over each domain and calculate Cronbach's Alpha
for (domain in names(cronbach_list_subjma)) {
  questions <- cronbach_list_subjma[[domain]]
  
  # Get the column indices of the specified questions
  relevant_columns <- match(questions, names(data_ready))
  relevant_columns <- relevant_columns[!is.na(relevant_columns)]  # Remove NA values
  
  # If there are at least two questions, calculate Cronbach's Alpha
  if (length(relevant_columns) >= 2) {
    domain_data <- data_ready[, relevant_columns, drop = FALSE]
    cronbach_alpha_results[[domain]] <- psych::alpha(domain_data)$total[1]  # $total[1] extracts the alpha value
  } else {
    cronbach_alpha_results[[domain]] <- NA
  }
}

# Print the Cronbach's Alpha for each domain
cronbach_alpha_results

######### CRONBACH ALPHA FOR STRESS DOMAINS #########

# Define the domains and corresponding questions
cronbach_list_stress<-list(change =c("Q47", "Q78", "Q79", "Q80"),
colleagues =c("Q34", "Q36", "Q40", "Q54", "Q57", "Q58", "Q64", "Q90", "Q98", "Q99"),
control =c("Q31", "Q52", "Q75", "Q76", "Q100", "Q116"),
demand =c("Q1", "Q38", "Q39", "Q42", "Q43", "Q44", "Q45", "Q46", "Q67", "Q68", "Q71", "Q92", "Q95", "Q115"),
management =c("Q2", "Q5", "Q8", "Q9", "Q11", "Q12", "Q15", "Q18", "Q20", "Q30", "Q32", "Q37", "Q49", "Q59", "Q63", "Q72", "Q89", "Q91"),
relations=c("Q4", "Q17", "Q22", "Q60", "Q61", "Q62", "Q85", "Q86", "Q94", "Q101", "Q102", "Q114", "Q118", "Q119"),
role=c("Q24", "Q25", "Q26", "Q27", "Q82", "Q84", "Q87", "Q88", "Q96", "Q97", "Q105"))


# Initialize a list to store the results
cronbach_alpha_results <- list()

# Loop over each domain and calculate Cronbach's Alpha
for (domain in names(cronbach_list_stress)) {
  questions <- cronbach_list_stress[[domain]]
  
  # Get the column indices of the specified questions
  relevant_columns <- match(questions, names(data_ready))
  relevant_columns <- relevant_columns[!is.na(relevant_columns)]  # Remove NA values
  
  # If there are at least two questions, calculate Cronbach's Alpha
  if (length(relevant_columns) >= 2) {
    domain_data <- data_ready[, relevant_columns, drop = FALSE]
    cronbach_alpha_results[[domain]] <- psych::alpha(domain_data)$total[1]  # $total[1] extracts the alpha value
  } else {
    cronbach_alpha_results[[domain]] <- NA
  }
}

# Print the Cronbach's Alpha for each domain
cronbach_alpha_results

#######################################################################################
#### CFA #############################################################################


library(lavaan)
library(dplyr) 
library(tidyr)
library(knitr)
library(MVN)
library(readxl)
library(semPlot)
library(psych)
library(ggplot2)
library(DiagrammeR)


# Creare un diagramma con un fattore latente e quattro variabili osservate
graph <- grViz("
digraph diagram {
  
  # Impostazioni grafiche
  graph[layout = dot, rankdir = LR]
  node[width = 1]
  
  # Nodo per il fattore latente
  Factor[label = Factor, shape = ellipse, width = 1.5]
  
  
  # Archi dai fattori latenti alle variabili osservate con coefficienti di percorso
  Factor -> q1[label = 'loading']
  Factor -> q2[label = 'loading']
  Factor -> q3[label = 'loading']
  Factor -> q4[label = 'loading']
  
  # Freccie circolari intorno ai nodi con varianze
  Factor -> Factor[dir = both, tailport = w, headport = e, label = 'variance', color = gray, minlen = 0.5]
  q1 -> q1[dir = both, tailport = w, headport = e, label = 'variance', color = gray, minlen = 0.5]
  q2 -> q2[dir = both, tailport = w, headport = e, label = 'variance', color = gray, minlen = 0.5]
  q3 -> q3[dir = both, tailport = w, headport = e, label = 'variance', color = gray, minlen = 0.5]
  q4 -> q4[dir = both, tailport = w, headport = e, label = 'variance', color = gray, minlen = 0.5]
}
")

# Visualizzare il diagramma
graph


# Do 10 different models of CFA.
# Define the model for each domain separately
models <- list(
  politics  = 'politics  =~ Q1 + Q17 + Q19 + Q21 + Q29 + Q35 + Q72 + Q78 + Q79 + Q80 + Q81 + Q83 + Q84 + Q115 + Q117',
  security  = 'security  =~ Q2 + Q3 + Q7 + Q62',
  health    = 'health  =~ Q4 + Q6 + Q8 + Q10 + Q67 + Q68 + Q69 + Q70 + Q76',
  relationships  = 'relationships  =~ Q5 + Q16 + Q22 + Q36 + Q37 + Q40 + Q42 + Q44 + Q54 + Q56 + Q60 + Q61 + Q85 + Q86 + Q93 + Q94 + Q95 + Q98 + Q99 + Q101 + Q102 + Q114 + Q118 + Q119',
  balance  = 'balance  =~ Q9 + Q11 + Q12 + Q23 + Q25 + Q27 + Q28 + Q30 + Q31 + Q32 + Q33 + Q34 + Q38 + Q39 + Q41 + Q43 + Q45 + Q46 + Q49 + Q52 + Q59 + Q63 + Q64 + Q65 + Q71 + Q75 + Q82 + Q87 + Q88 + Q92 + Q103 + Q104 + Q105 + Q107 + Q109 + Q110 + Q112 + Q116',
  services  = 'services  =~ Q15 + Q20 + Q73 + Q120 + Q122',
  econwb  = 'econwb  =~ Q24 + Q26 + Q89 + Q123 + Q124 + Q125 + Q96 + Q97',
  innovation  = 'innovation  =~ Q47 + Q50 + Q51',
  education  = 'education  =~ Q48 + Q100 + Q121',
  subjwb = 'subjwb =~ Q18 + Q53 + Q55 + Q57 + Q58 + Q66 + Q74 + Q77 + Q90 + Q91 + Q106 + Q111 + Q126 + Q127'
)

# Initialize lists to store results
fit_objects <- list()
fit_measures <- list()

# Loop over each domain and fit the CFA model
for (domain in names(models)) {
  model <- models[[domain]]
  fit <- cfa(model, data=data_factor, ordered=TRUE)
  
  # Store the fitted object and summary measures
  fit_objects[[domain]] <- fit
  fit_measures[[domain]] <- summary(fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
}

# Access the results for each domain separately by referencing the appropriate list and domain name
print(fit_objects)
print(fit_measures)


# Create the path diagram with customizations
semPaths(
  fit,
  "est",
  "std",
  intercepts = FALSE,
  layout = "tree",
  rotation = 2,
  edge.width = 1.0,
  edge.label.cex=1.3, 
  curvePivot=TRUE
)



###POLITICS
library(DiagrammeR)

graph <- grViz("
digraph diagram {
  graph[layout = dot, rankdir = LR]
  node[width = 1]
  
  Factor[label = 'politics\\n0.596', shape = ellipse, width = 1.5, color = lightblue]
  
  Factor -> Q1[label = '1.000']
  Factor -> Q17[label = '0.999']
  Factor -> Q19[label = '0.463']
  Factor -> Q21[label = '1.131']
  Factor -> Q29[label = '0.445']
  Factor -> Q35[label = '0.942']
  Factor -> Q72[label = '1.055']
  Factor -> Q78[label = '0.865']
  Factor -> Q79[label = '0.987']
  Factor -> Q80[label = '1.224']
  Factor -> Q81[label = '1.181']
  Factor -> Q83[label = '1.049']
  Factor -> Q84[label = '1.163']
  Factor -> Q115[label = '1.102']
  Factor -> Q117[label = '0.326']
  
  # Observations
  Q1[label = 'Q1', shape = box, color = lightcoral]
  Q17[label = 'Q17', shape = box, color = lightcoral]
  Q19[label = 'Q19', shape = box, color = lightcoral]
  Q21[label = 'Q21', shape = box, color = lightcoral]
  Q29[label = 'Q29', shape = box, color = lightcoral]
  Q35[label = 'Q35', shape = box, color = lightcoral]
  Q72[label = 'Q72', shape = box, color = lightcoral]
  Q78[label = 'Q78', shape = box, color = lightcoral]
  Q79[label = 'Q79', shape = box, color = lightcoral]
  Q80[label = 'Q80', shape = box, color = lightcoral]
  Q81[label = 'Q81', shape = box, color = lightcoral]
  Q83[label = 'Q83', shape = box, color = lightcoral]
  Q84[label = 'Q84', shape = box, color = lightcoral]
  Q115[label = 'Q115', shape = box, color = lightcoral]
  Q117[label = 'Q117', shape = box, color = lightcoral]
  
  # Variances
  Factor -> Factor[dir = both, tailport = w, headport = e, label = '0.596', color = gray, minlen = 0.5]
  Q1 -> Q1[dir = both, tailport = w, headport = e, label = '0.404', color = gray, minlen = 0.5]
  Q17 -> Q17[dir = both, tailport = w, headport = e, label = '0.404', color = gray, minlen = 0.5]
  Q19 -> Q19[dir = both, tailport = w, headport = e, label = '0.872', color = gray, minlen = 0.5]
  Q21 -> Q21[dir = both, tailport = w, headport = e, label = '0.238', color = gray, minlen = 0.5]
  Q29 -> Q29[dir = both, tailport = w, headport = e, label = '0.882', color = gray, minlen = 0.5]
  Q35 -> Q35[dir = both, tailport = w, headport = e, label = '0.471', color = gray, minlen = 0.5]
  Q72 -> Q72[dir = both, tailport = w, headport = e, label = '0.336', color = gray, minlen = 0.5]
  Q78 -> Q78[dir = both, tailport = w, headport = e, label = '0.554', color = gray, minlen = 0.5]
  Q79 -> Q79[dir = both, tailport = w, headport = e, label = '0.418', color = gray, minlen = 0.5]
  Q80 -> Q80[dir = both, tailport = w, headport = e, label = '0.106', color = gray, minlen = 0.5]
  Q81 -> Q81[dir = both, tailport = w, headport = e, label = '0.168', color = gray, minlen = 0.5]
  Q83 -> Q83[dir = both, tailport = w, headport = e, label = '0.344', color = gray, minlen = 0.5]
  Q84 -> Q84[dir = both, tailport = w, headport = e, label = '0.194', color = gray, minlen = 0.5]
  Q115 -> Q115[dir = both, tailport = w, headport = e, label = '0.276', color = gray, minlen = 0.5]
  Q117 -> Q117[dir = both, tailport = w, headport = e, label = '0.936', color = gray, minlen = 0.5]
}
")

print(graph)

## SECURITY

graph <- grViz("
digraph diagram {
  graph[layout = dot, rankdir = LR]
  node[width = 1]
  
  Factor[label = 'security\\n0.784', shape = ellipse, width = 1.5, color = lightblue]
  
  Factor -> Q2[label = '1.000']
  Factor -> Q3[label = '0.890']
  Factor -> Q7[label = '1.008']
  Factor -> Q62[label = '0.306']
  
  # Observations
  Q2[label = 'Q2', shape = box, color = lightcoral]
  Q3[label = 'Q3', shape = box, color = lightcoral]
  Q7[label = 'Q7', shape = box, color = lightcoral]
  Q62[label = 'Q62', shape = box, color = lightcoral]
  
  # Variances
  Factor -> Factor[dir = both, tailport = w, headport = w, label = '0.784', color = gray, minlen = 0.5]
  Q2 -> Q2[dir = both, tailport = w, headport = w, label = '0.216', color = gray, minlen = 0.5]
  Q3 -> Q3[dir = both, tailport = e, headport = w, label = '0.378', color = gray, minlen = 0.5]
  Q7 -> Q7[dir = both, tailport = e, headport = w, label = '0.204', color = gray, minlen = 0.5]
  Q62 -> Q62[dir = both, tailport = w, headport = w, label = '0.927', color = gray, minlen = 0.5]
}
")

print(graph)

##HEALTH

library(DiagrammeR)

graph <- grViz("
digraph diagram {
  graph[layout = dot, rankdir = LR]
  node[width = 1]
  
  Factor[label = 'health\\n0.063', shape = ellipse, width = 1.5, color = lightblue]
  
  Factor -> Q4[label = '1.000']
  Factor -> Q6[label = '2.538']
  Factor -> Q8[label = '2.246']
  Factor -> Q10[label = '2.372']
  Factor -> Q67[label = '3.118']
  Factor -> Q68[label = '2.670']
  Factor -> Q69[label = '3.490']
  Factor -> Q70[label = '3.197']
  Factor -> Q76[label = '2.002']
  
  # Observations
  Q4[label = 'Q4', shape = box, color = lightcoral]
  Q6[label = 'Q6', shape = box, color = lightcoral]
  Q8[label = 'Q8', shape = box, color = lightcoral]
  Q10[label = 'Q10', shape = box, color = lightcoral]
  Q67[label = 'Q67', shape = box, color = lightcoral]
  Q68[label = 'Q68', shape = box, color = lightcoral]
  Q69[label = 'Q69', shape = box, color = lightcoral]
  Q70[label = 'Q70', shape = box, color = lightcoral]
  Q76[label = 'Q76', shape = box, color = lightcoral]
  
  # Variances
  Factor -> Factor[dir = both, tailport = n, headport = s, label = '0.063', color = gray, minlen = 0.5]
  Q4 -> Q4[dir = both, tailport = e, headport = w, label = '0.937', color = gray, minlen = 0.5]
  Q6 -> Q6[dir = both, tailport = e, headport = w, label = '0.592', color = gray, minlen = 0.5]
  Q8 -> Q8[dir = both, tailport = e, headport = w, label = '0.681', color = gray, minlen = 0.5]
  Q10 -> Q10[dir = both, tailport = e, headport = w, label = '0.644', color = gray, minlen = 0.5]
  Q67 -> Q67[dir = both, tailport = e, headport = w, label = '0.384', color = gray, minlen = 0.5]
  Q68 -> Q68[dir = both, tailport = e, headport = w, label = '0.549', color = gray, minlen = 0.5]
  Q69 -> Q69[dir = both, tailport = e, headport = w, label = '0.229', color = gray, minlen = 0.5]
  Q70 -> Q70[dir = both, tailport = e, headport = w, label = '0.353', color = gray, minlen = 0.5]
  Q76 -> Q76[dir = both, tailport = e, headport = w, label = '0.746', color = gray, minlen = 0.5]
}
")

print(graph)



###EDUCATION
library(DiagrammeR)

graph <- grViz("
digraph diagram {
  graph[layout = dot, rankdir = LR]
  node[width = 1]
  
  Factor[label = 'education\\n0.626', shape = ellipse, width = 1.5, color = lightblue]
  
  Factor -> Q48[label = '1.000']
  Factor -> Q100[label = '1.113']
  Factor -> Q121[label = '0.707']
  
  Q48[label = 'Q48', shape = box, color = lightcoral]
  Q100[label = 'Q100', shape = box, color = lightcoral]
  Q121[label = 'Q121', shape = box, color = lightcoral]
  
  # Variances
  Factor -> Factor[dir = both, tailport = w, headport = e, label = '0.626', color = gray, minlen = 0.5]
  Q48 -> Q48[dir = both, tailport = w, headport = e, label = '0.374', color = gray, minlen = 0.5]
  Q100 -> Q100[dir = both, tailport = w, headport = e, label = '0.225', color = gray, minlen = 0.5]
  Q121 -> Q121[dir = both, tailport = w, headport = e, label = '0.687', color = gray, minlen = 0.5]
}
")

print(graph)

### SUBJECTIVE WELL-BEING
graph <- grViz("
digraph diagram {
  graph[layout = dot, rankdir = LR]
  node[width = 1]
  
  Factor[label = 'subjwb\\n0.662', shape = ellipse, width = 1.5, color = lightblue]
  
  Factor -> Q18[label = '1.000']
  Factor -> Q53[label = '0.839']
  Factor -> Q55[label = '0.778']
  Factor -> Q57[label = '0.883']
  Factor -> Q58[label = '0.532']
  Factor -> Q66[label = '0.989']
  Factor -> Q74[label = '0.863']
  Factor -> Q77[label = '0.415']
  Factor -> Q90[label = '1.111']
  Factor -> Q91[label = '1.097']
  Factor -> Q106[label = '0.681']
  Factor -> Q111[label = '1.048']
  Factor -> Q126[label = '1.009']
  Factor -> Q127[label = '1.051']
  
  Q18[label = 'Q18', shape = box, color = lightcoral]
  Q53[label = 'Q53', shape = box, color = lightcoral]
  Q55[label = 'Q55', shape = box, color = lightcoral]
  Q57[label = 'Q57', shape = box, color = lightcoral]
  Q58[label = 'Q58', shape = box, color = lightcoral]
  Q66[label = 'Q66', shape = box, color = lightcoral]
  Q74[label = 'Q74', shape = box, color = lightcoral]
  Q77[label = 'Q77', shape = box, color = lightcoral]
  Q90[label = 'Q90', shape = box, color = lightcoral]
  Q91[label = 'Q91', shape = box, color = lightcoral]
  Q106[label = 'Q106', shape = box, color = lightcoral]
  Q111[label = 'Q111', shape = box, color = lightcoral]
  Q126[label = 'Q126', shape = box, color = lightcoral]
  Q127[label = 'Q127', shape = box, color = lightcoral]
  
  # Variances
  Factor -> Factor[dir = both, tailport = w, headport = e, label = '0.662', color = gray, minlen = 0.5]
  Q18 -> Q18[dir = both, tailport = w, headport = e, label = '0.338', color = gray, minlen = 0.5]
  Q53 -> Q53[dir = both, tailport = w, headport = e, label = '0.534', color = gray, minlen = 0.5]
  Q55 -> Q55[dir = both, tailport = w, headport = e, label = '0.600', color = gray, minlen = 0.5]
  Q57 -> Q57[dir = both, tailport = w, headport = e, label = '0.484', color = gray, minlen = 0.5]
  Q58 -> Q58[dir = both, tailport = w, headport = e, label = '0.813', color = gray, minlen = 0.5]
  Q66 -> Q66[dir = both, tailport = w, headport = e, label = '0.352', color = gray, minlen = 0.5]
  Q74 -> Q74[dir = both, tailport = w, headport = e, label = '0.507', color = gray, minlen = 0.5]
  Q77 -> Q77[dir = both, tailport = w, headport = e, label = '0.886', color = gray, minlen = 0.5]
  Q90 -> Q90[dir = both, tailport = w, headport = e, label = '0.182', color = gray, minlen = 0.5]
  Q91 -> Q91[dir = both, tailport = w, headport = e, label = '0.203', color = gray, minlen = 0.5]
  Q106 -> Q106[dir = both, tailport = w, headport = e, label = '0.693', color = gray, minlen = 0.5]
  Q111 -> Q111[dir = both, tailport = w, headport = e, label = '0.273', color = gray, minlen = 0.5]
  Q126 -> Q126[dir = both, tailport = w, headport = e, label = '0.326', color = gray, minlen = 0.5]
  Q127 -> Q127[dir = both, tailport = w, headport = e, label = '0.269', color = gray, minlen = 0.5]
}
")

print(graph)


##### SERVICES
library(DiagrammeR)

grViz("
digraph diagram {
  graph[layout = dot, rankdir = LR]
  node[width = 1]
  
  Factor[label=services, shape = ellipse, width = 1.5, color = lightblue]
  
  Factor -> Q15[label = '0.458']
  Factor -> Q20[label = '0.772']
  Factor -> Q73[label = '0.728']
  Factor -> Q120[label = '0.214']
  Factor -> Q122[label = '0.544']
  
  Q15[label = 'Q15', shape = box, color = lightcoral]
  Q20[label = 'Q20', shape = box, color = lightcoral]
  Q73[label = 'Q73', shape = box, color = lightcoral]
  Q120[label = 'Q120', shape = box, color = lightcoral]
  Q122[label = 'Q122', shape = box, color = lightcoral]
  
  # Variances
  Q15 -> Q15[dir = both, tailport = w, headport = e, label = '0.790', color = gray, minlen = 0.5]
  Q20 -> Q20[dir = both, tailport = w, headport = e, label = '0.404', color = gray, minlen = 0.5]
  Q73 -> Q73[dir = both, tailport = w, headport = e, label = '0.470', color = gray, minlen = 0.5]
  Q120 -> Q120[dir = both, tailport = w, headport = e, label = '0.954', color = gray, minlen = 0.5]
  Q122 -> Q122[dir = both, tailport = w, headport = e, label = '0.704', color = gray, minlen = 0.5]
}
")


### ECON WELL-BEING

grViz("
digraph diagram {
  graph[layout = dot, rankdir = LR]
  node[width = 1]
  
  Factor[label=econ_wb, shape = ellipse, width = 1.5, color = lightblue]
  
  Factor -> Q24[label = '0.959']
  Factor -> Q26[label = '0.869']
  Factor -> Q89[label = '0.326']
  Factor -> Q123[label = '0.429']
  Factor -> Q124[label = '0.402']
  Factor -> Q125[label = '0.517']
  Factor -> Q96[label = '0.891']
  Factor -> Q97[label = '0.678']
  
  Q24[label = 'Q24', shape = box, color = lightcoral]
  Q26[label = 'Q26', shape = box, color = lightcoral]
  Q89[label = 'Q89', shape = box, color = lightcoral]
  Q123[label = 'Q123', shape = box, color = lightcoral]
  Q124[label = 'Q124', shape = box, color = lightcoral]
  Q125[label = 'Q125', shape = box, color = lightcoral]
  Q96[label = 'Q96', shape = box, color = lightcoral]
  Q97[label = 'Q97', shape = box, color = lightcoral]
  
  # Variances
  Q24 -> Q24[dir = both, tailport = w, headport = e, label = '0.081', color = gray, minlen = 0.5]
  Q26 -> Q26[dir = both, tailport = w, headport = e, label = '0.245', color = gray, minlen = 0.5]
  Q89 -> Q89[dir = both, tailport = w, headport = e, label = '0.894', color = gray, minlen = 0.5]
  Q123 -> Q123[dir = both, tailport = w, headport = e, label = '0.816', color = gray, minlen = 0.5]
  Q124 -> Q124[dir = both, tailport = w, headport = e, label = '0.838', color = gray, minlen = 0.5]
  Q125 -> Q125[dir = both, tailport = w, headport = e, label = '0.733', color = gray, minlen = 0.5]
  Q96 -> Q96[dir = both, tailport = w, headport = e, label = '0.206', color = gray, minlen = 0.5]
  Q97 -> Q97[dir = both, tailport = w, headport = e, label = '0.540', color = gray, minlen = 0.5]
}
")

####INNOVATION

grViz("
digraph diagram {
  graph[layout = dot, rankdir = LR]
  node[width = 1]
  
  Factor[label=innovation, shape = ellipse, width = 1.5, color = lightblue]
  
  Factor -> Q47[label = '0.551']
  Factor -> Q50[label = '0.844']
  Factor -> Q51[label = '0.945']
  
  Q47[label = 'Q47', shape = box, color = lightcoral]
  Q50[label = 'Q50', shape = box, color = lightcoral]
  Q51[label = 'Q51', shape = box, color = lightcoral]
  
  # Variances
  Q47 -> Q47[dir = both, tailport = w, headport = e, label = '0.697', color = gray, minlen = 0.5]
  Q50 -> Q50[dir = both, tailport = w, headport = e, label = '0.288', color = gray, minlen = 0.5]
  Q51 -> Q51[dir = both, tailport = w, headport = e, label = '0.106', color = gray, minlen = 0.5]
}
")

###EDUCATION
grViz("
digraph diagram {
  graph[layout = dot, rankdir = LR]
  node[width = 1]
  
  Factor[label=education, shape = ellipse, width = 1.5, color = lightblue]
  
  Factor -> Q48[label = '0.791']
  Factor -> Q100[label = '0.881']
  Factor -> Q121[label = '0.559']
  
  Q48[label = 'Q48', shape = box, color = lightcoral]
  Q100[label = 'Q100', shape = box, color = lightcoral]
  Q121[label = 'Q121', shape = box, color = lightcoral]
  
  # Variances
  Q48 -> Q48[dir = both, tailport = w, headport = e, label = '0.374', color = gray, minlen = 0.5]
  Q100 -> Q100[dir = both, tailport = w, headport = e, label = '0.225', color = gray, minlen = 0.5]
  Q121 -> Q121[dir = both, tailport = w, headport = e, label = '0.687', color = gray, minlen = 0.5]
}
")


#########################################################
#######MODIFICATION INDECES

# Assume fit is your CFA model object
mod_indices <- as.data.frame(modificationindices(fit))

# Retain only the row with the highest MI
highest_mi_row <- mod_indices[which.max(mod_indices$mi), ]
highest_mi_row

############### CFA PARTICIPATION ITEMS
library(lavaan)

models <- list(
  part  = 'part =~ part_welfare + part_development + part_hr +
 part_joborg +
 part_remuneration +
 part_intcomm +
 part_extcomm +
 part_training +
 part_health +
 part_econstatus +
 part_suppliers +
 part_innovation +
 part_investments +
 part_occupation +
 part_general +
 part_area +
 part_shareholders +
 part_intconflicts +
 part_extconflicts +
 part_stakeholder +
 part_environment')

fit_part <- cfa(part_model, data=data_factor, ordered=TRUE)
summary(fit_part)

