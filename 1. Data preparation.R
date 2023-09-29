#################################################################
##  SCRIPT 1: Data preparation, composite measures computation ##
##  Author: Marta Magnani                                      ##                                        
##  Last: Sep 10th, 2023                                       ##
#################################################################

#Packages
library(readxl)
library(dplyr)

#Load the data
data_thesis <- read_excel("C:/Users/magna/OneDrive/Tesi Magistrale DSE/Analisi Script R/data_BWL.xlsx")
attach(data_thesis)

#Change polarity of well-being indicators
list_negative <- c("Q4", "Q22", "Q28", "Q29", "Q37", "Q39", "Q40", "Q45", "Q50", "Q51", "Q55", "Q58", "Q60", "Q61", "Q62", "Q63", "Q64", "Q65", "Q67", "Q68", "Q69", "Q70", "Q71", "Q77", "Q85", "Q86", "Q87", "Q88", "Q89", "Q97", "Q104", "Q114", "Q119")

#Change (1 = 5) (2 = 4) (4 = 2) (5 = 1) 
for (i in list_negative){
  data_thesis[i][data_thesis[i] == 1] <- 10
  data_thesis[i][data_thesis[i] == 2] <- 11
  data_thesis[i][data_thesis[i] == 4] <- 12
  data_thesis[i][data_thesis[i] == 5] <- 13
}

for (i in list_negative){
  data_thesis[i][data_thesis[i] == 10] <- 5
  data_thesis[i][data_thesis[i] == 11] <- 4
  data_thesis[i][data_thesis[i] == 12] <- 2
  data_thesis[i][data_thesis[i] == 13] <- 1
}

attach(data_thesis)

## Compute well-being domains and composite measure
# Create variables to store the row means
data_thesis$bes_politics <- rowMeans(data_thesis[, c("Q1", "Q17", "Q19", "Q21", "Q29", "Q35", "Q72", "Q78", "Q79", "Q80", "Q81", "Q83", "Q84", "Q115", "Q117")])
data_thesis$bes_security <- rowMeans(data_thesis[, c("Q2", "Q3", "Q7", "Q62")])
data_thesis$bes_health <- rowMeans(data_thesis[, c("Q4", "Q6", "Q8", "Q10", "Q67", "Q68", "Q69", "Q70", "Q76")])
data_thesis$bes_socialrelationships <- rowMeans(data_thesis[, c("Q5", "Q16", "Q22", "Q36", "Q37", "Q40", "Q42", "Q44", "Q54", "Q56", "Q60", "Q61", "Q85", "Q86", "Q93", "Q94", "Q95", "Q98", "Q99", "Q101", "Q102", "Q114", "Q118", "Q119")])
data_thesis$bes_worklifebalance <- rowMeans(data_thesis[, c("Q9", "Q11", "Q12", "Q23", "Q25", "Q27", "Q28", "Q30", "Q31", "Q32", "Q33", "Q34", "Q38", "Q39", "Q41", "Q43", "Q45", "Q46", "Q49", "Q52", "Q59", "Q63", "Q64", "Q65", "Q71", "Q75", "Q82","Q87", "Q88","Q92", "Q103", "Q104", "Q105", "Q107", "Q109", "Q110", "Q112", "Q116")])
data_thesis$bes_naturalculturalheritage <- rowMeans(data_thesis[, c("Q13", "Q14")])
data_thesis$bes_qualityofservices <- rowMeans(data_thesis[, c("Q15", "Q20", "Q73", "Q120", "Q122")])
data_thesis$bes_economicwellbeing <- rowMeans(data_thesis[, c("Q24", "Q26", "Q89", "Q123", "Q124", "Q125", "Q96", "Q97")])
data_thesis$bes_innovationresearchcreativity <- rowMeans(data_thesis[, c("Q47", "Q50", "Q51")])
data_thesis$bes_education <- rowMeans(data_thesis[, c("Q48", "Q100", "Q121")])
data_thesis$bes_environment <- rowMeans(data_thesis[, c("Q108", "Q113")])
data_thesis$bes_subjectivewellbeing <- rowMeans(data_thesis[, c("Q18", "Q53", "Q55", "Q57", "Q58", "Q66", "Q74", "Q77", "Q90", "Q91", "Q106", "Q111", "Q126", "Q127")])
attach(data_thesis)

data_thesis$bes_wellbeing <- (bes_politics * 15 +
                bes_security * 4 +
                bes_health * 9 +
                bes_socialrelationships * 24 +
                bes_worklifebalance * 38 +
                bes_naturalculturalheritage * 2 +
                bes_qualityofservices * 5 +
                bes_economicwellbeing * 8 +
                bes_innovationresearchcreativity * 3 +
                bes_education * 3 +
                bes_environment * 2 +
                bes_subjectivewellbeing * 14) / 127

attach(data_thesis)


##Compute the Level of Participation using the weights of NeXt's CTS
data_thesis <- data_thesis %>%
  rename(
    part_welfare = P1,
    part_development= P2,
    part_hr = P3,
    part_joborg = P4,
    part_remuneration=P5,
    part_intcomm = P6,
    part_extcomm = P7,
    part_training = P8,
    part_health = P9,
    part_econstatus = P10,
    part_suppliers = P11,
    part_innovation = P12,
    part_investments= P13,
    part_occupation = P14,
    part_general = P15,
    part_area = P16,
    part_shareholders = P17,
    part_intconflicts = P18,
    part_extconflicts = P19,
    part_stakeholder = P20,
    part_environment = P21
  )
attach(data_thesis)

data_thesis$participation_level <- (part_welfare*5.47619 + part_development*6.47619 + part_hr*4.952381 + part_joborg*5.142857 + part_remuneration*4.095238 + part_intcomm*4.761905 + part_extcomm*3.095238 + part_training*5.952381 + part_health*5.095238 + part_econstatus*3.952381 + part_suppliers*2.904762 + part_innovation*4.666667 + part_investments*3.857143 + part_occupation*5.142857 + part_general*5.761905 + part_area*3.238095 + part_shareholders*4.666667 + part_intconflicts*4.47619 + part_extconflicts*3.619048 + part_stakeholder*6.333333 + part_environment*6.333333)/100

mean(data_thesis$participation_level)

##Compute the Best Work Life (BWL) with different weights according to the organization size
data_thesis <- 
  data_thesis %>% 
  mutate(BWL = case_when(dimension== "Large" ~ (bes_wellbeing*0.85 + participation_level*0.15)*2, dimension== "Medium" ~ (bes_wellbeing*0.8 + participation_level*0.2)*2, dimension== "Small" ~ (bes_wellbeing*0.75 + participation_level*0.25)*2, dimension== "Micro" ~ (bes_wellbeing*0.7 + participation_level*0.3)*2))

attach(data_thesis)
mean(BWL)

##################################################
############# OTHER MEASURES ####################

##Compute organizational health domains and composite measure
data_thesis$health_prevention <- rowMeans(data_thesis[, c("Q1", "Q2", "Q3", "Q4", "Q5")])
data_thesis$health_workenvironment <- rowMeans(data_thesis[, c("Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15")])
data_thesis$health_activelistening <- rowMeans(data_thesis[, c("Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23")])
data_thesis$health_equity <- rowMeans(data_thesis[, c("Q24", "Q25", "Q26", "Q27", "Q28", "Q29")])
data_thesis$health_administrativeefficiency <- rowMeans(data_thesis[, c("Q30", "Q31", "Q32", "Q33", "Q34", "Q35", "Q36", "Q37")])
data_thesis$health_jobsustainability <- rowMeans(data_thesis[, c("Q38", "Q39", "Q40", "Q41", "Q42", "Q43", "Q44", "Q45", "Q46")])
data_thesis$health_openinnovation <- rowMeans(data_thesis[, c("Q47", "Q48", "Q49", "Q50", "Q51", "Q52", "Q43")])
data_thesis$health_conflicts <- rowMeans(data_thesis[, c("Q54", "Q55", "Q56", "Q57", "Q58", "Q59", "Q60", "Q61", "Q62", "Q63", "Q64")])
data_thesis$health_stresslevels <- rowMeans(data_thesis[, c("Q65", "Q66", "Q67", "Q68", "Q69", "Q70", "Q72", "Q73", "Q74", "Q75", "Q76", "Q77")])
data_thesis$health_information <- rowMeans(data_thesis[, c("Q78", "Q79", "Q80", "Q81")])
data_thesis$health_clearobjectives <- rowMeans(data_thesis[, c("Q82", "Q83", "Q84", "Q85", "Q86", "Q87", "Q88", "Q89", "Q90")])
data_thesis$health_skillsenhancement <- rowMeans(data_thesis[, c("Q90", "Q91", "Q92", "Q93", "Q94", "Q95", "Q96", "Q97", "Q98", "Q99", "Q100", "Q101", "Q102")])
data_thesis$health_meaningfulness <- rowMeans(data_thesis[, c("Q102", "Q103", "Q104", "Q105", "Q106", "Q107", "Q108", "Q109", "Q110", "Q111", "Q112", "Q113")])
data_thesis$health_relationalenvironment <- rowMeans(data_thesis[, c("Q114", "Q115", "Q116", "Q117", "Q118", "Q119", "Q126", "Q127")])
data_thesis$health_organizationalwelfare<- rowMeans(data_thesis[, c("Q120", "Q121", "Q122", "Q123", "Q124", "Q125")])

attach(data_thesis)

data_thesis$health_organization <- (health_prevention * 5 +
                                health_workenvironment * 10 +
                                health_activelistening * 8 +
                                health_equity * 6 +
                                health_administrativeefficiency*8+
                                health_jobsustainability * 9 +
                                health_openinnovation * 7 +
                                health_conflicts * 11 +
                                health_stresslevels * 13 +
                                health_information * 4 + 
                                health_clearobjectives * 8 + 
                                health_skillsenhancement * 13 +
                                health_meaningfulness * 11 + 
                                health_relationalenvironment * 8 +
                                health_organizationalwelfare * 6)/ 127

attach(data_thesis)

##Compute subjective well-being and composite measure
data_thesis$subjective_wb_managementesteem <- rowMeans(data_thesis[, c("Q1", "Q5", "Q20", "Q21", "Q29", "Q59", "Q72")])
data_thesis$subjective_wb_changeability <- rowMeans(data_thesis[, c("Q4", "Q32")])
data_thesis$subjective_wb_selfrealization <- rowMeans(data_thesis[, c("Q18", "Q48", "Q105", "Q108")])
data_thesis$subjective_wb_interpersonal <- rowMeans(data_thesis[, c("Q22", "Q60", "Q61", "Q62", "Q64", "Q94", "Q99", "Q101", "Q118")])
data_thesis$subjective_wb_managementcredibility <- rowMeans(data_thesis[, c("Q30", "Q41", "Q42", "Q44", "Q81")])
data_thesis$subjective_wb_team<- rowMeans(data_thesis[, c("Q35", "Q36", "Q40", "Q54", "Q57")])
data_thesis$subjective_wb_organizationsuccess <- rowMeans(data_thesis[, c("Q37", "Q53")])
data_thesis$subjective_wb_worklifebalance <- rowMeans(data_thesis[, c("Q38", "Q39", "Q52", "Q67", "Q69", "Q70", "Q71", "Q75", "Q77")])
data_thesis$subjective_wb_values <- rowMeans(data_thesis[, c("Q78", "Q79", "Q80", "Q116")])
data_thesis$subjective_wb_gotowork <- rowMeans(data_thesis[, c("Q110")])
data_thesis$subjective_wb_satisfaction <- rowMeans(data_thesis[, c("Q103", "Q107")])
data_thesis$subjective_wb_commitment <- rowMeans(data_thesis[, c("Q112")])

attach(data_thesis)

data_thesis$subjective_wellbeing <- (subjective_wb_managementesteem * 7 +
                                     subjective_wb_changeability * 2 +
                                     subjective_wb_selfrealization * 4 +
                                     subjective_wb_interpersonal * 9 +
                                     subjective_wb_managementcredibility * 5 +
                                     subjective_wb_team * 5 +
                                     subjective_wb_organizationsuccess * 2 + 
                                     subjective_wb_worklifebalance * 9 + 
                                     subjective_wb_values * 4 +
                                     subjective_wb_gotowork * 1 +
                                     subjective_wb_satisfaction * 2 +
                                     subjective_wb_commitment * 1) / 51

attach(data_thesis)

##Compute subjective malaise and composite measure
data_thesis$subjective_ma_organizationalconfusion <- rowMeans(data_thesis[, c("Q27", "Q45", "Q46", "Q87")])
data_thesis$subjective_ma_absenteeism <- rowMeans(data_thesis[, c("Q65")])
data_thesis$subjective_ma_changejob <- rowMeans(data_thesis[, c("Q104")])
data_thesis$subjective_ma_irrelevance <- rowMeans(data_thesis[, c("Q85", "Q86", "Q109")])
data_thesis$subjective_ma_unrecognized <- rowMeans(data_thesis[, c("Q24", "Q26", "Q90", "Q91", "Q96", "Q97")])
data_thesis$subjective_ma_unusefulness<- rowMeans(data_thesis[, c("Q92", "Q95")])
data_thesis$subjective_ma_rulesadherence <- rowMeans(data_thesis[, c("Q88")])
data_thesis$subjective_ma_proactivity <- rowMeans(data_thesis[, c("Q17", "Q102")])
data_thesis$subjective_ma_jobinterest <- rowMeans(data_thesis[, c("Q23", "Q111")])
data_thesis$subjective_ma_resentment <- rowMeans(data_thesis[, c("Q28", "Q63", "Q89")])
data_thesis$subjective_ma_slowness <- rowMeans(data_thesis[, c("Q31", "Q34")])
data_thesis$subjective_ma_aggressive <- rowMeans(data_thesis[, c("Q55")])
data_thesis$subjective_ma_gossip <- rowMeans(data_thesis[, c("Q114", "Q119")])

attach(data_thesis)

data_thesis$subjective_malaise <- (
                                   subjective_ma_absenteeism * 1 +
                                   subjective_ma_changejob * 1 +
                                   subjective_ma_irrelevance * 3 +
                                   subjective_ma_unrecognized * 6 +
                                   subjective_ma_unusefulness * 2 +
                                   subjective_ma_rulesadherence * 1 + 
                                   subjective_ma_proactivity * 2 + 
                                   subjective_ma_jobinterest * 2 +                                 
                                   subjective_ma_organizationalconfusion * 4 +
                                   subjective_ma_resentment * 3 +
                                   subjective_ma_aggressive * 1 + 
                                   subjective_ma_slowness * 2 + 
                                   subjective_ma_gossip * 2) / 30

attach(data_thesis)

##Compute work-related stress and composite measure
data_thesis$stress_change <- rowMeans(data_thesis[, c("Q47", "Q78", "Q79", "Q80")])
data_thesis$stress_colleagues <- rowMeans(data_thesis[, c("Q34", "Q36", "Q40", "Q54", "Q57", "Q58", "Q64", "Q90", "Q98", "Q99")])
data_thesis$stress_control <- rowMeans(data_thesis[, c("Q31", "Q52", "Q75", "Q76", "Q100", "Q116")])
data_thesis$stress_demand <- rowMeans(data_thesis[, c("Q1", "Q38", "Q39", "Q42", "Q43", "Q44", "Q45", "Q46", "Q67", "Q68", "Q71", "Q92", "Q95", "Q115")])
data_thesis$stress_management <- rowMeans(data_thesis[, c("Q2", "Q5", "Q8", "Q9", "Q11", "Q12", "Q15", "Q18", "Q20", "Q30", "Q32", "Q37", "Q49", "Q59", "Q63", "Q72", "Q89", "Q91")])
data_thesis$stress_relations<- rowMeans(data_thesis[, c("Q4", "Q17", "Q22", "Q60", "Q61", "Q62", "Q85", "Q86", "Q94", "Q101", "Q102", "Q114", "Q118", "Q119")])
data_thesis$stress_role<- rowMeans(data_thesis[, c("Q24", "Q25", "Q26", "Q27", "Q82", "Q84", "Q87", "Q88", "Q96", "Q97", "Q105")])

attach(data_thesis)

data_thesis$stress <- (
    stress_change * 4 +
    stress_colleagues * 10 +
    stress_control * 6 +
    stress_demand * 14 +
    stress_management * 18 +
    stress_relations * 14 + 
    stress_role * 11) / 77

attach(data_thesis)

#####SAVE THIS MANIPULATED DATA IN A NEW FILE
###Save the data
write.csv(data_thesis, "C:/Users/magna/OneDrive/Tesi Magistrale DSE/Analisi Script R/data_ready.csv", row.names=FALSE)
