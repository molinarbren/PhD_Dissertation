###Install.Packages to Install Important Packages###
#install.packages("dplyr") #For Data manipulation#
#install.packages("plyr") #For Data manipulation#
#install.packages("psych") #For Psychology Based Statistics and Analyses#
#install.packages("tidyr") #For Data Cleaning#
#install.packages("readxl") #To Read in Excel Documents#
#install.packages("rematch") #For readxl#
#install.packages("lsr") #For freq function#
#install.packages("missForest") #Uses Nonparametric Random Forest modeling to perform missing data imputation#
#install.packages("nFactors") #Scree Test and Factor Dimension Extraction#
#install.packages("Hmisc") #For Correlation Matrix with P values#
#install.packages("ggplot2") #For Visualization of Data#
#install.packages("RColorBrewer") #Color Pallets for ggplot2
#install.packages("mediation") #To Conduct Mediational Analyses#
#install.packages("questionr") #Frequency Function


###Load Necessary Packages into Workspace###
#library(Hmisc)
library(plyr)
library(dplyr)
library(psych)
library(tidyr)
library(readxl)
library(rematch)
library(lsr)
library(missForest)
library(randomForest)
library(nFactors)
library(ggplot2)
library(RColorBrewer) 
library(mediation)
library(questionr)


#####Load the Data into the work Environment#####
##Create List of variable Names##
Var_Col = c("Start_Time", "End_Time", "IP_Address", "Progress", "Duration_Secs", "Finished", "Comp_Date", "ID", "IP", "Lang", "Consent", "Present_1", "Present_2", "Present_3", "Present_4", "Present_5", "Present_6", "Manip_Check_1", "Present_First_Click",
"Present_Last_Click", "Present_Page_Submit", "Present_Click_Count", "Best_Future_1", "Best_Future_2", "Best_Future_3", "Best_Future_4", "Best_Future_5", "Best_Future_6", "Best_Future_Likelihood", "Manip_Check_2", "Best_Future_First_Click", "Best_Future_Last_Click",
"Best_Future_Page_Submit", "Best_Future_Click_Count", "Worst_Future_1", "Worst_Future_2", "Worst_Future_3", "Worst_Future_4", "Worst_Future_5", "Worst_Future_6", "Worst_Future_Likelihood", "Manip_Check_3", "Worst_Future_First_Click", "Worst_Future_Last_Click", "Worst_Future_Page_Submit",
"Worst_Future_Click_Count", "FWB_1", "FWB_2", "FWB_3", "FWB_4", "FWB_5", "FWB_6", "FWB_7", "FWB_8", "AL_1", "AL_2", "AL_3", "AL_4", "AL_5", "AL_6", "AL_7", "AL_8", "AL_9", "AL_10", "AL_11", "AL_12", "AL_13", "AL_14", "AL_15", "AL_16", "AL_17", "AL_18", "AL_19", "AL_20", "RWA_1", "RWA_2", "RWA_3", "RWA_4", "RWA_5", "RWA_6",
"RWA_7", "RWA_8", "RWA_9", "RWA_10", "RWA_11", "RWA_12", "RWA_13", "RWA_14", "RWA_15", "PHNS_1", "PHNS_2", "PHNS_3", "PHNS_4", "PHNS_5", "PHNS_6", "PHNS_7", "PHNS_8", "PHNS_9", "PHNS_10", "PHNS_11", "PHNS_12", "PHNS_13", "PHNS_14", "LOT_1", "LOT_2", "LOT_3", "LOT_4", "LOT_5", "LOT_6", "D1",
"D2", "D3", "D3_TXT", "D4", "D5", "D5_TXT", "D6", "D6_TXT", "D7", "D8", "D9", "D10", "D10_TXT", "D11", "D11_TXT", "D12", "D13", "D14", "D15", "Comments", "Code", "Society_Random_Order", "Survey_Random_Order")

###Load CSV File with Data from Dropbox###
#?read_excel #Use this function to read in Excel Document
Raw_Study_One_Data <- read_excel("C:/Users/bmoli/Documents/Dissertation/Dissertation Study 1 (Full) w Randomization.xlsx", range = "A4:EC513", sheet = "Dissertation Study w Part 2", col_names = Var_Col)


##Verify Structure of the Data##
str(Raw_Study_One_Data, list.len = 140)
summary(Raw_Study_One_Data$Duration_Secs/60) ##Average Time to Complete Study in Minutes##


#####Factors for Missing Value Imputation##########
Raw_Study_One_Data$FWB_1 = as.factor(Raw_Study_One_Data$FWB_1)
Raw_Study_One_Data$FWB_2 = as.factor(Raw_Study_One_Data$FWB_2)
Raw_Study_One_Data$FWB_3 = as.factor(Raw_Study_One_Data$FWB_3)
Raw_Study_One_Data$FWB_4 = as.factor(Raw_Study_One_Data$FWB_4)
Raw_Study_One_Data$FWB_5 = as.factor(Raw_Study_One_Data$FWB_5)
Raw_Study_One_Data$FWB_6 = as.factor(Raw_Study_One_Data$FWB_6)
Raw_Study_One_Data$FWB_7 = as.factor(Raw_Study_One_Data$FWB_7)
Raw_Study_One_Data$FWB_8 = as.factor(Raw_Study_One_Data$FWB_8)

Raw_Study_One_Data$AL_1 = as.factor(Raw_Study_One_Data$AL_1)
Raw_Study_One_Data$AL_2 = as.factor(Raw_Study_One_Data$AL_2)
Raw_Study_One_Data$AL_3 = as.factor(Raw_Study_One_Data$AL_3)
Raw_Study_One_Data$AL_4 = as.factor(Raw_Study_One_Data$AL_4)
Raw_Study_One_Data$AL_5 = as.factor(Raw_Study_One_Data$AL_5)
Raw_Study_One_Data$AL_6 = as.factor(Raw_Study_One_Data$AL_6)
Raw_Study_One_Data$AL_7 = as.factor(Raw_Study_One_Data$AL_7)
Raw_Study_One_Data$AL_8 = as.factor(Raw_Study_One_Data$AL_8)
Raw_Study_One_Data$AL_9 = as.factor(Raw_Study_One_Data$AL_9)
Raw_Study_One_Data$AL_10 = as.factor(Raw_Study_One_Data$AL_10)
Raw_Study_One_Data$AL_11 = as.factor(Raw_Study_One_Data$AL_11)
Raw_Study_One_Data$AL_12 = as.factor(Raw_Study_One_Data$AL_12)
Raw_Study_One_Data$AL_13 = as.factor(Raw_Study_One_Data$AL_13)
Raw_Study_One_Data$AL_14 = as.factor(Raw_Study_One_Data$AL_14)
Raw_Study_One_Data$AL_15 = as.factor(Raw_Study_One_Data$AL_15)
Raw_Study_One_Data$AL_16 = as.factor(Raw_Study_One_Data$AL_16)
Raw_Study_One_Data$AL_17 = as.factor(Raw_Study_One_Data$AL_17)
Raw_Study_One_Data$AL_18 = as.factor(Raw_Study_One_Data$AL_18)
Raw_Study_One_Data$AL_19 = as.factor(Raw_Study_One_Data$AL_19)
Raw_Study_One_Data$AL_20 = as.factor(Raw_Study_One_Data$AL_20)

Raw_Study_One_Data$RWA_1 = as.factor(Raw_Study_One_Data$RWA_1)
Raw_Study_One_Data$RWA_2 = as.factor(Raw_Study_One_Data$RWA_2)
Raw_Study_One_Data$RWA_3 = as.factor(Raw_Study_One_Data$RWA_3)
Raw_Study_One_Data$RWA_4 = as.factor(Raw_Study_One_Data$RWA_4)
Raw_Study_One_Data$RWA_5 = as.factor(Raw_Study_One_Data$RWA_5)
Raw_Study_One_Data$RWA_6 = as.factor(Raw_Study_One_Data$RWA_6)
Raw_Study_One_Data$RWA_7 = as.factor(Raw_Study_One_Data$RWA_7)
Raw_Study_One_Data$RWA_8 = as.factor(Raw_Study_One_Data$RWA_8)
Raw_Study_One_Data$RWA_9 = as.factor(Raw_Study_One_Data$RWA_9)
Raw_Study_One_Data$RWA_10 = as.factor(Raw_Study_One_Data$RWA_10)
Raw_Study_One_Data$RWA_11 = as.factor(Raw_Study_One_Data$RWA_11)
Raw_Study_One_Data$RWA_12 = as.factor(Raw_Study_One_Data$RWA_12)
Raw_Study_One_Data$RWA_13 = as.factor(Raw_Study_One_Data$RWA_13)
Raw_Study_One_Data$RWA_14 = as.factor(Raw_Study_One_Data$RWA_14)
Raw_Study_One_Data$RWA_15 = as.factor(Raw_Study_One_Data$RWA_15)

Raw_Study_One_Data$PHNS_1 = as.factor(Raw_Study_One_Data$PHNS_1)
Raw_Study_One_Data$PHNS_2 = as.factor(Raw_Study_One_Data$PHNS_2)
Raw_Study_One_Data$PHNS_3 = as.factor(Raw_Study_One_Data$PHNS_3)
Raw_Study_One_Data$PHNS_4 = as.factor(Raw_Study_One_Data$PHNS_4)
Raw_Study_One_Data$PHNS_5 = as.factor(Raw_Study_One_Data$PHNS_5)
Raw_Study_One_Data$PHNS_6 = as.factor(Raw_Study_One_Data$PHNS_6)
Raw_Study_One_Data$PHNS_7 = as.factor(Raw_Study_One_Data$PHNS_7)
Raw_Study_One_Data$PHNS_8 = as.factor(Raw_Study_One_Data$PHNS_8)
Raw_Study_One_Data$PHNS_9 = as.factor(Raw_Study_One_Data$PHNS_9)
Raw_Study_One_Data$PHNS_10 = as.factor(Raw_Study_One_Data$PHNS_10)
Raw_Study_One_Data$PHNS_11 = as.factor(Raw_Study_One_Data$PHNS_11)
Raw_Study_One_Data$PHNS_12 = as.factor(Raw_Study_One_Data$PHNS_12)
Raw_Study_One_Data$PHNS_13 = as.factor(Raw_Study_One_Data$PHNS_13)
Raw_Study_One_Data$PHNS_14 = as.factor(Raw_Study_One_Data$PHNS_14)

Raw_Study_One_Data$LOT_1 = as.factor(Raw_Study_One_Data$LOT_1)
Raw_Study_One_Data$LOT_2 = as.factor(Raw_Study_One_Data$LOT_2)
Raw_Study_One_Data$LOT_3 = as.factor(Raw_Study_One_Data$LOT_3)
Raw_Study_One_Data$LOT_4 = as.factor(Raw_Study_One_Data$LOT_4)
Raw_Study_One_Data$LOT_5 = as.factor(Raw_Study_One_Data$LOT_5)
Raw_Study_One_Data$LOT_6 = as.factor(Raw_Study_One_Data$LOT_6)

Raw_Study_One_Data$D2 = as.factor(Raw_Study_One_Data$D2)
Raw_Study_One_Data$D3 = as.factor(Raw_Study_One_Data$D3)
Raw_Study_One_Data$D4 = as.factor(Raw_Study_One_Data$D4)
Raw_Study_One_Data$D5 = as.factor(Raw_Study_One_Data$D5)
Raw_Study_One_Data$D6 = as.factor(Raw_Study_One_Data$D6)
Raw_Study_One_Data$D7 = as.factor(Raw_Study_One_Data$D7)
Raw_Study_One_Data$D8 = as.factor(Raw_Study_One_Data$D8)
Raw_Study_One_Data$D10 = as.factor(Raw_Study_One_Data$D10)
Raw_Study_One_Data$D11 = as.factor(Raw_Study_One_Data$D11)
Raw_Study_One_Data$D12 = as.factor(Raw_Study_One_Data$D12)
Raw_Study_One_Data$D13 = as.factor(Raw_Study_One_Data$D13)
Raw_Study_One_Data$D14 = as.factor(Raw_Study_One_Data$D14)
Raw_Study_One_Data$D15 = as.factor(Raw_Study_One_Data$D15)



########Missing Data Random Forest Model##########
Missing_Data_RF_FRAME <- dplyr::select(Raw_Study_One_Data, Present_1, Present_2, Present_3, Present_4, Present_5, Present_6, Present_First_Click, Present_Last_Click, Present_Page_Submit, Present_Click_Count, Best_Future_1, Best_Future_2, Best_Future_3, Best_Future_4,
Best_Future_5, Best_Future_6, Best_Future_Likelihood, Best_Future_First_Click, Best_Future_Last_Click, Best_Future_Page_Submit, Best_Future_Click_Count, Worst_Future_1, Worst_Future_2, Worst_Future_3, Worst_Future_4, Worst_Future_5, Worst_Future_6, Worst_Future_Likelihood,
Worst_Future_First_Click, Worst_Future_Last_Click, Worst_Future_Page_Submit, Worst_Future_Click_Count, FWB_1, FWB_2, FWB_3, FWB_4, FWB_5, FWB_6, FWB_7, FWB_8, AL_1, AL_2, AL_3, AL_4, AL_5, AL_6, AL_7, AL_8, AL_9, AL_10, AL_11, AL_12, AL_13, AL_14, AL_15, AL_16, AL_17, AL_18, AL_19, AL_20,
RWA_1, RWA_2, RWA_3, RWA_4, RWA_5, RWA_6, RWA_7, RWA_8, RWA_9, RWA_10, RWA_11, RWA_12, RWA_13, RWA_14, RWA_15, PHNS_1, PHNS_2, PHNS_3, PHNS_4, PHNS_5, PHNS_6, PHNS_7, PHNS_8, PHNS_9, PHNS_10, PHNS_11, PHNS_12, PHNS_13, PHNS_14, LOT_1, LOT_2, LOT_3, LOT_4, LOT_5, LOT_6, D1, D2, D3, D4, D5, D6, D7, D8, D10, D11, D12, D13, D14, D15)

varClass(Missing_Data_RF_FRAME)

##Number of Missing Values in Frame##
freq(is.na(Missing_Data_RF_FRAME)) #897/53550 = 1.7%#

#?na.omit
Missing_Data_RF_FRAME_Training = as.data.frame(na.omit(Missing_Data_RF_FRAME))
Missing_Data_RF_FRAME_Test = prodNA(Missing_Data_RF_FRAME_Training, noNA = 0.1)
#freq(is.na(Missing_Data_RF_FRAME_Training))
str(Missing_Data_RF_FRAME_Training, list.len = 120)

###Random Forest Model to Predict Missing Numeric values###
set.seed(18) #For Reproducability#
Missing_Data_RF_Model <- missForest(as.data.frame(Missing_Data_RF_FRAME), maxiter = 100, ntree = 100, variablewise = FALSE)
Missing_Data_RF_Model$OOBerror #Normalized Root Mean Squared Error to computer percentage out of bag error#
Imputed_Data <- as.tbl(as.data.frame(Missing_Data_RF_Model$ximp)) #Assign to tibble object with imputation#
table(is.na(Imputed_Data)) #No Missing Data Points#
str(Imputed_Data, list.len = 120) #See Data Structure#
class(Imputed_Data) #Is Tibble


ALT_Missing_Data_RF_Model <- missForest(as.data.frame(Missing_Data_RF_FRAME_Test), maxiter = 10, ntree = 300, variablewise = FALSE, xtrue = Missing_Data_RF_FRAME_Training)
ALT_Missing_Data_RF_Model$OOBerror #Normalized Root Mean Squared Error to computer percentage out of bag error#
ALT_Missing_Data_RF_Model$error
mean(Missing_Data_RF_FRAME_Training == ALT_Missing_Data_RF_Model$ximp)
#mixError(ALT_Missing_Data_RF_Model$ximp, Missing_Data_RF_FRAME_Test, Missing_Data_RF_FRAME_Training)


#?missForest
#warnings() #Can only include numeric and logical variables#
#nrmse(Missing_Data_RF_Model$ximp, as.matrix(Missing_Data_RF_FRAME), xtrue = NULL)


#which( colnames(df)=="b" )
#First_Variables <- dplyr::select(Raw_Study_One_Data, 1:11) #First Eleven Variables#
Demo_1 <- dplyr::select(Raw_Study_One_Data, D2, D3, D3_TXT)
Demo_2 <- dplyr::select(Raw_Study_One_Data, D5, D5_TXT, D6, D6_TXT, D7, D8, D9, D10, D10_TXT, D11, D11_TXT)
Demo_3 <- dplyr::select(Raw_Study_One_Data, Comments, Code, Society_Random_Order, Survey_Random_Order)

str(Imputed_Data, list.len = 110)
Imputed_Raw_Data_Study_One <- cbind(Raw_Study_One_Data[, 1:11], Imputed_Data[,1:100], Demo_1, Imputed_Data[,101], Demo_2, Imputed_Data[,102:105], Demo_3)
str(Imputed_Raw_Data_Study_One, list.len = 150)


#####Recode Data Variables for Cleanining######


###Free Will Beliefs Scale###
Raw_Study_One_Data$FWB_1 = recode(Raw_Study_One_Data$FWB_1, `Strongly Disagree` = 1, `Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Agree` = 6, `Strongly Agree` = 7) #People have complete control over the decisions they make#
#summary(Raw_Study_One_Data$FWB_1) #Mean: 5.3#
Raw_Study_One_Data$FWB_2 = recode(Raw_Study_One_Data$FWB_2, `Strongly Disagree` = 1, `Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Agree` = 6, `Strongly Agree` = 7) #People must take full responsibility for any bad choices they make#
#summary(Raw_Study_One_Data$FWB_2) #Mean: 5.8#
Raw_Study_One_Data$FWB_3 = recode(Raw_Study_One_Data$FWB_3, `Strongly Disagree` = 1, `Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Agree` = 6, `Strongly Agree` = 7) #People can overcome any obstacles if they truly wan to#
#summary(Raw_Study_One_Data$FWB_3) #Mean: 5.1#
Raw_Study_One_Data$FWB_4 = recode(Raw_Study_One_Data$FWB_4, `Strongly Disagree` = 1, `Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Agree` = 6, `Strongly Agree` = 7) #People are totally responsible for the bad things they do#
#summary(Raw_Study_One_Data$FWB_4) #Mean: 5.4#
Raw_Study_One_Data$FWB_5 = recode(Raw_Study_One_Data$FWB_5, `Strongly Disagree` = 1, `Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Agree` = 6, `Strongly Agree` = 7) #People have complete free will#
#summary(Raw_Study_One_Data$FWB_5) #Mean: 5.2#
Raw_Study_One_Data$FWB_6 = recode(Raw_Study_One_Data$FWB_6, `Strongly Disagree` = 1, `Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Agree` = 6, `Strongly Agree` = 7) #Criminals are totally responsible for the bad things they do#
#summary(Raw_Study_One_Data$FWB_6) #Mean: 5.5#
Raw_Study_One_Data$FWB_7 = recode(Raw_Study_One_Data$FWB_7, `Strongly Disagree` = 1, `Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Agree` = 6, `Strongly Agree` = 7) #People are always at fault for their bad behavior#
#summary(Raw_Study_One_Data$FWB_7) #Mean: 5.2#
Raw_Study_One_Data$FWB_8 = recode(Raw_Study_One_Data$FWB_8, `Strongly Disagree` = 1, `Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Agree` = 6, `Strongly Agree` = 7) #Strength of will can always overcome the body's desires#
#summary(Raw_Study_One_Data$FWB_8) #Mean: 5.1#


###Totalitarianism/Authoritarianims and Libertarianims Scale###
##Libertarianism High##
Raw_Study_One_Data$AL_1 = recode(Raw_Study_One_Data$AL_1, `Strong Disagreement` = 1, `Moderate Disagreement` = 2, `Slight Disagreement` = 3, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 5, `Moderate Agreement` = 6, `Strong Agreement` = 7) #Excessive taxation is a prime example of the way in which governments take away individual freedom#
#summary(Raw_Study_One_Data$AL_1) #Mean: 4.9#
Raw_Study_One_Data$AL_2 = recode(Raw_Study_One_Data$AL_2, `Strong Disagreement` = 7, `Moderate Disagreement` = 6, `Slight Disagreement` = 5, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 3, `Moderate Agreement` = 2, `Strong Agreement` = 1) #REVERSED: We need a stronger government to create a better society#
#summary(Raw_Study_One_Data$AL_2) #Mean: 3.9#
Raw_Study_One_Data$AL_3 = recode(Raw_Study_One_Data$AL_3, `Strong Disagreement` = 1, `Moderate Disagreement` = 2, `Slight Disagreement` = 3, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 5, `Moderate Agreement` = 6, `Strong Agreement` = 7) #Government programs discourage individual responsibility and achievement while fostering dependency and failure#
#summary(Raw_Study_One_Data$AL_3) #Mean: 4.1#
Raw_Study_One_Data$AL_4 = recode(Raw_Study_One_Data$AL_4, `Strong Disagreement` = 7, `Moderate Disagreement` = 6, `Slight Disagreement` = 5, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 3, `Moderate Agreement` = 2, `Strong Agreement` = 1) #REVERSED: In my kind of ideal society, all basic needs (food, housing, healthcare, education) will be gauranteed by the government for everyone#
#summary(Raw_Study_One_Data$AL_4) #Mean: 3.4#
Raw_Study_One_Data$AL_5 = recode(Raw_Study_One_Data$AL_5, `Strong Disagreement` = 1, `Moderate Disagreement` = 2, `Slight Disagreement` = 3, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 5, `Moderate Agreement` = 6, `Strong Agreement` = 7) #The more powerful a government becomes, the greater is the risk that it will become corrupt and unresponsive to the will of its people#
#summary(Raw_Study_One_Data$AL_5) #Mean: 5.3#
Raw_Study_One_Data$AL_6 = recode(Raw_Study_One_Data$AL_6, `Strong Disagreement` = 7, `Moderate Disagreement` = 6, `Slight Disagreement` = 5, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 3, `Moderate Agreement` = 2, `Strong Agreement` = 1) #REVERSED: A fair society is not possible without strict and comprehensive government controls#
#summary(Raw_Study_One_Data$AL_6) #Mean: 4.2#
Raw_Study_One_Data$AL_7 = recode(Raw_Study_One_Data$AL_7, `Strong Disagreement` = 1, `Moderate Disagreement` = 2, `Slight Disagreement` = 3, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 5, `Moderate Agreement` = 6, `Strong Agreement` = 7) #Individuals create wealth and governments tax it away to promote the interests of those in control#
#summary(Raw_Study_One_Data$AL_7) #Mean: 4.6#
Raw_Study_One_Data$AL_8 = recode(Raw_Study_One_Data$AL_8, `Strong Disagreement` = 1, `Moderate Disagreement` = 2, `Slight Disagreement` = 3, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 5, `Moderate Agreement` = 6, `Strong Agreement` = 7) #Individual freedom and opportunity are greater when government is smaller and less able to intervene in social and economic areas#
#summary(Raw_Study_One_Data$AL_8) #Mean: 4.6#
Raw_Study_One_Data$AL_9 = recode(Raw_Study_One_Data$AL_9, `Strong Disagreement` = 7, `Moderate Disagreement` = 6, `Slight Disagreement` = 5, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 3, `Moderate Agreement` = 2, `Strong Agreement` = 1) #REVERSED: Government laws and regulations make it possible to have a moral society#
#summary(Raw_Study_One_Data$AL_9) #Mean: 3.4#
Raw_Study_One_Data$AL_10 = recode(Raw_Study_One_Data$AL_10, `Strong Disagreement` = 1, `Moderate Disagreement` = 2, `Slight Disagreement` = 3, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 5, `Moderate Agreement` = 6, `Strong Agreement` = 7) #I am entitled to the fruits of my own labor; not to that of others passed on to me through government handouts#
#summary(Raw_Study_One_Data$AL_10) #Mean: 4.6#
Raw_Study_One_Data$AL_11 = recode(Raw_Study_One_Data$AL_11, `Strong Disagreement` = 7, `Moderate Disagreement` = 6, `Slight Disagreement` = 5, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 3, `Moderate Agreement` = 2, `Strong Agreement` = 1) #REVERSED: I am willing to exchange my personal freedoms for greater security provided by government programs#
#summary(Raw_Study_One_Data$AL_11) #Mean: 4.5#
Raw_Study_One_Data$AL_12 = recode(Raw_Study_One_Data$AL_12, `Strong Disagreement` = 7, `Moderate Disagreement` = 6, `Slight Disagreement` = 5, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 3, `Moderate Agreement` = 2, `Strong Agreement` = 1) #REVERSED: We need strict government intervention to ensure that everyone will succeed socially and economically#
#summary(Raw_Study_One_Data$AL_12) #Mean: 4.2#
Raw_Study_One_Data$AL_13 = recode(Raw_Study_One_Data$AL_13, `Strong Disagreement` = 1, `Moderate Disagreement` = 2, `Slight Disagreement` = 3, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 5, `Moderate Agreement` = 6, `Strong Agreement` = 7) #Typically, government agencies spend our money carelessly and wastefully, which is natural, since they don't have to earn it#
#summary(Raw_Study_One_Data$AL_13) #Mean: 4.7#
Raw_Study_One_Data$AL_14 = recode(Raw_Study_One_Data$AL_14, `Strong Disagreement` = 1, `Moderate Disagreement` = 2, `Slight Disagreement` = 3, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 5, `Moderate Agreement` = 6, `Strong Agreement` = 7) #Most of our econmic woes are caused by repeated and massive government meddling in the economy#
#summary(Raw_Study_One_Data$AL_14) #Mean: 4.5#
Raw_Study_One_Data$AL_15 = recode(Raw_Study_One_Data$AL_15, `Strong Disagreement` = 7, `Moderate Disagreement` = 6, `Slight Disagreement` = 5, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 3, `Moderate Agreement` = 2, `Strong Agreement` = 1) #REVERSED: Our government is not active enough; we need more laws and government programs to regulate and improve our lives and dealings with each other#
#summary(Raw_Study_One_Data$AL_15) #Mean: 4.2#
Raw_Study_One_Data$AL_16 = recode(Raw_Study_One_Data$AL_16, `Strong Disagreement` = 1, `Moderate Disagreement` = 2, `Slight Disagreement` = 3, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 5, `Moderate Agreement` = 6, `Strong Agreement` = 7) #As a government gets bigger and more powerful, its citizens become poorer and less free#
#summary(Raw_Study_One_Data$AL_16) #Mean: 4.7#
Raw_Study_One_Data$AL_17 = recode(Raw_Study_One_Data$AL_17, `Strong Disagreement` = 7, `Moderate Disagreement` = 6, `Slight Disagreement` = 5, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 3, `Moderate Agreement` = 2, `Strong Agreement` = 1) #REVERSED: For me, government-imposed social order and security are more important than individual freedom#
#summary(Raw_Study_One_Data$AL_17) #Mean: 4.9#
Raw_Study_One_Data$AL_18 = recode(Raw_Study_One_Data$AL_18, `Strong Disagreement` = 7, `Moderate Disagreement` = 6, `Slight Disagreement` = 5, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 3, `Moderate Agreement` = 2, `Strong Agreement` = 1) #REVERSED: Our society can improve only with more government controls over over individuals and businesses#
#summary(Raw_Study_One_Data$AL_18) #Mean: 4.7#
Raw_Study_One_Data$AL_19 = recode(Raw_Study_One_Data$AL_19, `Strong Disagreement` = 1, `Moderate Disagreement` = 2, `Slight Disagreement` = 3, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 5, `Moderate Agreement` = 6, `Strong Agreement` = 7) #My ideal government would be very small and would only perform a very few essential functions#
#summary(Raw_Study_One_Data$AL_19) #Mean: 4.5#
Raw_Study_One_Data$AL_20 = recode(Raw_Study_One_Data$AL_20, `Strong Disagreement` = 7, `Moderate Disagreement` = 6, `Slight Disagreement` = 5, `Neither Agree Nor Disagree` = 4, `Slight Agreement` = 3, `Moderate Agreement` = 2, `Strong Agreement` = 1) #REVERSED: Government must limit our individual freedoms so as to prevent unchecked selfishness, greed and immorality#
#summary(Raw_Study_One_Data$AL_20) #Mean: 4.8#


###Right Wing Authoritarianism###
##High = Authoritarian##
Raw_Study_One_Data$RWA_1 = recode(Raw_Study_One_Data$RWA_1, `Very Negative` =  1, `Moderately Negative` = 2, `Slightly Negative` = 3, `Neutral` = 4, `Slightly Positive` = 5, `Moderately Positive` = 6, `Very Positive` = 7) #Our country needs a powerful leader, in order to destroy the radical and immoral currents prevailing in society today.#
#summary(Raw_Study_One_Data$RWA_1) #Mean: 3.8#
Raw_Study_One_Data$RWA_2 = recode(Raw_Study_One_Data$RWA_2, `Very Negative` =  7, `Moderately Negative` = 6, `Slightly Negative` = 5, `Neutral` = 4, `Slightly Positive` = 3, `Moderately Positive` = 2, `Very Positive` = 1) #REVERSED: Our country needs free thinkers, who will have the courage to stand up against traditional ways, even if this upsets many people.#
#summary(Raw_Study_One_Data$RWA_2) #Mean: 2.7#
Raw_Study_One_Data$RWA_3 = recode(Raw_Study_One_Data$RWA_3, `Very Negative` =  1, `Moderately Negative` = 2, `Slightly Negative` = 3, `Neutral` = 4, `Slightly Positive` = 5, `Moderately Positive` = 6, `Very Positive` = 7) #The "old-fashioned ways" and "old-fashioned values" still show the best way to live.#
#summary(Raw_Study_One_Data$RWA_3) #Mean: 3.8#
Raw_Study_One_Data$RWA_4 = recode(Raw_Study_One_Data$RWA_4, `Very Negative` =  7, `Moderately Negative` = 6, `Slightly Negative` = 5, `Neutral` = 4, `Slightly Positive` = 3, `Moderately Positive` = 2, `Very Positive` = 1) #REVERSED: Our society would be better off if we showed tolerance and understanding for nontraditional values and opinions.#
#summary(Raw_Study_One_Data$RWA_4) #Mean: 2.7#
Raw_Study_One_Data$RWA_5 = recode(Raw_Study_One_Data$RWA_5, `Very Negative` =  1, `Moderately Negative` = 2, `Slightly Negative` = 3, `Neutral` = 4, `Slightly Positive` = 5, `Moderately Positive` = 6, `Very Positive` = 7) #God's laws about abortion, pornography and marriage must be strictly followed before it is too late, violations must be punished.#
#summary(Raw_Study_One_Data$RWA_5) #Mean: 3.0#
Raw_Study_One_Data$RWA_6 = recode(Raw_Study_One_Data$RWA_6, `Very Negative` =  7, `Moderately Negative` = 6, `Slightly Negative` = 5, `Neutral` = 4, `Slightly Positive` = 3, `Moderately Positive` = 2, `Very Positive` = 1) #REVERSED: The society needs to show openness towards people thinking differently, rather than a strong leader, the world is not particularly evil or dangerous.#
#summary(Raw_Study_One_Data$RWA_6) #Mean: 2.9#
Raw_Study_One_Data$RWA_7 = recode(Raw_Study_One_Data$RWA_7, `Very Negative` =  1, `Moderately Negative` = 2, `Slightly Negative` = 3, `Neutral` = 4, `Slightly Positive` = 5, `Moderately Positive` = 6, `Very Positive` = 7) #It would be best if newspapers were censored so that people would not be able to get hold of destructive and disgusting material.#
#summary(Raw_Study_One_Data$RWA_7) #Mean: 2.5#
Raw_Study_One_Data$RWA_8 = recode(Raw_Study_One_Data$RWA_8, `Very Negative` =  7, `Moderately Negative` = 6, `Slightly Negative` = 5, `Neutral` = 4, `Slightly Positive` = 3, `Moderately Positive` = 2, `Very Positive` = 1) #REVERSED: Many good people challenge the state, criticize the church and ignore "the normal way of living."#
#summary(Raw_Study_One_Data$RWA_8) #Mean: 3.1#
Raw_Study_One_Data$RWA_9 = recode(Raw_Study_One_Data$RWA_9, `Very Negative` =  1, `Moderately Negative` = 2, `Slightly Negative` = 3, `Neutral` = 4, `Slightly Positive` = 5, `Moderately Positive` = 6, `Very Positive` = 7) #Our forefathers ought to be honored more for the way they have built our society, at the same time we ought to put an end to those forces destroying it.#
#summary(Raw_Study_One_Data$RWA_9) #Mean: 4.1#
Raw_Study_One_Data$RWA_10 = recode(Raw_Study_One_Data$RWA_10, `Very Negative` =  7, `Moderately Negative` = 6, `Slightly Negative` = 5, `Neutral` = 4, `Slightly Positive` = 3, `Moderately Positive` = 2, `Very Positive` = 1) #REVERSED: People ought to put less attention to the Bible and religion, instead they ought to develop their own moral standards.#
#summary(Raw_Study_One_Data$RWA_10) #Mean: 3.5#
Raw_Study_One_Data$RWA_11 = recode(Raw_Study_One_Data$RWA_11, `Very Negative` =  1, `Moderately Negative` = 2, `Slightly Negative` = 3, `Neutral` = 4, `Slightly Positive` = 5, `Moderately Positive` = 6, `Very Positive` = 7) #There are many radical, immoral people trying to ruin things; the society ought to stop them.#
#summary(Raw_Study_One_Data$RWA_11) #Mean: 3.8#
Raw_Study_One_Data$RWA_12 = recode(Raw_Study_One_Data$RWA_12, `Very Negative` =  7, `Moderately Negative` = 6, `Slightly Negative` = 5, `Neutral` = 4, `Slightly Positive` = 3, `Moderately Positive` = 2, `Very Positive` = 1) #REVERSED: It is better to accept bad literature than to censor it.#
#summary(Raw_Study_One_Data$RWA_12) #Mean: 2.5#
Raw_Study_One_Data$RWA_13 = recode(Raw_Study_One_Data$RWA_13, `Very Negative` =  1, `Moderately Negative` = 2, `Slightly Negative` = 3, `Neutral` = 4, `Slightly Positive` = 5, `Moderately Positive` = 6, `Very Positive` = 7) #Facts show that we have to be harder against crime and sexual immorality, in order to uphold law and order.#
#summary(Raw_Study_One_Data$RWA_13) #Mean: 3.8#
Raw_Study_One_Data$RWA_14 = recode(Raw_Study_One_Data$RWA_14, `Very Negative` =  7, `Moderately Negative` = 6, `Slightly Negative` = 5, `Neutral` = 4, `Slightly Positive` = 3, `Moderately Positive` = 2, `Very Positive` = 1) #REVERSED: The situation in the society of today would be improved if troublemakers were treated with reason and humanity.#
#summary(Raw_Study_One_Data$RWA_14) #Mean: 3.3#
Raw_Study_One_Data$RWA_15 = recode(Raw_Study_One_Data$RWA_15, `Very Negative` =  1, `Moderately Negative` = 2, `Slightly Negative` = 3, `Neutral` = 4, `Slightly Positive` = 5, `Moderately Positive` = 6, `Very Positive` = 7) #If the society so wants, it is the duty of every true citizen to help eliminate the evil that poisons our country from within.#
#summary(Raw_Study_One_Data$RWA_15) #Mean: 4.0#


###Philosophies of Human Nature Scale###
##High = Trustworthiness##
Raw_Study_One_Data$PHNS_1 = recode(Raw_Study_One_Data$PHNS_1, `Strongly Disagree` = 1, `Moderately Disagree` = 2, `Slightly Disagree` = 3, `Slightly Agree` = 4, `Moderately Agree` = 5, `Strongly Agree` = 6) #Most students will tell the instructor when he has made a mistake in adding up their scores, even if he has given them more points than they deserve#
#summary(Raw_Study_One_Data$PHNS_1) #Mean: 3.3#
Raw_Study_One_Data$PHNS_2 = recode(Raw_Study_One_Data$PHNS_2, `Strongly Disagree` = 1, `Moderately Disagree` = 2, `Slightly Disagree` = 3, `Slightly Agree` = 4, `Moderately Agree` = 5, `Strongly Agree` = 6) #If you give the average person a job and leave him to do it, he will finish it successfully#
#summary(Raw_Study_One_Data$PHNS_2) #Mean: 4.4#
Raw_Study_One_Data$PHNS_3 = recode(Raw_Study_One_Data$PHNS_3, `Strongly Disagree` = 1, `Moderately Disagree` = 2, `Slightly Disagree` = 3, `Slightly Agree` = 4, `Moderately Agree` = 5, `Strongly Agree` = 6) #People usually tell the truth even if they would be better off lying#
#summary(Raw_Study_One_Data$PHNS_3) #Mean: 3.8#
Raw_Study_One_Data$PHNS_4 = recode(Raw_Study_One_Data$PHNS_4, `Strongly Disagree` = 1, `Moderately Disagree` = 2, `Slightly Disagree` = 3, `Slightly Agree` = 4, `Moderately Agree` = 5, `Strongly Agree` = 6) #Most students do not cheat when taking an exam#
#summary(Raw_Study_One_Data$PHNS_4) #Mean: 4.5#
Raw_Study_One_Data$PHNS_5 = recode(Raw_Study_One_Data$PHNS_5, `Strongly Disagree` = 1, `Moderately Disagree` = 2, `Slightly Disagree` = 3, `Slightly Agree` = 4, `Moderately Agree` = 5, `Strongly Agree` = 6) #Most people are basically honest#
#summary(Raw_Study_One_Data$PHNS_5) #Mean: 4.4#
Raw_Study_One_Data$PHNS_6 = recode(Raw_Study_One_Data$PHNS_6, `Strongly Disagree` = 1, `Moderately Disagree` = 2, `Slightly Disagree` = 3, `Slightly Agree` = 4, `Moderately Agree` = 5, `Strongly Agree` = 6) #If you act in good faith with people, almost all of them will reciprocate with fairness toward you#
#summary(Raw_Study_One_Data$PHNS_6) #Mean: 4.4#
Raw_Study_One_Data$PHNS_7 = recode(Raw_Study_One_Data$PHNS_7, `Strongly Disagree` = 1, `Moderately Disagree` = 2, `Slightly Disagree` = 3, `Slightly Agree` = 4, `Moderately Agree` = 5, `Strongly Agree` = 6) #Most people lead clean decent lives#
#summary(Raw_Study_One_Data$PHNS_7) #Mean: 4.5#
Raw_Study_One_Data$PHNS_8 = recode(Raw_Study_One_Data$PHNS_8, `Strongly Disagree` = 7, `Moderately Disagree` = 6, `Slightly Disagree` = 4, `Slightly Agree` = 3, `Moderately Agree` = 2, `Strongly Agree` = 1) #REVERSED: People claim to have ethical standards toward honesty and morality, but few people stick to them when no one will find out about their amorality#
#summary(Raw_Study_One_Data$PHNS_8) #Mean: 3.1#
Raw_Study_One_Data$PHNS_9 = recode(Raw_Study_One_Data$PHNS_9, `Strongly Disagree` = 7, `Moderately Disagree` = 6, `Slightly Disagree` = 4, `Slightly Agree` = 3, `Moderately Agree` = 2, `Strongly Agree` = 1) #REVERSED: If you want people to do a job right, you should explain things to them in great detail and supervise them closely#
#summary(Raw_Study_One_Data$PHNS_9) #Mean: 3.0#
Raw_Study_One_Data$PHNS_10 = recode(Raw_Study_One_Data$PHNS_10, `Strongly Disagree` = 7, `Moderately Disagree` = 6, `Slightly Disagree` = 4, `Slightly Agree` = 3, `Moderately Agree` = 2, `Strongly Agree` = 1) #REVERSED: If most people could get into a movie without paying and be sure they were not seen, they would do it#
#summary(Raw_Study_One_Data$PHNS_10) #Mean: 3.1#
Raw_Study_One_Data$PHNS_11 = recode(Raw_Study_One_Data$PHNS_11, `Strongly Disagree` = 7, `Moderately Disagree` = 6, `Slightly Disagree` = 4, `Slightly Agree` = 3, `Moderately Agree` = 2, `Strongly Agree` = 1) #REVERSED: Most people are not really honest for a desireable reason; they're afraid of getting caught#
#summary(Raw_Study_One_Data$PHNS_11) #Mean: 3.3#
Raw_Study_One_Data$PHNS_12 = recode(Raw_Study_One_Data$PHNS_12, `Strongly Disagree` = 7, `Moderately Disagree` = 6, `Slightly Disagree` = 4, `Slightly Agree` = 3, `Moderately Agree` = 2, `Strongly Agree` = 1) #REVERSED: Most people would tell a lie if they thought they could gain by it#
#summary(Raw_Study_One_Data$PHNS_12) #Mean: 3.2#
Raw_Study_One_Data$PHNS_13 = recode(Raw_Study_One_Data$PHNS_13, `Strongly Disagree` = 7, `Moderately Disagree` = 6, `Slightly Disagree` = 4, `Slightly Agree` = 3, `Moderately Agree` = 2, `Strongly Agree` = 1) #REVERSED: Most people would cheat their income taxes if they had a chance#
#summary(Raw_Study_One_Data$PHNS_13) #Mean: 3.1#
Raw_Study_One_Data$PHNS_14 = recode(Raw_Study_One_Data$PHNS_14, `Strongly Disagree` = 7, `Moderately Disagree` = 6, `Slightly Disagree` = 4, `Slightly Agree` = 3, `Moderately Agree` = 2, `Strongly Agree` = 1) #REVERSED: Nowadays people commit a lot of crimes and sins that no one ever hears about#
#summary(Raw_Study_One_Data$PHNS_14) #Mean: 3.1#


###Life Orientation Test###
##High = Optimistic##
Raw_Study_One_Data$LOT_1 = recode(Raw_Study_One_Data$LOT_1, `Strongly Disagree` = 1, `Moderately Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree Nor Disagree` = 4, `Slightly Agree` = 5, `Moderately Agree` = 6, `Strongly Agree` = 7) #In uncertain times, I usually expect the best#
#summary(Raw_Study_One_Data$LOT_1) #Mean 4.8#
Raw_Study_One_Data$LOT_2 = recode(Raw_Study_One_Data$LOT_2, `Strongly Disagree` = 7, `Moderately Disagree` = 6, `Slightly Disagree` = 5, `Neither Agree Nor Disagree` = 4, `Slightly Agree` = 3, `Moderately Agree` = 2, `Strongly Agree` = 1) #REVERSED: If something can go wrong for me, it will#
#summary(Raw_Study_One_Data$LOT_2) #Mean 4.2#
Raw_Study_One_Data$LOT_3 = recode(Raw_Study_One_Data$LOT_3, `Strongly Disagree` = 1, `Moderately Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree Nor Disagree` = 4, `Slightly Agree` = 5, `Moderately Agree` = 6, `Strongly Agree` = 7) #I'm always optimistic about my future#
#summary(Raw_Study_One_Data$LOT_3) #Mean 5.1#
Raw_Study_One_Data$LOT_4 = recode(Raw_Study_One_Data$LOT_4, `Strongly Disagree` = 7, `Moderately Disagree` = 6, `Slightly Disagree` = 5, `Neither Agree Nor Disagree` = 4, `Slightly Agree` = 3, `Moderately Agree` = 2, `Strongly Agree` = 1) #REVERSED: I hardly ever expect things to go my way#
#summary(Raw_Study_One_Data$LOT_4) #Mean 4.3#
Raw_Study_One_Data$LOT_5 = recode(Raw_Study_One_Data$LOT_5, `Strongly Disagree` = 7, `Moderately Disagree` = 6, `Slightly Disagree` = 5, `Neither Agree Nor Disagree` = 4, `Slightly Agree` = 3, `Moderately Agree` = 2, `Strongly Agree` = 1) #REVERSED: I rarely count on good things happening to me#
#summary(Raw_Study_One_Data$LOT_5) #Mean 4.2#
Raw_Study_One_Data$LOT_6 = recode(Raw_Study_One_Data$LOT_6, `Strongly Disagree` = 1, `Moderately Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree Nor Disagree` = 4, `Slightly Agree` = 5, `Moderately Agree` = 6, `Strongly Agree` = 7) #Overall, I expect more good things to happen to me than bad#
#summary(Raw_Study_One_Data$LOT_6) #Mean 5.2#


###Demographics###
#summary(Raw_Study_One_Data$D1) #Age: Median 33 and Average 42.1#
Raw_Study_One_Data$D2 = as.factor(Raw_Study_One_Data$D2) #Gender#
#table(Raw_Study_One_Data$D2) #52% Female#
Raw_Study_One_Data$D3 = as.factor(Raw_Study_One_Data$D3) #Race#
#table(Raw_Study_One_Data$D3) #73% White#
Raw_Study_One_Data$D4 = recode(Raw_Study_One_Data$D4, `Strongly Liberal` = 1, `Moderately Liberal` = 2, `Slightly Liberal` = 3, `Moderate or Centrist` = 4, `Slightly Conservative` = 5, `Moderately Conservative` = 6, `Strongly Conservative` = 7) #Political Orientation#
#summary(Raw_Study_One_Data$D4) #Average 3.51#
Raw_Study_One_Data$D5 = as.factor(Raw_Study_One_Data$D5) #Poltical Party#
#table(Raw_Study_One_Data$D5)
Raw_Study_One_Data$D6 = as.factor(Raw_Study_One_Data$D6) #Education#
#table(Raw_Study_One_Data$D6) #Mostly Some College or Degree#
Raw_Study_One_Data$D7 = recode(Raw_Study_One_Data$D7, `Less than $20,000` = 1, `Between $20,000 and $40,000` = 2, `Between $40,000 and $65,000` = 3, `Between $65,000 and $105,000` = 4, `Between $105,000 and $160,000` = 5, `More than $160,000` = 6) #Income#
#summary(Raw_Study_One_Data$D7) #Mean: 3.01#
Raw_Study_One_Data$D8 = recode(Raw_Study_One_Data$D8, `Poor/Struggling` = 1, `Working Class/Lower Middle Class` = 2, `Middle Class` = 3, `Upper Middle Class` = 4, `Wealthy/Affluent` = 5) #Subjective SES#
#summary(Raw_Study_One_Data$D8) #Mean: 2.66#
Raw_Study_One_Data$D9 = recode(Raw_Study_One_Data$D9, `Yes` = TRUE, `No` = FALSE) #Is US Citizen#
#table(Raw_Study_One_Data$D9) #Four FALSE to be EXCLUDED#
Raw_Study_One_Data$D10 = as.factor(Raw_Study_One_Data$D10) #Region#
#table(Raw_Study_One_Data$D10) #Disproportionate Number from South#
Raw_Study_One_Data$D11 = as.factor(Raw_Study_One_Data$D11) #Religious Affiliation#
#table(Raw_Study_One_Data$D11)
Raw_Study_One_Data$D12 = recode(Raw_Study_One_Data$D12, `Not At All Religious` = 1, `Not Religious` = 2, `Somewhat Not Religious` = 3, `Neither Very Religious nor Not Religious` = 4, `Somewhat Religious` = 5, `Religious` = 6, `Devoutly Religious` = 7) #Religiousity#
#summary(Raw_Study_One_Data$D12) #Mean: 3.5; Median: 4#
Raw_Study_One_Data$D13 = recode(Raw_Study_One_Data$D13, `Never` = 1, `Seldom` = 2, `Often` = 3, `Very Often` = 4) #Religious Service Attendance#
#summary(Raw_Study_One_Data$D13) #Mean: 1.9#
Raw_Study_One_Data$D14 = recode(Raw_Study_One_Data$D14, `Never` = 1, `Seldom` = 2, `Often` = 3, `Very Often` = 4) #Prayer#
#summary(Raw_Study_One_Data$D14) #Mean: 2.3#
Raw_Study_One_Data$D15 = recode(Raw_Study_One_Data$D15, `Never` = 1, `Seldom` = 2, `Often` = 3, `Very Often` = 4) #Other Church Activities#
#summary(Raw_Study_One_Data$D15) #Mean 1.8#
Raw_Study_One_Data$Society_Random_Order = as.factor(Raw_Study_One_Data$Society_Random_Order) #Random Assignment of Future/Present Variables#
#table(Raw_Study_One_Data$Society_Random_Order) #Roughly Even#
Raw_Study_One_Data$Survey_Random_Order = as.factor(Raw_Study_One_Data$Survey_Random_Order) #Random Order of Other Variables#
#table(Raw_Study_One_Data$Survey_Random_Order) #Roughly Even; Numerous Combinations with Low N's; 118 Levels#


##Verify Structure of dataset##
str(Raw_Study_One_Data, list.len = 140)


##########Descriptive Statistics##############
freq(Raw_Study_One_Data$Manip_Check_1)
freq(Raw_Study_One_Data$Manip_Check_2)
freq(Raw_Study_One_Data$Manip_Check_3)

##Create New Logical Variable to Indicate if Passed Manipulation Check One##
Raw_Study_One_Data = mutate(Raw_Study_One_Data, Manip_Check_1_Passed = if_else(Manip_Check_1 == -13, TRUE, FALSE))
#freq(Raw_Study_One_Data$Manip_Check_1_Passed) #470 (92.2%) Passed#
#cor(Raw_Study_One_Data$Manip_Check_1_Passed, Raw_Study_One_Data$Duration_Secs)

##Create New Logical Variable to Indicate if Passed Manipulation Check Two##
Raw_Study_One_Data = mutate(Raw_Study_One_Data, Manip_Check_2_Passed = if_else(Manip_Check_2 == 57, TRUE, FALSE))
#freq(Raw_Study_One_Data$Manip_Check_2_Passed) #428 (83.9%) Passed#
#cor(Raw_Study_One_Data$Manip_Check_2_Passed, Raw_Study_One_Data$Duration_Secs)

##Create New Logical Variable to Indicate if Passed Manipulation Check Three##
Raw_Study_One_Data = mutate(Raw_Study_One_Data, Manip_Check_3_Passed = if_else(Manip_Check_3 == -78, TRUE, FALSE))
#freq(Raw_Study_One_Data$Manip_Check_3_Passed) #428 (83.9%) Passed#
#cor(Raw_Study_One_Data$Manip_Check_3_Passed, Raw_Study_One_Data$Duration_Secs)

##Passed all Three Attention Checks##
Raw_Study_One_Data = mutate(Raw_Study_One_Data, PASSED_ALL_ATTN = if_else(Manip_Check_3_Passed == TRUE & Manip_Check_2_Passed == TRUE & Manip_Check_1_Passed == TRUE, TRUE, FALSE))
freq(Raw_Study_One_Data$PASSED_ALL_ATTN) #393 (77.1%) Passed all Three#


#####Create Subset for all participants who passed ATTN Checks#####
##Use dplyr::filter##
Passed_Study_One_Data = dplyr::filter(Imputed_Raw_Data_Study_One, PASSED_ALL_ATTN == 1)
summary(Passed_Study_One_Data$Duration_Secs/60)


#####Chronbach's Alpha Reliability Assessment####
##Use dplyr::select to subset of scale items##
Passed_FWB_Items = dplyr::select(Passed_Study_One_Data, FWB_1, FWB_2, FWB_3, FWB_4, FWB_5, FWB_6, FWB_7, FWB_8)
Passed_AL_Items = dplyr::select(Passed_Study_One_Data, AL_1, AL_2, AL_3, AL_4, AL_5, AL_6, AL_7, AL_8, AL_9, AL_10, AL_11, AL_12, AL_13, AL_14, AL_15, AL_16, AL_17, AL_18, AL_19, AL_20)
Passed_RWA = dplyr::select(Passed_Study_One_Data, RWA_1, RWA_2, RWA_3, RWA_4, RWA_5, RWA_6, RWA_7, RWA_8, RWA_9, RWA_10, RWA_11, RWA_12, RWA_13, RWA_14, RWA_15)
Passed_PHNS = dplyr::select(Passed_Study_One_Data, PHNS_1, PHNS_2, PHNS_3, PHNS_4, PHNS_5, PHNS_6, PHNS_7, PHNS_8, PHNS_9, PHNS_10, PHNS_11, PHNS_12, PHNS_13, PHNS_14)
Passed_LOT = dplyr::select(Passed_Study_One_Data, LOT_1, LOT_2, LOT_3, LOT_4, LOT_5, LOT_6)
Passed_Present <- dplyr::select(Passed_Study_One_Data, Present_1, Present_2, Present_3, Present_4, Present_5, Present_6)
#correlate(Passed_Present)
Passed_Best_Future <- dplyr::select(Passed_Study_One_Data, Best_Future_1, Best_Future_2, Best_Future_3, Best_Future_4, Best_Future_5, Best_Future_6)
#correlate(Passed_Best_Future)
Passed_Worst_Future <- dplyr::select(Passed_Study_One_Data, Worst_Future_1, Worst_Future_2, Worst_Future_3, Worst_Future_4, Worst_Future_5, Worst_Future_6)
#correlate(Passed_Worst_Future)



##psych::alpha to compute Cronbach's Alpha and GUttman's Lambda 6##
psych::alpha(Passed_FWB_Items) #FWB 0.9 Alpha#
psych::alpha(Passed_AL_Items) #Libertarianism 0.91 Alpha#
psych::alpha(Passed_RWA) #RWA 0.91 Alpha#
psych::alpha(Passed_PHNS) #PHNS 0.85 Alpha#
psych::alpha(Passed_LOT) #PHNS 0.90 Alpha#
#psych::alpha(Passed_Present)
#psych::alpha(Passed_Best_Future)
#psych::alpha(Passed_Worst_Future)


#####Compute Scale Means#####
#Use dplyr::mutate#
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, FWB = (FWB_1 + FWB_2 + FWB_3 + FWB_4 + FWB_5 + FWB_6 + FWB_7 + FWB_8)/8)
summary(Passed_Study_One_Data$FWB) #Mean: 5.42#
#table(is.na(Passed_Study_One_Data$FWB)) #No Missing#

##Totalitarianism-Liberalism##
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, AL = (AL_1 + AL_2 + AL_3 + AL_4 + AL_5 + AL_6 + AL_7 + AL_8 + AL_9 + AL_10 + AL_11 + AL_12 + AL_13 + AL_14 + AL_15 + AL_16 + AL_17 + AL_18 + AL_19 + AL_20)/20)
summary(Passed_Study_One_Data$AL) #Mean: 4.53#

##Right Wing Authoritarianism##
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, RWA = (RWA_1 + RWA_2 + RWA_3 + RWA_4 + RWA_5 + RWA_6 + RWA_7 + RWA_8 + RWA_9 + RWA_10 + RWA_11 + RWA_12 + RWA_13 + RWA_14 + RWA_15)/15)
summary(Passed_Study_One_Data$RWA) #Mean: 3.29#

##Philosophies of Human Nature Trusworthiness##
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, PHNS = (PHNS_1 + PHNS_2 + PHNS_3 + PHNS_4 + PHNS_5 + PHNS_6 + PHNS_7 + PHNS_8 + PHNS_9 + PHNS_10 + PHNS_11 + PHNS_12 + PHNS_13 + PHNS_14)/14)
summary(Passed_Study_One_Data$PHNS)

##Life Orientation Test##
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, LOT = (LOT_1 + LOT_2 + LOT_3 + LOT_4 + LOT_5 + LOT_6)/6)
summary(Passed_Study_One_Data$LOT)


###############################################################Correlations##################################################################################################################################
Descriptive_Corr = Passed_Study_One_Data %>%
  dplyr::select(FWB, D4, RWA, AL, PHNS, LOT, D12, D13, D14, D15, Duration_Secs)

#Descriptive_Matrix = rcorr(as.matrix(Descriptive_Corr))


cor.test(Passed_Study_One_Data$FWB, Passed_Study_One_Data$RWA) #0.4#
cor.test(Passed_Study_One_Data$FWB, Passed_Study_One_Data$D4) #0.39#
cor.test(Passed_Study_One_Data$FWB, Passed_Study_One_Data$AL) #0.33#
cor.test(Passed_Study_One_Data$FWB, Passed_Study_One_Data$LOT) #0.33#
cor.test(Passed_Study_One_Data$FWB, Passed_Study_One_Data$PHNS) #NS#

cor.test(Passed_Study_One_Data$RWA, Passed_Study_One_Data$AL) #0.35#
cor.test(Passed_Study_One_Data$RWA, Passed_Study_One_Data$D12) #0.46#
cor.test(Passed_Study_One_Data$RWA, Passed_Study_One_Data$LOT) #NS#
cor.test(Passed_Study_One_Data$RWA, Passed_Study_One_Data$PHNS) #-0.22#


cor.test(Passed_Study_One_Data$D4, Passed_Study_One_Data$LOT) #0.14#
cor.test(Passed_Study_One_Data$D4, Passed_Study_One_Data$PHNS) #NS#
cor.test(Passed_Study_One_Data$D4, Passed_Study_One_Data$D12) #0.35#
cor.test(Passed_Study_One_Data$D4, Passed_Study_One_Data$AL) #0.51#


##Verify Structure##
str(Passed_Study_One_Data, list.len = 150)

########Exploratory Factor Analysis##############
#use fa() from package 'psych'#
Present_Eigen_Scree <- psych::scree(as.matrix(Passed_Present), main = "Present Scree Plot") #One Solution Indicated#
Best_Future_Eigen_Scree <- psych::scree(as.matrix(Passed_Best_Future), main = "Best Future Scree Plot") #One Solution Indicated#
Worst_Future_Eigen_Scree <- psych::scree(as.matrix(Passed_Worst_Future), main = "Worst Future Scree Plot") #One Solution Indicated#

###Very Simple Structure###
#Present_VSS = vss(as.matrix(Passed_Present), n=3, rotate = "Promax", fm = "ml")#Very simple structure#
#Present_VSS #Contains Fit Statistics#
#Best_Future_VSS = vss(as.matrix(Passed_Best_Future), n=3, rotate = "Promax", fm = "ml") #Very simple structure#
#Best_Future_VSS #Contains Fit Statistics#
#Worst_Future_VSS = vss(as.matrix(Passed_Worst_Future), n=3, rotate = "Promax", fm = "ml") #Very simple structure#
#Worst_Future_VSS #Contains Fit Statistics#

###Extract One Factor Solutions###
Present_Factor_Analysis <- fa(Passed_Present, nfactors = 1, fm = "ml", alpha = .08, rotate = "Promax")
Best_Future_Factor_Analysis <- fa(Passed_Best_Future, nfactors = 1, fm = "ml", alpha = .08, rotate = "Promax")
Worst_Future_Factor_Analysis <- fa(Passed_Worst_Future, nfactors = 1, fm = "ml", alpha = .08, rotate = "Promax")

#####Compute Composite Variables for Present and Futures######
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, Present = (Present_1 + Present_2 + Present_3 + Present_4 + Present_5 + Present_6)/6)
#summary(Passed_Study_One_Data$Present) #Mean: 16.80#
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, Best_Future = (Best_Future_1 + Best_Future_2 + Best_Future_3 + Best_Future_4 + Best_Future_5 + Best_Future_6)/6)
#summary(Passed_Study_One_Data$Best_Future) #Mean: 65.36#
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, Worst_Future = (Worst_Future_1 + Worst_Future_2 + Worst_Future_3 + Worst_Future_4 + Worst_Future_5 + Worst_Future_6)/6)
#summary(Passed_Study_One_Data$Worst_Future) #Mean: -55.96#

##TO BE REMOVED: Number of People where the Worst Future is Better than the Best##
freq(Passed_Study_One_Data$Worst_Future > Passed_Study_One_Data$Best_Future)
##Number of People where the present is higher than the best possible future##
freq(Passed_Study_One_Data$Present > Passed_Study_One_Data$Best_Future)

##Drop Those lower whose best future is lower than the worst##
Passed_Study_One_Data = dplyr::filter(Passed_Study_One_Data, Worst_Future < Best_Future)


###Compute Difference Variables###
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, Positive_Potential = Best_Future - Present) #Positive Potential: Best Future Minus Present#
#summary(Passed_Study_One_Data$Positive_Potential) #Mean: 51.97#
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, Negative_Potential = Worst_Future - Present) #Negative Potential: Worst Future Minus Present#
#summary(Passed_Study_One_Data$Negative_Potential) #Mean: -78.53#
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, Absolute_Potential = Best_Future - Worst_Future) #Absolute Potential: Best Future - Worst#
#summary(Passed_Study_One_Data$Absolute_Potential) #Mean: 130.5#

#Number of people with more Positive than absolue potential#
freq(Passed_Study_One_Data$Positive_Potential > Passed_Study_One_Data$Absolute_Potential) #14#
##Number of people with more Negative than absolue potential#
freq(Passed_Study_One_Data$Negative_Potential < Passed_Study_One_Data$Absolute_Potential) #27#

#######################Correlations###################################
Descriptive_Corr_2 = Passed_Study_One_Data %>%
  dplyr::select(Present, Best_Future, Worst_Future, Positive_Potential, Negative_Potential, Absolute_Potential, FWB, D4, RWA, AL, PHNS, LOT, D1, D12, D13, D14, D15, Duration_Secs)

#Descriptive_Matrix2 = rcorr(as.matrix(Descriptive_Corr_2))


###Free Will and Time Estimates###
cor.test(Passed_Study_One_Data$Present, Passed_Study_One_Data$FWB)
cor.test(Passed_Study_One_Data$Best_Future, Passed_Study_One_Data$FWB)
cor.test(Passed_Study_One_Data$Worst_Future, Passed_Study_One_Data$FWB)

cor.test(Passed_Study_One_Data$Positive_Potential, Passed_Study_One_Data$FWB)
cor.test(Passed_Study_One_Data$Negative_Potential, Passed_Study_One_Data$FWB)
cor.test(Passed_Study_One_Data$Absolute_Potential, Passed_Study_One_Data$FWB)


###Plots FWB and Time Estimates with Political Orientation###

GG_FWB_PO_POS_PO = ggplot(Passed_Study_One_Data, aes(x=FWB, y=Positive_Potential)) + geom_jitter(aes(col = D4), size = 3) + scale_color_continuous(low = "blue", high = "red") + 
  ggtitle("Free Will, Political Orientation, and Positive Potential") + xlab("Free Will Beliefs") + ylab("Positive Potential")

GG_FWB_PO_NEG_PO = ggplot(Passed_Study_One_Data, aes(x=FWB, y=Negative_Potential)) + geom_jitter(aes(col = D4), size = 3) + scale_color_continuous(low = "blue", high = "red") + 
  ggtitle("Free Will, Political Orientation, and Negative Potential") + xlab("Free Will Beliefs") + ylab("Negative Potential")

GG_FWB_PO_PRES = ggplot(Passed_Study_One_Data, aes(x=FWB, y=Present)) + geom_jitter(aes(col = D4), size = 3) + scale_color_continuous(low = "blue", high = "red") + 
  ggtitle("Free Will, Political Orientation, and Present") + xlab("Free Will Beliefs") + ylab("Present")

GG_FWB_PO_BEST = ggplot(Passed_Study_One_Data, aes(x=FWB, y=Best_Future)) + geom_jitter(aes(col = D4), size = 3) + scale_color_continuous(low = "blue", high = "red") + 
  ggtitle("Free Will, Political Orientation, and Best Future") + xlab("Free Will Beliefs") + ylab("Best Future")

GG_FWB_PO_WORST = ggplot(Passed_Study_One_Data, aes(x=FWB, y=Worst_Future)) + geom_jitter(aes(col = D4), size = 3) + scale_color_continuous(low = "blue", high = "red") + 
  ggtitle("Free Will, Political Orientation, and Worst Future") + xlab("Free Will Beliefs") + ylab("Worst Future")


###Political Orientation and Time Estimates###
cor.test(Passed_Study_One_Data$Present, Passed_Study_One_Data$D4) #0.38#
cor.test(Passed_Study_One_Data$Best_Future, Passed_Study_One_Data$D4) #N.S.#
cor.test(Passed_Study_One_Data$Worst_Future, Passed_Study_One_Data$D4) #N.S.#

cor.test(Passed_Study_One_Data$Positive_Potential, Passed_Study_One_Data$D4) #-0.33#
cor.test(Passed_Study_One_Data$Negative_Potential, Passed_Study_One_Data$D4) #-0.28#
cor.test(Passed_Study_One_Data$Absolute_Potential, Passed_Study_One_Data$D4) #N.S.#

###Life Orientation Test and Time Estimates###
cor.test(Passed_Study_One_Data$Present, Passed_Study_One_Data$LOT)
cor.test(Passed_Study_One_Data$Best_Future, Passed_Study_One_Data$LOT)
cor.test(Passed_Study_One_Data$Worst_Future, Passed_Study_One_Data$LOT)

cor.test(Passed_Study_One_Data$Positive_Potential, Passed_Study_One_Data$LOT)
cor.test(Passed_Study_One_Data$Negative_Potential, Passed_Study_One_Data$LOT)
cor.test(Passed_Study_One_Data$Absolute_Potential, Passed_Study_One_Data$LOT)

###Libertarianism and Time Estimates###
cor.test(Passed_Study_One_Data$Present, Passed_Study_One_Data$AL)
cor.test(Passed_Study_One_Data$Best_Future, Passed_Study_One_Data$AL)
cor.test(Passed_Study_One_Data$Worst_Future, Passed_Study_One_Data$AL)

cor.test(Passed_Study_One_Data$Positive_Potential, Passed_Study_One_Data$AL)
cor.test(Passed_Study_One_Data$Negative_Potential, Passed_Study_One_Data$AL)
cor.test(Passed_Study_One_Data$Absolute_Potential, Passed_Study_One_Data$AL)

###Trustworthiness and Time Estimates###
cor.test(Passed_Study_One_Data$Present, Passed_Study_One_Data$PHNS)
cor.test(Passed_Study_One_Data$Best_Future, Passed_Study_One_Data$PHNS)
cor.test(Passed_Study_One_Data$Worst_Future, Passed_Study_One_Data$PHNS)

cor.test(Passed_Study_One_Data$Positive_Potential, Passed_Study_One_Data$PHNS)
cor.test(Passed_Study_One_Data$Negative_Potential, Passed_Study_One_Data$PHNS)
cor.test(Passed_Study_One_Data$Absolute_Potential, Passed_Study_One_Data$PHNS)

###RWA and Time Estimates###
cor.test(Passed_Study_One_Data$Present, Passed_Study_One_Data$RWA)
cor.test(Passed_Study_One_Data$Best_Future, Passed_Study_One_Data$RWA)
cor.test(Passed_Study_One_Data$Worst_Future, Passed_Study_One_Data$RWA)

cor.test(Passed_Study_One_Data$Positive_Potential, Passed_Study_One_Data$RWA)
cor.test(Passed_Study_One_Data$Negative_Potential, Passed_Study_One_Data$RWA)
cor.test(Passed_Study_One_Data$Absolute_Potential, Passed_Study_One_Data$RWA)

###Religiousity and Time Estimates###
cor.test(Passed_Study_One_Data$Present, Passed_Study_One_Data$D12)
cor.test(Passed_Study_One_Data$Best_Future, Passed_Study_One_Data$D12)
cor.test(Passed_Study_One_Data$Worst_Future, Passed_Study_One_Data$D12)

cor.test(Passed_Study_One_Data$Positive_Potential, Passed_Study_One_Data$D12)
cor.test(Passed_Study_One_Data$Negative_Potential, Passed_Study_One_Data$D12)
cor.test(Passed_Study_One_Data$Absolute_Potential, Passed_Study_One_Data$D12)


Ratio_Positive_Negative_Potential = Passed_Study_One_Data$Positive_Potential/Passed_Study_One_Data$Negative_Potential
summary(Ratio_Positive_Negative_Potential)

Standardized_Positive_Potential = Passed_Study_One_Data$Positive_Potential/Passed_Study_One_Data$Absolute_Potential
summary(Standardized_Positive_Potential)
table(abs(Standardized_Positive_Potential)>1)

Standardized_Negative_Potential = Passed_Study_One_Data$Negative_Potential/Passed_Study_One_Data$Absolute_Potential
summary(Standardized_Negative_Potential)
table(abs(Standardized_Negative_Potential)>1)

###Descriptive Means by Categorical Variables###

Mean_by_Gender = Passed_Study_One_Data %>%
  group_by(D2) %>%
  dplyr::filter(D2 == "Female" | D2 == "Male") %>%
  summarise(Mean_Present = mean(Present), Mean_BF = mean(Best_Future), Mean_WF = mean(Worst_Future), Mean_POS = mean(Positive_Potential), Mean_NEG = mean(Negative_Potential), Mean_PO = mean(D4), Mean_FWB = mean(FWB), Mean_AL = mean(AL), Mean_RWA = mean(RWA), Mean_LOT = mean(LOT), Mean_PHNS = mean(PHNS), N = n())


Mean_by_Party = Passed_Study_One_Data %>%
  group_by(D5) %>%
  #dplyr::filter(D2 == "Female" | D2 == "Male") %>%
  summarise(Mean_Present = mean(Present), Mean_BF = mean(Best_Future), Mean_WF = mean(Worst_Future), Mean_POS = mean(Positive_Potential), Mean_NEG = mean(Negative_Potential), Mean_PO = mean(D4), Mean_FWB = mean(FWB), Mean_AL = mean(AL), Mean_RWA = mean(RWA), Mean_LOT = mean(LOT), Mean_PHNS = mean(PHNS), N = n())


Mean_by_Race = Passed_Study_One_Data %>%
  group_by(D3) %>%
  dplyr::filter(n() > 5) %>%
  summarise(Mean_Present = mean(Present), Mean_BF = mean(Best_Future), Mean_WF = mean(Worst_Future), Mean_POS = mean(Positive_Potential), Mean_NEG = mean(Negative_Potential), Mean_PO = mean(D4), Mean_FWB = mean(FWB), Mean_AL = mean(AL), Mean_RWA = mean(RWA), Mean_LOT = mean(LOT), Mean_PHNS = mean(PHNS), N = n())


Mean_by_Education = Passed_Study_One_Data %>%
  group_by(D6) %>%
  dplyr::filter(n() > 5) %>%
  summarise(Mean_Present = mean(Present), Mean_BF = mean(Best_Future), Mean_WF = mean(Worst_Future), Mean_POS = mean(Positive_Potential), Mean_NEG = mean(Negative_Potential), Mean_PO = mean(D4), Mean_FWB = mean(FWB), Mean_AL = mean(AL), Mean_RWA = mean(RWA), Mean_LOT = mean(LOT), Mean_PHNS = mean(PHNS), N = n())


Mean_by_Denomination = Passed_Study_One_Data %>%
  group_by(D11) %>%
  dplyr::filter(n() > 5) %>%
  summarise(Mean_Present = mean(Present), Mean_BF = mean(Best_Future), Mean_WF = mean(Worst_Future), Mean_POS = mean(Positive_Potential), Mean_NEG = mean(Negative_Potential), Mean_PO = mean(D4), Mean_FWB = mean(FWB), Mean_AL = mean(AL), Mean_RWA = mean(RWA), Mean_LOT = mean(LOT), Mean_PHNS = mean(PHNS), N = n())


#########################################################Test of Mediational Relationship#########################################################################
#?psych::mediate

###FWB Mediated by PO###
Mediation_FWB_PO_Potential = psych::mediate(y = "Positive_Potential", x = "FWB", m = "D4", data = Passed_Study_One_Data, std = TRUE)
Mediation_FWB_PO_Potential

###PO Mediated by FWB###
Alternative_Mediation_PO_POS_Potential = psych::mediate(y = "Positive_Potential", x = "D4", m = "FWB", data = Passed_Study_One_Data, std = TRUE)
Alternative_Mediation_PO_POS_Potential

###FWB Mediated by AL###
Alternative_Mediation_AL_POS_Potential = psych::mediate(y = "Positive_Potential", x = "FWB", m = "AL", data = Passed_Study_One_Data, std = TRUE)
Alternative_Mediation_AL_POS_Potential 

###FWB Mediated by AL and PO###
Alternative_Mediation_AL_PO_POS_Potential = psych::mediate(y = "Positive_Potential", x = "FWB", m = c("AL", "D4"), data = Passed_Study_One_Data, std = TRUE)
Alternative_Mediation_AL_PO_POS_Potential #Only PO Significant#

###FWB Mediated by RWA and PO###
Alternative_Mediation_RWA_PO_POS_Potential = psych::mediate(y = "Positive_Potential", x = "FWB", m = c("RWA", "D4"), data = Passed_Study_One_Data, std = TRUE)
Alternative_Mediation_RWA_PO_POS_Potential #Both PO and RWA Significant#

###RWA Mediated by FWB and PO###
Alternative_Mediation_RWA_PO_POS_Potential = psych::mediate(y = "Positive_Potential", x = "RWA", m = c("D4", "FWB"), data = Passed_Study_One_Data, std = TRUE)
Alternative_Mediation_RWA_PO_POS_Potential

###FWB Mediated by LOT and PO###
Alternative_Mediation_LOT_PO_POS_Potential = psych::mediate(y = "Positive_Potential", x = "FWB", m = c("LOT", "D4"), data = Passed_Study_One_Data, std = TRUE)
Alternative_Mediation_LOT_PO_POS_Potential

###FWB Mediated by PO###
Mediation_FWB_PHNS_PO_Potential = psych::mediate(y = "Positive_Potential", x = "FWB", m = "D4", mod = "PHNS", data = Passed_Study_One_Data, std = TRUE)
Mediation_FWB_PHNS_PO_Potential


#######Best_Future############

###Future: FWB Mediated by RWA and PO###
Alternative_Mediation_RWA_PO_Best_Future = psych::mediate(y = "Best_Future", x = "FWB", m = c("D4", "RWA"), data = Passed_Study_One_Data, std = TRUE)
Alternative_Mediation_RWA_PO_Best_Future

#Alternative_Mediation_RWA_PO_AL_PHNS_Best_Future = psych::mediate(y = "Best_Future", x = "FWB", m = c("D4", "RWA", "PHNS"), data = Passed_Study_One_Data, std = TRUE)
#Alternative_Mediation_RWA_PO_AL_PHNS_Best_Future

#Alternative_Mediation_PHNS_Best_Future = psych::mediate(y = "Best_Future", x = "FWB", m = "PHNS", data = Passed_Study_One_Data, std = TRUE)
#Alternative_Mediation_PHNS_Best_Future #N.S#

Alternative_Mediation_LOT_Best_Future = psych::mediate(y = "Best_Future", x = "FWB", m = c("LOT", "D12"), data = Passed_Study_One_Data, std = TRUE)
Alternative_Mediation_LOT_Best_Future 

#Alternative_Mediation_PO_Best_Future = psych::mediate(y = "Best_Future", x = "FWB", m = "D4", data = Passed_Study_One_Data, std = TRUE)
#Alternative_Mediation_PO_Best_Future #Not Significant#


###FWB and Present Mediated by PO###
Mediation_FWB_Present = psych::mediate(y = "Present", x = "FWB", m = "D4", data = Passed_Study_One_Data, std = TRUE)
Mediation_FWB_Present
