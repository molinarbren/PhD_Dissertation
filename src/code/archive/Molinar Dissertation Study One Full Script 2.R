###Install.Packages to Install Important Packages###
#install.packages("dplyr") #For Data manipulation#
#install.packages("plyr") #For Data manipulation#
#install.packages("psych") #For Psychology Based Statistics and Analyses#
#install.packages("tidyr") #For Data Cleaning#
#install.packages("readxl") #To Read in Excel Documents#
#install.packages("rematch") #For readxl#
#install.packages("lsr") #For table function#
#install.packages("missForest") #Uses Nonparametric Random Forest modeling to perform missing data imputation#
#install.packages("nFactors") #Scree Test and Factor Dimension Extraction#
#install.packages("Hmisc") #For Correlation Matrix with P values#
#install.packages("ggplot2") #For Visualization of Data#
#install.packages("RColorBrewer") #Color Pallets for ggplot2
#install.packages("mediation") #To Conduct Mediational Analyses#
##install.packages("questionr") #questionr::frequency Function#
#install.packages("DMwR") #KNN Imputation
#install.packages("imputeMissings")
#install.packages("corrplot")
#install.packages("cluster") #For Partitiona Around Medoid#
#install.packages("fpc") #For Factor Extraction pamk#
#install.packages("factoextra")
#install.packages("moments") #For Skewness and Kurtosis#
#install.packages("rcompanion") #For Normality Transformations#
#install.packages("forecast") #To test accuracy#
#install.packages("xlsx")
#install.packages("heplots") #Eta-Squared#
#install.packages("gridExtra") #Combine GGplots#
#install.packages("jtools") #Simple slopes#


###Load Necessary Packages into Workspace###
#library(Hmisc)
library(dplyr)
library(psych)
library(tidyr)
library(readxl)
library(rematch)
library(lsr)
library(missForest)
library(nFactors)
library(ggplot2)
library(RColorBrewer) 
library(mediation)
library(questionr)
library(DMwR)
library(imputeMissings)
library(corrplot)
library(cluster)
library(fpc)
library(factoextra)
library(moments)
library(rcompanion)
library(forecast)
#library(xlsx)
#library(heplots)
library(gridExtra)
library(jtools)


corstars <- function(x){ 
  #install.packages('Hmisc')
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.##
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal##
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars## 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle##
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)##
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

spearman_corstars <- function(x){ 
  #install.packages('Hmisc')
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x, type = "spearman")$r 
  p <- rcorr(x, type = "spearman")$P 
  
  ## define notions for significance levels; spacing is important.##
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal##
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars## 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle##
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)##
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}


#####Load the Data into the work Environment#####
##Create List of variable Names##
Var_Col = c("Start_Time", "End_Time", "IP_Address", "Progress", "Duration_Secs", "Finished", "Comp_Date", "ID", "IP", "Lang", "Consent", "Present_1", "Present_2", "Present_3", "Present_4", "Present_5", "Present_6", "Manip_Check_1", "Present_First_Click",
"Present_Last_Click", "Present_Page_Submit", "Present_Click_Count", "Best_Future_1", "Best_Future_2", "Best_Future_3", "Best_Future_4", "Best_Future_5", "Best_Future_6", "Best_Future_Likelihood", "Manip_Check_2", "Best_Future_First_Click", "Best_Future_Last_Click",
"Best_Future_Page_Submit", "Best_Future_Click_Count", "Worst_Future_1", "Worst_Future_2", "Worst_Future_3", "Worst_Future_4", "Worst_Future_5", "Worst_Future_6", "Worst_Future_Likelihood", "Manip_Check_3", "Worst_Future_First_Click", "Worst_Future_Last_Click", "Worst_Future_Page_Submit",
"Worst_Future_Click_Count", "FWB_1", "FWB_2", "FWB_3", "FWB_4", "FWB_5", "FWB_6", "FWB_7", "FWB_8", "AL_1", "AL_2", "AL_3", "AL_4", "AL_5", "AL_6", "AL_7", "AL_8", "AL_9", "AL_10", "AL_11", "AL_12", "AL_13", "AL_14", "AL_15", "AL_16", "AL_17", "AL_18", "AL_19", "AL_20", "RWA_1", "RWA_2", "RWA_3", "RWA_4", "RWA_5", "RWA_6",
"RWA_7", "RWA_8", "RWA_9", "RWA_10", "RWA_11", "RWA_12", "RWA_13", "RWA_14", "RWA_15", "PHNS_1", "PHNS_2", "PHNS_3", "PHNS_4", "PHNS_5", "PHNS_6", "PHNS_7", "PHNS_8", "PHNS_9", "PHNS_10", "PHNS_11", "PHNS_12", "PHNS_13", "PHNS_14", "LOT_1", "LOT_2", "LOT_3", "LOT_4", "LOT_5", "LOT_6", "D1",
"D2", "D3", "D3_TXT", "D4", "D5", "D5_TXT", "D6", "D6_TXT", "D7", "D8", "D9", "D10", "D10_TXT", "D11", "D11_TXT", "D12", "D13", "D14", "D15", "Comments", "Code", "Society_Random_Order", "Survey_Random_Order")

###Load XLSX File with Data from Dropbox###
#?read_excel #Use this function to read in Excel Document
Raw_Study_One_Data <- read_excel("C:/Users/bmoli/Documents/Dissertation/Dissertation Study 1 (Full) w Randomization.xlsx", range = "A4:EC513", sheet = "Dissertation Study w Part 2", col_names = Var_Col)


##Verify Structure of the Data##
str(Raw_Study_One_Data, list.len = 140)
summary(Raw_Study_One_Data$Duration_Secs/60) ##Average Time to Complete Study in Minutes##


#################################################################################################################################################################
#######################################################Recode Data Variables for Cleanining######################################################################
#################################################################################################################################################################

######################################################Free Will Beliefs Scale#################################################################
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


############################Totalitarianism/Authoritarianims and Libertarianims Scale#########################################################
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


#################################################################Right Wing Authoritarianism###################################################################
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


###########################################################Philosophies of Human Nature Scale######################################################################################
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


#################################################################Life Orientation Test#########################################################################
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


###################################################################Demographics##############################################################################################
summary(Raw_Study_One_Data$D1) #Age: Median 33 and Average 42.1#

Raw_Study_One_Data$D2 = as.factor(Raw_Study_One_Data$D2) #Gender#
#questionr::questionr::freq(Raw_Study_One_Data$D2, sort = "dec") #54% Female#

Raw_Study_One_Data$D3 = as.factor(Raw_Study_One_Data$D3) #Race#
#questionr::questionr::freq(Raw_Study_One_Data$D3, sort = "dec") #73% White#

Raw_Study_One_Data$D4 = recode(Raw_Study_One_Data$D4, `Strongly Liberal` = 1, `Moderately Liberal` = 2, `Slightly Liberal` = 3, `Moderate or Centrist` = 4, `Slightly Conservative` = 5, `Moderately Conservative` = 6, `Strongly Conservative` = 7) #Political Orientation#
#summary(Raw_Study_One_Data$D4) #Average 3.51#

Raw_Study_One_Data$D5 = as.factor(Raw_Study_One_Data$D5) #Poltical Party#
#questionr::freq(Raw_Study_One_Data$D5, sort = "dec") #43% Democrat, 29% Rep, and 10% Libertarian#

Raw_Study_One_Data$D6 = recode(Raw_Study_One_Data$D6, `Less than High School` = 1, `High School or GED` = 2, `Some College or Two Year Degree` = 3, `Bachelor's or Four Year Degree` = 4, `Graduate or Professional Degree` = 5, `Other (Please Specify)` = NULL) #Education#
#summary(Raw_Study_One_Data$D6) #Mostly Some College or Degree#

Raw_Study_One_Data$D7 = recode(Raw_Study_One_Data$D7, `Less than $20,000` = 1, `Between $20,000 and $40,000` = 2, `Between $40,000 and $65,000` = 3, `Between $65,000 and $105,000` = 4, `Between $105,000 and $160,000` = 5, `More than $160,000` = 6) #Income#
#summary(Raw_Study_One_Data$D7) #Mean: 3.01#

Raw_Study_One_Data$D8 = recode(Raw_Study_One_Data$D8, `Poor/Struggling` = 1, `Working Class/Lower Middle Class` = 2, `Middle Class` = 3, `Upper Middle Class` = 4, `Wealthy/Affluent` = 5) #Subjective SES#
#summary(Raw_Study_One_Data$D8) #Mean: 2.66#

Raw_Study_One_Data$D9 = recode(Raw_Study_One_Data$D9, `Yes` = TRUE, `No` = FALSE) #Is US Citizen#
#questionr::freq(Raw_Study_One_Data$D9) #Five FALSE to be EXCLUDED#

Raw_Study_One_Data$D10 = as.factor(Raw_Study_One_Data$D10) #Region#
#questionr::freq(Raw_Study_One_Data$D10) #Disproportionate Number from South#

Raw_Study_One_Data$D11 = as.factor(Raw_Study_One_Data$D11) #Religious Affiliation#
#questionr::freq(Raw_Study_One_Data$D11)

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


###Verify Structure of dataset###
str(Raw_Study_One_Data, list.len = 140)


##########Manipulation Check Statistics##############
questionr::freq(Raw_Study_One_Data$Manip_Check_1) #92% Passed#
questionr::freq(Raw_Study_One_Data$Manip_Check_2) #93% Passed#
questionr::freq(Raw_Study_One_Data$Manip_Check_3) #84% Passed#


##Create New Logical Variable to Indicate if Passed Manipulation Check One##
Raw_Study_One_Data = mutate(Raw_Study_One_Data, Manip_Check_1_Passed = if_else(Manip_Check_1 == -13, 1, 0))
#questionr::freq(Raw_Study_One_Data$Manip_Check_1_Passed) #470 Passed#
#cor(Raw_Study_One_Data$Manip_Check_1_Passed, Raw_Study_One_Data$Duration_Secs)


##Create New Logical Variable to Indicate if Passed Manipulation Check Two##
Raw_Study_One_Data = mutate(Raw_Study_One_Data, Manip_Check_2_Passed = if_else(Manip_Check_2 == 57, 1, 0))
#questionr::freq(Raw_Study_One_Data$Manip_Check_2_Passed) #475 Passed#
#cor(Raw_Study_One_Data$Manip_Check_2_Passed, Raw_Study_One_Data$Duration_Secs)


##Create New Logical Variable to Indicate if Passed Manipulation Check Three##
Raw_Study_One_Data = mutate(Raw_Study_One_Data, Manip_Check_3_Passed = if_else(Manip_Check_3 == -78, 1, 0))
#questionr::freq(Raw_Study_One_Data$Manip_Check_3_Passed) #428 Passed#
#cor(Raw_Study_One_Data$Manip_Check_3_Passed, Raw_Study_One_Data$Duration_Secs)


##Passed all Three Attention Checks##
Raw_Study_One_Data = mutate(Raw_Study_One_Data, PASSED_ALL_ATTN = if_else(Manip_Check_3_Passed == 1 & Manip_Check_2_Passed == 1 & Manip_Check_1_Passed == 1, 1, 0))
#questionr::freq(Raw_Study_One_Data$PASSED_ALL_ATTN) #393 Passed all Three#


############################################################################################################################################################################
#####################################################################Missing Data Random Forest Model#######################################################################
############################################################################################################################################################################


Missing_Data_RF_FRAME <- dplyr::select(Raw_Study_One_Data, Present_1, Present_2, Present_3, Present_4, Present_5, Present_6, Manip_Check_1_Passed, Present_First_Click, Present_Last_Click, Present_Page_Submit, Present_Click_Count, Best_Future_1, Best_Future_2, Best_Future_3, Best_Future_4,
Best_Future_5, Best_Future_6, Best_Future_Likelihood, Manip_Check_2_Passed, Best_Future_First_Click, Best_Future_Last_Click, Best_Future_Page_Submit, Best_Future_Click_Count, Worst_Future_1, Worst_Future_2, Worst_Future_3, Worst_Future_4, Worst_Future_5, Worst_Future_6, Worst_Future_Likelihood,
Manip_Check_3_Passed, Worst_Future_First_Click, Worst_Future_Last_Click, Worst_Future_Page_Submit, Worst_Future_Click_Count, PASSED_ALL_ATTN, FWB_1, FWB_2, FWB_3, FWB_4, FWB_5, FWB_6, FWB_7, FWB_8, AL_1, AL_2, AL_3, AL_4, AL_5, AL_6, AL_7, AL_8, AL_9, AL_10, AL_11, AL_12, AL_13, AL_14, AL_15, AL_16, AL_17, AL_18, AL_19, AL_20,
RWA_1, RWA_2, RWA_3, RWA_4, RWA_5, RWA_6, RWA_7, RWA_8, RWA_9, RWA_10, RWA_11, RWA_12, RWA_13, RWA_14, RWA_15, PHNS_1, PHNS_2, PHNS_3, PHNS_4, PHNS_5, PHNS_6, PHNS_7, PHNS_8, PHNS_9, PHNS_10, PHNS_11, PHNS_12, PHNS_13, PHNS_14, LOT_1, LOT_2, LOT_3, LOT_4, LOT_5, LOT_6, D1, D4, D6, D7, D8, D12, D13, D14, D15)


#Missing_Data_RF_FRAME <- dplyr::select(Raw_Study_One_Data, Present_1, Present_2, Present_3, Present_4, Present_5, Present_6, Best_Future_1, Best_Future_2, Best_Future_3, Best_Future_4,
#Best_Future_5, Best_Future_6, Best_Future_Likelihood, Worst_Future_1, Worst_Future_2, Worst_Future_3, Worst_Future_4, Worst_Future_5, Worst_Future_6, Worst_Future_Likelihood,
#FWB_1, FWB_2, FWB_3, FWB_4, FWB_5, FWB_6, FWB_7, FWB_8, AL_1, AL_2, AL_3, AL_4, AL_5, AL_6, AL_7, AL_8, AL_9, AL_10, AL_11, AL_12, AL_13, AL_14, AL_15, AL_16, AL_17, AL_18, AL_19, AL_20,
#RWA_1, RWA_2, RWA_3, RWA_4, RWA_5, RWA_6, RWA_7, RWA_8, RWA_9, RWA_10, RWA_11, RWA_12, RWA_13, RWA_14, RWA_15, PHNS_1, PHNS_2, PHNS_3, PHNS_4, PHNS_5, PHNS_6, PHNS_7, PHNS_8, PHNS_9, PHNS_10, PHNS_11, PHNS_12, PHNS_13, PHNS_14, LOT_1, LOT_2, LOT_3, LOT_4, LOT_5, LOT_6, D1, D4, D6, D7, D8, D12, D13, D14, D15)


##Number of Missing Values in Frame##
questionr::freq(is.na(Missing_Data_RF_FRAME)) #917/46920 = 1.7%#
ncol(Missing_Data_RF_FRAME)

####################################################################Test Model#########################################################
set.seed(18)
No_Missing_RF_FRAME = as.data.frame(na.omit(Missing_Data_RF_FRAME))
questionr::freq(is.na(No_Missing_RF_FRAME))
nrow(No_Missing_RF_FRAME)

New_Missing_RF_Frame = prodNA(No_Missing_RF_FRAME, noNA = 0.017)
questionr::freq(is.na(New_Missing_RF_Frame))
#str(New_Missing_RF_Frame, list.len = 110)


#Removing NAs these frames are the same#
mean(No_Missing_RF_FRAME == New_Missing_RF_Frame, na.rm = TRUE)

###Random Forest##
Accuracy_Model = missForest(New_Missing_RF_Frame, maxiter = 100, ntree = 100, variablewise = TRUE, xtrue = No_Missing_RF_FRAME)
MSE_by_Variable <- Accuracy_Model$OOBerror #Mean Square Error by Variable# #Higher Indicates more Error#
#RF Imputed#
Test_Imputed = as.tbl(round(as.data.frame(Accuracy_Model$ximp)))
str(Test_Imputed)

###K Nearest Neigbors Imputation###
KNN_Imputation_Model <- knnImputation(New_Missing_RF_Frame, k = 10, scale = T)
citation("DMwR")

##Mediant Imputation##
Median_Imputation <- imputeMissings::impute(New_Missing_RF_Frame, flag = FALSE)
#RF_Imputation <- imputeMissings::impute(New_Missing_RF_Frame, method = "randomForest", flag = FALSE)


#mean(round(No_Missing_RF_FRAME) == Test_Imputed)
nrmse(round(Accuracy_Model$ximp), round(New_Missing_RF_Frame), xtrue = round(No_Missing_RF_FRAME))

mean(round(KNN_Imputation_Model) == Test_Imputed)
nrmse(KNN_Imputation_Model, round(New_Missing_RF_Frame), xtrue = round(No_Missing_RF_FRAME))

mean(round(Median_Imputation) == Test_Imputed)
nrmse(Median_Imputation, round(New_Missing_RF_Frame), xtrue = round(No_Missing_RF_FRAME))


#regr.eval(trues = No_Missing_RF_FRAME, preds = Test_Imputed) ##Random Forest##
#regr.eval(RF_Imputation, No_Missing_RF_FRAME)
regr.eval(trues = No_Missing_RF_FRAME, preds = KNN_Imputation_Model) ##KNN##
regr.eval(trues = No_Missing_RF_FRAME, preds= Median_Imputation) ##Median/Mode##


#warnings() #Can only include numeric and logical variables#


###############################################################Random Forest Model to Predict Missing Numeric values############################################################
##DMwR Could also be used for a Knn Imputation Method##
set.seed(18) #For Reproducability#
Missing_Data_RF_Model <- missForest(as.matrix(Missing_Data_RF_FRAME), maxiter = 100, ntree = 100, variablewise = FALSE)
Missing_Data_RF_Model$OOBerror #Normalized Root Mean Squared Error to computer percentage out of bag error#

Imputed_Data <- as.tbl(round(as.data.frame(Missing_Data_RF_Model$ximp))) #Assign to tibble object with imputation#
table(is.na(Imputed_Data)) #No Missing Data Points#
str(Imputed_Data) #See Data Structure#
class(Imputed_Data) #Is Tibble


############################################################################Rebuild Dataset with Imputed Data##################################################################
#which(colnames(Raw_Study_Two_Data)=="Best_Future_1")
#which(colnames(Imputed_Data)=="D12")


str(Imputed_Data, list.len = 110)
Imputed_Raw_Data_Study_One <- cbind(Raw_Study_One_Data[, 1:11], Imputed_Data[,1:100], Raw_Study_One_Data[, 111:113], Imputed_Data[,101], Raw_Study_One_Data[, 115:116], Imputed_Data[,102], Raw_Study_One_Data[, 118], Imputed_Data[,103:104], Raw_Study_One_Data[, 121:125], Imputed_Data[,105:108], Raw_Study_One_Data[, 130:133])
str(Imputed_Raw_Data_Study_One, list.len = 150)


#####Create Subset for all participants who passed ATTN Checks#####
##Use dplyr::filter##
Passed_Study_One_Data = dplyr::filter(Imputed_Raw_Data_Study_One, PASSED_ALL_ATTN == 1 & D9 == TRUE)
summary(Passed_Study_One_Data$Duration_Secs/60)


#############################################################################################################################################
####################################################Scale escriptives########################################################################
#############################################################################################################################################


##################################################Chronbach's Alpha Reliability Assessment##############################################
##Use dplyr::select to subset of scale items##
Passed_FWB_Items = dplyr::select(Passed_Study_One_Data, FWB_1, FWB_2, FWB_3, FWB_4, FWB_5, FWB_6, FWB_7, FWB_8)
Passed_AL_Items = dplyr::select(Passed_Study_One_Data, AL_1, AL_2, AL_3, AL_4, AL_5, AL_6, AL_7, AL_8, AL_9, AL_10, AL_11, AL_12, AL_13, AL_14, AL_15, AL_16, AL_17, AL_18, AL_19, AL_20)
Passed_RWA = dplyr::select(Passed_Study_One_Data, RWA_1, RWA_2, RWA_3, RWA_4, RWA_5, RWA_6, RWA_7, RWA_8, RWA_9, RWA_10, RWA_11, RWA_12, RWA_13, RWA_14, RWA_15)
Passed_PHNS = dplyr::select(Passed_Study_One_Data, PHNS_1, PHNS_2, PHNS_3, PHNS_4, PHNS_5, PHNS_6, PHNS_7, PHNS_8, PHNS_9, PHNS_10, PHNS_11, PHNS_12, PHNS_13, PHNS_14)
Passed_LOT = dplyr::select(Passed_Study_One_Data, LOT_1, LOT_2, LOT_3, LOT_4, LOT_5, LOT_6)
Passed_Present <- dplyr::select(Passed_Study_One_Data, Present_1, Present_2, Present_3, Present_4, Present_5, Present_6)
Passed_Best_Future <- dplyr::select(Passed_Study_One_Data, Best_Future_1, Best_Future_2, Best_Future_3, Best_Future_4, Best_Future_5, Best_Future_6)
Passed_Worst_Future <- dplyr::select(Passed_Study_One_Data, Worst_Future_1, Worst_Future_2, Worst_Future_3, Worst_Future_4, Worst_Future_5, Worst_Future_6)



##psych::alpha to compute Cronbach's Alpha and GUttman's Lambda 6##
alpha(Passed_FWB_Items) #FWB 0.9 Alpha#
alpha(Passed_AL_Items) #Libertarianism 0.92 Alpha#
alpha(Passed_RWA) #RWA 0.91 Alpha#
alpha(Passed_PHNS) #PHNS 0.86 Alpha#
alpha(Passed_LOT) #PHNS 0.91 Alpha#
#alpha(Passed_Present)
#alpha(Passed_Best_Future)
#alpha(Passed_Worst_Future)


##########################################################Compute Scale Means######################################################################
#Use dplyr::mutate#

##Free Will Beliefs Scale##
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, FWB = (FWB_1 + FWB_2 + FWB_3 + FWB_4 + FWB_5 + FWB_6 + FWB_7 + FWB_8)/8)
summary(Passed_Study_One_Data$FWB) 


##Totalitarianism-Liberalism##
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, AL = (AL_1 + AL_2 + AL_3 + AL_4 + AL_5 + AL_6 + AL_7 + AL_8 + AL_9 + AL_10 + AL_11 + AL_12 + AL_13 + AL_14 + AL_15 + AL_16 + AL_17 + AL_18 + AL_19 + AL_20)/20)
summary(Passed_Study_One_Data$AL)


##Right Wing Authoritarianism##
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, RWA = (RWA_1 + RWA_2 + RWA_3 + RWA_4 + RWA_5 + RWA_6 + RWA_7 + RWA_8 + RWA_9 + RWA_10 + RWA_11 + RWA_12 + RWA_13 + RWA_14 + RWA_15)/15)
summary(Passed_Study_One_Data$RWA)


##Philosophies of Human Nature Trusworthiness##
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, PHNS = (PHNS_1 + PHNS_2 + PHNS_3 + PHNS_4 + PHNS_5 + PHNS_6 + PHNS_7 + PHNS_8 + PHNS_9 + PHNS_10 + PHNS_11 + PHNS_12 + PHNS_13 + PHNS_14)/14)
summary(Passed_Study_One_Data$PHNS)


##Life Orientation Test##
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, LOT = (LOT_1 + LOT_2 + LOT_3 + LOT_4 + LOT_5 + LOT_6)/6)
summary(Passed_Study_One_Data$LOT)


###############################################################Correlations##################################################################################################################################
Descriptive_Corr = Passed_Study_One_Data %>%
  dplyr::select(FWB, D4, RWA, AL, PHNS, LOT, D12, D13, D14, D15, Duration_Secs)

Scale_Correlation = spearman_corstars(Descriptive_Corr)
print(Scale_Correlation)


###############################################################################################################################
##############################################Exploratory Factor Analysis######################################################
###############################################################################################################################
#use fa() from package 'psych'##

##Verify Structure##
str(Passed_Study_One_Data, list.len = 150)

###Create Scree Plots with Eigen Values for Each Grouping###
par(mfrow = c(1, 3))
Present_Eigen_Scree <- psych::scree(as.matrix(Passed_Present), main = "Present Scree Plot", factors = FALSE) #One Solution Indicated#
Best_Future_Eigen_Scree <- psych::scree(as.matrix(Passed_Best_Future), main = "Best Future Scree Plot", factors = FALSE) #One Solution Indicated#
Worst_Future_Eigen_Scree <- psych::scree(as.matrix(Passed_Worst_Future), main = "Worst Future Scree Plot", factors = FALSE) #One Solution Indicated#
par(mfrow = c(1, 1))


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
questionr::freq(Passed_Study_One_Data$Worst_Future > Passed_Study_One_Data$Best_Future)
##Number of People where the present is higher than the best possible future##
questionr::freq(Passed_Study_One_Data$Present > Passed_Study_One_Data$Best_Future)


###Drop Those lower whose best future is lower than the worst###
Failed_Worst_Best_Data = dplyr::filter(Passed_Study_One_Data, Worst_Future > Best_Future)
str(Failed_Worst_Best_Data )

Passed_Study_One_Data = dplyr::filter(Passed_Study_One_Data, Worst_Future < Best_Future)


#####Compute Difference Variables#####
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, Positive_Potential = Best_Future - Present) #Positive Potential: Best Future Minus Present#
#summary(Passed_Study_One_Data$Positive_Potential) #Mean: 51.97#
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, Negative_Potential = -1*(Worst_Future - Present)) #Negative Potential: Worst Future Minus Present#
#summary(Passed_Study_One_Data$Negative_Potential) #Mean: -78.53#
Passed_Study_One_Data = dplyr::mutate(Passed_Study_One_Data, Absolute_Potential = abs(Best_Future - Worst_Future)) #Absolute Potential: Best Future - Worst#
#summary(Passed_Study_One_Data$Absolute_Potential) #Mean: 130.5#


##Number of people with more Positive than absolue potential##
questionr::freq(Passed_Study_One_Data$Positive_Potential > Passed_Study_One_Data$Absolute_Potential) #14#
##Number of people with more Negative than absolue potential##
questionr::freq(Passed_Study_One_Data$Negative_Potential < Passed_Study_One_Data$Absolute_Potential) #27#


###############################################################################################################
#################################################Normality Check###############################################
###############################################################################################################
str(Passed_Study_One_Data, list.len = 160)

###Free Will Visuals and Descriptives###
hist(Passed_Study_One_Data$FWB, main = "Free Will Histogram")
plot(density(Passed_Study_One_Data$FWB), main = "Free Will Density")
psych::describe(Passed_Study_One_Data$FWB)
agostino.test(Passed_Study_One_Data$FWB) ##Negatively Skewed##
anscombe.test(Passed_Study_One_Data$FWB) ##NS##
shapiro.test(Passed_Study_One_Data$FWB) ##Not Normal##

###Best Future Visuals and Descriptives###
hist(Passed_Study_One_Data$Best_Future, main = "Best Future Histogram")
plot(density(Passed_Study_One_Data$Best_Future), main = "Best Future Density")
psych::describe(Passed_Study_One_Data$Best_Future)
agostino.test(Passed_Study_One_Data$Best_Future) ##Negatively Skewed##
anscombe.test(Passed_Study_One_Data$Best_Future) ##NS##
shapiro.test(Passed_Study_One_Data$Best_Future) ##Not Normal##

###Worst Future Visuals and Descriptives###
hist(Passed_Study_One_Data$Worst_Future, main = "Worst Future Histogram")
plot(density(Passed_Study_One_Data$Worst_Future), main = "Worst Future Density")
psych::describe(Passed_Study_One_Data$Worst_Future)
agostino.test(Passed_Study_One_Data$Worst_Future) ##Positively Skewed##
anscombe.test(Passed_Study_One_Data$Worst_Future) ##NS##
shapiro.test(Passed_Study_One_Data$Worst_Future) ##Not Normal##

###Present Visuals and Descriptives###
hist(Passed_Study_One_Data$Present, main = "Present Histogram")
plot(density(Passed_Study_One_Data$Present), main = "Present Density")
psych::describe(Passed_Study_One_Data$Present)
agostino.test(Passed_Study_One_Data$Present) ##Negatively Skewed##
anscombe.test(Passed_Study_One_Data$Present) ##NS##
shapiro.test(Passed_Study_One_Data$Present) ##Not Normal##

###Positive Potential Visuals and Descriptives###
hist(Passed_Study_One_Data$Positive_Potential, main = "Positive Potential Histogram")
plot(density(Passed_Study_One_Data$Positive_Potential), main = "Positive Potential Density")
psych::describe(Passed_Study_One_Data$Positive_Potential)
agostino.test(Passed_Study_One_Data$Positive_Potential) ##Positively Skewed##
anscombe.test(Passed_Study_One_Data$Positive_Potential) ##NS##
shapiro.test(Passed_Study_One_Data$Positive_Potential) ##Not Normal##

###Negative Potential Visuals and Descriptives###
hist(Passed_Study_One_Data$Negative_Potential, main = "Negative Potential Histogram")
plot(density(Passed_Study_One_Data$Negative_Potential), main = "Negative Potential Density")
psych::describe(Passed_Study_One_Data$Negative_Potential)
agostino.test(Passed_Study_One_Data$Negative_Potential) ##Marginal##
anscombe.test(Passed_Study_One_Data$Negative_Potential) ##Significant##
shapiro.test(Passed_Study_One_Data$Negative_Potential) ##Not Normal##

###Absolute Potential Visuals and Descriptives###
hist(Passed_Study_One_Data$Absolute_Potential, main = "Absolute Potential Histogram")
plot(density(Passed_Study_One_Data$Absolute_Potential), main = "Absolute Potential Density")
psych::describe(Passed_Study_One_Data$Absolute_Potential)
agostino.test(Passed_Study_One_Data$Absolute_Potential) ##Negatively Skewed##
anscombe.test(Passed_Study_One_Data$Absolute_Potential) ##NS##
shapiro.test(Passed_Study_One_Data$Absolute_Potential) ##Not Normal##

###Trait Optimism Visuals and Descriptives###
hist(Passed_Study_One_Data$LOT, main = "Trait Optimism")
plot(density(Passed_Study_One_Data$LOT))
psych::describe(Passed_Study_One_Data$LOT)
agostino.test(Passed_Study_One_Data$LOT) ##Negatively Skewed##
anscombe.test(Passed_Study_One_Data$LOT) ##Significant##
shapiro.test(Passed_Study_One_Data$LOT) ##Not Normal##

###Libertarianism Visuals and Descritpives###
hist(Passed_Study_One_Data$AL, main = "Libertarianism")
plot(density(Passed_Study_One_Data$AL))
psych::describe(Passed_Study_One_Data$AL)
agostino.test(Passed_Study_One_Data$AL) ##NS##
anscombe.test(Passed_Study_One_Data$AL) ##NS##
shapiro.test(Passed_Study_One_Data$AL) ##Not Normal##

###Right Wing Authoritarianism Visuals and Descritpives###
hist(Passed_Study_One_Data$RWA, main = "Right Wing Authoritarianism Histogram")
plot(density(Passed_Study_One_Data$RWA))
psych::describe(Passed_Study_One_Data$RWA)
agostino.test(Passed_Study_One_Data$RWA) ##NS##
anscombe.test(Passed_Study_One_Data$RWA) ##Significant##
shapiro.test(Passed_Study_One_Data$RWA) ##Not Normal##

###Trustworthiness Visuals and Descritpives###
hist(Passed_Study_One_Data$PHNS, main = "Perceived Trustworthiness Histogram")
plot(density(Passed_Study_One_Data$PHNS))
psych::describe(Passed_Study_One_Data$PHNS)
agostino.test(Passed_Study_One_Data$PHNS) ##Positively Skewed##
anscombe.test(Passed_Study_One_Data$PHNS) ##NS##
shapiro.test(Passed_Study_One_Data$PHNS) ##Not Normal##

###Political Orientation Visuals and Descritpives###
hist(Passed_Study_One_Data$D4, main = "Political Orientation Histogram")
plot(density(Passed_Study_One_Data$D4))
psych::describe(Passed_Study_One_Data$D4)
agostino.test(Passed_Study_One_Data$D4) ##Marginal##
anscombe.test(Passed_Study_One_Data$D4) ##Significant Platokurkic##
shapiro.test(Passed_Study_One_Data$D4) ##Not Normal##


############Create Transformed Variables##################
#?transformTukey #Tukeys Ladder of Powers#

##Transform Skewed Free Will Beliefs##
Passed_Study_One_Data$TR_FWB <- transformTukey(Passed_Study_One_Data$FWB)
psych::describe(Passed_Study_One_Data$TR_FWB)
agostino.test(Passed_Study_One_Data$TR_FWB) ##NS##
anscombe.test(Passed_Study_One_Data$TR_FWB) ##Platokurkic##

##Transform Skewed Best Future##
Passed_Study_One_Data$TR_Best_Future <- transformTukey(((Passed_Study_One_Data$Best_Future+100)/10))
#Passed_Study_One_Data$TR_Best_Future <- sqrt(Passed_Study_One_Data$Best_Future)
psych::describe(Passed_Study_One_Data$TR_Best_Future)
agostino.test(Passed_Study_One_Data$TR_Best_Future) ##Small Negative##
anscombe.test(Passed_Study_One_Data$TR_Best_Future) ##Platokurkic##


##Transformed Positive Potential## #Cannot Go Negative#
#Passed_Study_One_Data$TR_Positive_Potential <- transformTukey((Passed_Study_One_Data$Positive_Potential/10)
#Passed_Study_One_Data$TR_Best_Future <- sqrt(Passed_Study_One_Data$Best_Future)
#psych::describe(Passed_Study_One_Data$TR_Positive_Potential)
#agostino.test(Passed_Study_One_Data$TR_Positive_Potential) ##Small Negative##
#anscombe.test(Passed_Study_One_Data$TR_Positive_Potential) ##Platokurkic##

##Transform Skewed Best Future##
Passed_Study_One_Data$TR_PO <- transformTukey(Passed_Study_One_Data$D4)
#Passed_Study_One_Data$TR_Best_Future <- sqrt(Passed_Study_One_Data$Best_Future)
psych::describe(Passed_Study_One_Data$TR_PO)
agostino.test(Passed_Study_One_Data$TR_PO) ##Small Negative##
anscombe.test(Passed_Study_One_Data$TR_PO) ##Platokurkic##


#####################################Descriptive Statistics################################################
str(Passed_Study_One_Data, list.len = 150)

Descriptives_Study_One = round(Passed_Study_One_Data %>%
  dplyr::select(`Free Will Beliefs` = FWB, `Political Orientation` = D4, Present, `Best Possible Future` = Best_Future, `Worst Possible Future` = Worst_Future, `Positive Potential` = Positive_Potential, `Negative Potential` = Negative_Potential, `Absolute Potential` = Absolute_Potential,`Best Future Likelihood` = Best_Future_Likelihood, `Worst Future Likelihood` = Worst_Future_Likelihood, `Libertarianism` = AL, `Right Wing Authoritarianism` = RWA, `Perceived Trustworthiness` = PHNS, `Trait Optimism` = LOT)) #%>%
  #summarise_all(c("mean", "sd", "min", "max", "skew", "kurtosis")),2)

#write.csv(psych::describe(Descriptives_Study_One), "C:/Users/bmoli/Documents/Dissertation/Study One Descriptives Revised.csv")

#######################Correlations###################################
Descriptive_Corr_2 = Passed_Study_One_Data %>%
  dplyr::select(`Free Will Beliefs` = FWB, `Conservative Political Orientation` = D4, Present, `Best Future` = Best_Future, `Worst Future` = Worst_Future, `Likelihood Best Future` = Best_Future_Likelihood, `Likelihood Worst Future` = Worst_Future_Likelihood, `Positive Potential` = Positive_Potential, `Negative Potential` = Negative_Potential, `Absolute Potential` = Absolute_Potential, `Right Wing Authoritarianism` = RWA, `Libertarianism` = AL, `Perceived Trustworthiness` = PHNS, `Trait Optimism` = LOT, `Age` = D1, `Religiousity` = D12)

Full_Correlation = corstars(Descriptive_Corr_2)
print(Full_Correlation, short = FALSE)

Spearman_Full_Correlation = spearman_corstars(Descriptive_Corr_2)
#Descriptive_Matrix2 = rcorr(as.matrix(Descriptive_Corr_2))


######################Visual Aides###################################

GG_FWB_PO_POS_PO = ggplot(Passed_Study_One_Data, aes(x=FWB, y=Positive_Potential)) + geom_jitter(aes(col = D4), size = 3) + geom_smooth(method = "lm") +
  scale_color_continuous(name = "Political Orientation", labels = c("Strongly Liberal", "Moderately Liberal", "Slightly Liberal", "Moderate", "Slightly Conservative", "Moderately Conservative", "Strongly Conservative"), low = "blue", high = "red") + 
  ggtitle("Figure 3: Intersection of Free Will, Political Orientation, and Positive Potential") + xlab("Free Will Beliefs") + ylab("Positive Potential") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text=element_text(size=16,face = "bold"), axis.title=element_text(size=16,face = "bold"), legend.text=element_text(size=14), legend.title = element_text(size=16))

GG_FWB_PO_NEG_PO = ggplot(Passed_Study_One_Data, aes(x=FWB, y=Negative_Potential)) + geom_jitter(aes(col = D4), size = 3) + scale_color_continuous(low = "blue", high = "red") + 
  ggtitle("Free Will, Political Orientation, and Negative Potential") + xlab("Free Will Beliefs") + ylab("Negative Potential")

GG_FWB_PO_PRES = ggplot(Passed_Study_One_Data, aes(x=FWB, y=Present)) + geom_jitter(aes(col = D4), size = 3) + scale_color_continuous(low = "blue", high = "red") + 
  ggtitle("Free Will, Political Orientation, and Present") + xlab("Free Will Beliefs") + ylab("Present")

GG_FWB_PO_BEST = ggplot(Passed_Study_One_Data, aes(x=FWB, y=Best_Future)) + geom_jitter(aes(col = D4), size = 3) + scale_color_continuous(low = "blue", high = "red") + 
  ggtitle("Free Will, Political Orientation, and Best Future") + xlab("Free Will Beliefs") + ylab("Best Future")

GG_FWB_PO_WORST = ggplot(Passed_Study_One_Data, aes(x=FWB, y=Worst_Future)) + geom_jitter(aes(col = D4), size = 3) + scale_color_continuous(low = "blue", high = "red") + 
  ggtitle("Free Will, Political Orientation, and Worst Future") + xlab("Free Will Beliefs") + ylab("Worst Future")


########################Standardized Potential##############################

#Ratio_Positive_Negative_Potential = Passed_Study_One_Data$Positive_Potential/Passed_Study_One_Data$Negative_Potential
#summary(Ratio_Positive_Negative_Potential)

Standardized_Positive_Potential = abs(Passed_Study_One_Data$Positive_Potential/Passed_Study_One_Data$Absolute_Potential)
summary(Standardized_Positive_Potential)
table(abs(Standardized_Positive_Potential)>1)
plot(Standardized_Positive_Potential)


Standardized_Negative_Potential = abs(Passed_Study_One_Data$Negative_Potential/Passed_Study_One_Data$Absolute_Potential)
summary(Standardized_Negative_Potential)
table(abs(Standardized_Negative_Potential)>1)


Standardized_Potential = as.data.frame(cbind(Standardized_Positive_Potential, Standardized_Negative_Potential))
#Standardized_Potential = dplyr::filter(Standardized_Potential, Standardized_Positive_Potential < 1, Standardized_Negative_Potential < 1)
Standardized_Potential$Combined = Standardized_Potential$Standardized_Positive_Potential + Standardized_Potential$Standardized_Negative_Potential
summary(Standardized_Potential$Standardized_Positive_Potential)
summary(Standardized_Potential$Standardized_Negative_Potential)


##################################################################################################################################################################
#########################################################Test of Mediational Relationship#########################################################################
##################################################################################################################################################################
#?psych::mediate
###FWB Mediated by PO###
Mediation_FWB_PO_Potential = psych::mediate(y = "Positive_Potential", x = "FWB", m = "D4", data = Passed_Study_One_Data, std = TRUE)
Mediation_FWB_PO_Potential #main = "Figure #: Medidation of Free Will Beliefs and Positive Potential by Political Orientation"

###PO Mediated by FWB###
Alternative_Mediation_PO_POS_Potential = psych::mediate(y = "Positive_Potential", x = "D4", m = "FWB", data = Passed_Study_One_Data, std = TRUE)
Alternative_Mediation_PO_POS_Potential

###TRANSFORMED FWB Mediated by PO###
TR_Mediation_FWB_PO_Potential = psych::mediate(y = "Positive_Potential", x = "TR_FWB", m = "TR_PO", data = Passed_Study_One_Data, std = TRUE)
TR_Mediation_FWB_PO_Potential

##################################################################################################################################################################
#########################################################Secondary Mediational Tests#############################################################################
##################################################################################################################################################################


###FWB Mediated by AL###
Alternative_Mediation_AL_POS_Potential = psych::mediate(y = "Positive_Potential", x = "FWB", m = "AL", data = Passed_Study_One_Data, std = TRUE)
Alternative_Mediation_AL_POS_Potential

###FWB Mediated by RWA
Alternative_Mediation_RWA_POS_Potential = psych::mediate(y = "Positive_Potential", x = "FWB", m = "RWA", data = Passed_Study_One_Data, std = TRUE)
Alternative_Mediation_RWA_POS_Potential

###FWB Mediated by AL and PO###
Alternative_Mediation_AL_PO_POS_Potential = psych::mediate(y = "Positive_Potential", x = "FWB", m = c("AL", "D4"), data = Passed_Study_One_Data, std = TRUE)
Alternative_Mediation_AL_PO_POS_Potential

###FWB Mediated by RWA and PO###
Alternative_Mediation_RWA_PO_POS_Potential = psych::mediate(y = "Positive_Potential", x = "FWB", m = c("RWA", "D4"), data = Passed_Study_One_Data, std = TRUE)
Alternative_Mediation_RWA_PO_POS_Potential

###RWA Mediated by FWB and PO###
Alternative_Mediation_RWA_PO_POS_Potential = psych::mediate(y = "Positive_Potential", x = "RWA", m = c("D4", "FWB"), data = Passed_Study_One_Data, std = TRUE)
Alternative_Mediation_RWA_PO_POS_Potential

###FWB Mediated by LOT and PO###
Alternative_Mediation_LOT_PO_POS_Potential = psych::mediate(y = "Positive_Potential", x = "FWB", m = c("LOT", "D4"), data = Passed_Study_One_Data, std = TRUE)
Alternative_Mediation_LOT_PO_POS_Potential

###FWB Mediated by PO###
Mediation_FWB_PHNS_PO_Potential = psych::mediate(y = "Positive_Potential", x = "FWB", m = "D4", mod = "PHNS", data = Passed_Study_One_Data, std = TRUE)
Mediation_FWB_PHNS_PO_Potential

###FWB and Present Mediated by PO###
Mediation_FWB_Present = psych::mediate(y = "Present", x = "FWB", m = "D4", data = Passed_Study_One_Data, std = TRUE)
Mediation_FWB_Present

########################################################################################################################################
#############################################################Cluster Analysis###########################################################
########################################################################################################################################
str(Passed_Study_One_Data, list.len = 160)

##Select Clustering Variables##\
Cluster_variables <- dplyr::select(Passed_Study_One_Data, FWB, D4, Present, Best_Future, Worst_Future, D1, D6, D7, D8, D12)
#Alt_Cluster_variables <- dplyr::select(Passed_Study_One_Data, FWB, D4, Best_Future, Worst_Future, D12, LOT, Present)
Alt_Cluster_variables <- dplyr::select(Passed_Study_One_Data, FWB, D4, Best_Future, Present, Worst_Future)

##Scale Cluster Variables by Z Score before Computing Distance matrix##
#Scale_Cluster_variables <- scale(Cluster_variables)
#Euclidean_Cluster_variables <- dist(Scale_Cluster_variables, method = "euclidean")

#Factor_Extraction <- pamk(Cluster_variables, krange = 2:10, metric = "euclidean", stand = TRUE)
#Factor_Extraction$nc #3 Clusters Optimal based on Silhouettes#
#Factor_Extraction$crit #3 Clusters Optimal based on Silhouettes#

Alt_Factor_Extraction <- pamk(Alt_Cluster_variables, krange = 2:10, metric = "euclidean", stand = TRUE)
Alt_Factor_Extraction$nc #2 Clusters Optimal based on Silhouettes#
Alt_Factor_Extraction$crit #2 Clusters Optimal based on Silhouettes#

#?fviz_nbclust
Alt_Scree_Plot <- fviz_nbclust(Alt_Cluster_variables, FUNcluster = pam, method = "wss", k.max = 10, stand = TRUE) + ggtitle("Figure 5: Total Variability Explained (Elbow Method) for Cluster Analysis") + labs(x = "Number of Clusters (K)") +
  geom_vline(xintercept = 4, linetype = 2, col = "blue") + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text=element_text(size=12,face = "bold"), axis.title=element_text(size=14,face = "bold"), legend.text=element_text(size=10), legend.title = element_text(size=10))

Alt_Silhouette_Plot <- fviz_nbclust(Alt_Cluster_variables, FUNcluster = pam, method = "silhouette", k.max = 10, stand = TRUE, metric = "euclidean")
Alt_Gap_Stat_Plot <- fviz_nbclust(Alt_Cluster_variables, FUNcluster = pam, method = "gap_stat", k.max = 10, stand = TRUE)


#Pam_Factor_Analysis <- pam(Cluster_variables, k = 3, metric = "euclidean", stand = TRUE)
#Pam_Factor_Analysis$clusinfo
#Pam_Factor_Analysis$medoids
#Pam_Factor_Analysis$silinfo

Alt_Pam_Factor_Analysis <- pam(Alt_Cluster_variables, k = 4, metric = "euclidean", stand = TRUE)
Alt_Pam_Factor_Analysis$clusinfo
Alt_Pam_Factor_Analysis$medoids
Alt_Pam_Factor_Analysis$silinfo$clus.avg.widths
Alt_Pam_Factor_Analysis$silinfo$avg.width


#Passed_Study_One_Data$Clusters <- as.factor(Pam_Factor_Analysis$clustering)
Passed_Study_One_Data$Alt_Clusters <- as.factor(Alt_Pam_Factor_Analysis$clustering)


#Cluster_Means = Passed_Study_One_Data %>% 
  #dplyr::select(FWB, D4, Present, Best_Future, Worst_Future, D1, D6, D7, D8, D12, Clusters) %>%
  #group_by(Clusters) %>%
  #summarise(Mean_FWB = mean(FWB), Mean_PO = mean(D4), Mean_Present = mean(Present), Mean_Best_Fut = mean(Best_Future), Mean_Worst_Fut = mean(Worst_Future), Mean_Age = mean(D1), Mean_Edu = mean(D6), Mean_Income = mean(D7), Mean_SSES = mean(D8), Mean_Rel = mean(D12))

questionr::freq(Passed_Study_One_Data$D5)

Alt_Cluster_Means = Passed_Study_One_Data %>% 
  group_by(Alt_Clusters) %>%
  #dplyr::select(FWB, D4, Present, Best_Future, Worst_Future, D12, Alt_Clusters, LOT) %>%
  summarise(Mean_FWB = mean(FWB), Mean_PO = mean(D4), Mean_Present = mean(Present), Mean_Best_Fut = mean(Best_Future), Mean_Worst_Fut = mean(Worst_Future), Mean_Rel = mean(D12), Mean_optimism = mean(LOT), N = n(), Republicans = sum(D5 == "Republican Party", na.rm = TRUE), Democrats = sum(D5 == "Democratic Party", na.rm = TRUE), Libertarians = sum(D5 == "Libertarian Party", na.rm = TRUE))


Pub_Cluster_Means = Passed_Study_One_Data %>% 
  group_by(Clusters = Alt_Clusters) %>%
  #dplyr::select(FWB, D4, Present, Best_Future, Worst_Future, D12, Alt_Clusters, LOT) %>%
  summarise(`Free Will Beliefs` = paste0(round(mean(FWB),2), " (", round(sd(FWB),2), ")"), `Conservative Political Orientation` = paste0(round(mean(D4),2), " (", round(sd(D4),2), ")"), Present = paste0(round(mean(Present),2), " (", round(sd(Present),2), ")"), `Best Future` = paste0(round(mean(Best_Future),2), " (", round(sd(Best_Future),2), ")"), `Worst Future` = paste0(round(mean(Worst_Future),2), " (", round(sd(Worst_Future),2), ")"), Religiousity = paste0(round(mean(D12),2), " (", round(sd(D12),2), ")"), `Trait Optimism` = paste0(round(mean(LOT),2), " (", round(sd(LOT),2), ")"), `Size of Cluster` = n())

Alt_MAnova = manova(cbind(FWB, D4, Present, Best_Future, Worst_Future) ~ Alt_Clusters, data = Passed_Study_One_Data)
summary(Alt_MAnova, test="Pillai")
summary.aov(Alt_MAnova)
class(Alt_MAnova)
heplots::etasq(Alt_MAnova)


Alt_FWB = aov(FWB ~ Alt_Clusters, data = Passed_Study_One_Data)
model.tables(Alt_FWB, type = "means", se = T)
TukeyHSD(Alt_FWB)
plot(TukeyHSD(Alt_FWB))


Alt_PO = aov(D4 ~ Alt_Clusters, data = Passed_Study_One_Data)
model.tables(Alt_PO, type = "means", se = T)
TukeyHSD(Alt_PO)
plot(TukeyHSD(Alt_PO))


Alt_Best = aov(Best_Future ~ Alt_Clusters, data = Passed_Study_One_Data)
model.tables(Alt_Best, type = "means", se = T)
TukeyHSD(Alt_Best)
plot(TukeyHSD(Alt_Best))


Alt_Pres = aov(Present ~ Alt_Clusters, data = Passed_Study_One_Data)
model.tables(Alt_Pres, type = "means", se = T)
TukeyHSD(Alt_Pres)
plot(TukeyHSD(Alt_Pres))


Alt_Worst = aov(Worst_Future ~ Alt_Clusters, data = Passed_Study_One_Data)
model.tables(Alt_Worst, type = "means", se = T)
TukeyHSD(Alt_Worst)
plot(TukeyHSD(Alt_Worst))


Alt_LOT = aov(LOT ~ Alt_Clusters, data = Passed_Study_One_Data)
model.tables(Alt_LOT, type = "means", se = T)
TukeyHSD(Alt_LOT)
plot(TukeyHSD(Alt_LOT))


Alt_Rel = aov(D12 ~ Alt_Clusters, data = Passed_Study_One_Data)
model.tables(Alt_Rel, type = "means", se = T)
TukeyHSD(Alt_Rel)
plot(TukeyHSD(Alt_Rel))



GG_FWB_PO_POS_Clus = ggplot(Passed_Study_One_Data, aes(x=FWB, y=Positive_Potential)) + geom_jitter(aes(col = Alt_Clusters), size = 3) + geom_smooth(method = "lm", show.legend = FALSE) +
  ggtitle("Figure 6: Free Will and Positive Potential with Clusters") + xlab("Free Will Beliefs") + ylab("Positive Potential") + scale_colour_manual(values = c("cyan3", "darkblue", "darkred", "orange"), labels = c("1", "2", "3", "4"), name = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), legend.direction = "horizontal", legend.position = "top")

GG_FWB_Clus_BEST = ggplot(Passed_Study_One_Data, aes(x=FWB, y=Best_Future)) + geom_jitter(aes(col = Alt_Clusters), size = 3) + geom_smooth(method = "lm", show.legend = FALSE) +
  ggtitle("Figure 7: Free Will and Best Future with Clusters") + xlab("Free Will Beliefs") + ylab("Best Future") + scale_colour_manual(values = c("cyan3", "darkblue", "darkred", "orange"), labels = c("1", "2", "3", "4"), name = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), legend.direction = "horizontal", legend.position = "top")

grid.arrange(GG_FWB_PO_POS_Clus, GG_FWB_Clus_BEST)

corr.test(Passed_Study_One_Data %>%
             filter(Alt_Clusters != 3) %>%
             dplyr::select(FWB, Best_Future, Present, Worst_Future, Positive_Potential, LOT, D4, RWA))

###FWB and Present Moderated by PO###
Moderation_FWB_Present = psych::mediate(y = "Present", x = "D4", mod = "FWB", data = Passed_Study_One_Data, std = TRUE)
Moderation_FWB_Present

###FWB and Present Moderated by PO###
Moderation_FWB_Best_Future = psych::mediate(y = "Best_Future", x = "D4", mod = "FWB", data = Passed_Study_One_Data, std = TRUE)
Moderation_FWB_Best_Future

GLM_Moderation_FWB_Best_Future = glm(Best_Future ~ D4*FWB, data = Passed_Study_One_Data)
reghelper::beta(GLM_Moderation_FWB_Best_Future , x = FALSE, y = TRUE)
summary(GLM_Moderation_FWB_Best_Future)

Mod_Best_Fut = interact_plot(GLM_Moderation_FWB_Best_Future, pred = "D4", modx = "FWB", plot.points = FALSE, interval = TRUE, color.class = c('#91bfdb','#af8dc3', '#fc8d59'), x.label = "Political Orientation (Conservative High)", y.label = "Best Future", legend.main = "Free Will Beliefs", main.title = "Figure 8: Moderation of Political Orientation and Best Future by Free Will Beliefs") + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text=element_text(size=16,face = "bold"), axis.title=element_text(size=16,face = "bold"), legend.text=element_text(size=14), legend.title = element_text(size=16))

High_FWB = Passed_Study_One_Data %>% filter(FWB >= (mean(FWB) + sd(FWB)))
cor.test(High_FWB$D4, High_FWB$Best_Future)
cor.test(High_FWB$D4, High_FWB$Positive_Potential)
cor.test(High_FWB$D4, High_FWB$Present)

Low_FWB = Passed_Study_One_Data %>% filter(FWB <= (mean(FWB) - sd(FWB)))
cor.test(Low_FWB$D4, Low_FWB$Best_Future)
cor.test(Low_FWB$D4, Low_FWB$Positive_Potential)
cor.test(Low_FWB$D4, Low_FWB$Present)


GLM_Moderation_FWB_Present = glm(Present ~ D4*FWB, data = Passed_Study_One_Data)
reghelper::beta(GLM_Moderation_FWB_Present , x = FALSE, y = TRUE)
summary(GLM_Moderation_FWB_Present)

Mod_Present = interact_plot(GLM_Moderation_FWB_Present, pred = "D4", modx = "FWB", plot.points = FALSE, interval = TRUE, color.class = c('#91bfdb','#af8dc3', '#fc8d59'), x.label = "Political Orientation (Conservative High)", y.label = "Present", legend.main = "Free Will Beliefs", main.title = "Figure 8: Moderation of Political Orientation and Present by Free Will Beliefs") 


#?psych::mediate

###FWB and Present Moderated by PHNS###
Moderation_FWB_PHNS_Pos_Po = psych::mediate(y = "Positive_Potential", x = "FWB", mod = "PHNS", data = Passed_Study_One_Data, std = TRUE)
Moderation_FWB_PHNS_Pos_Po$total.reg

GLM_Moderation_FWB_Pos_Po = glm(Positive_Potential ~ FWB*D4, data = Passed_Study_One_Data)
reghelper::beta(GLM_Moderation_FWB_Pos_Po, x = FALSE, y = TRUE)
summary(GLM_Moderation_FWB_Pos_Po)


Moderation_PO_FWB_Pos_Po = psych::mediate(y = "Positive_Potential", x = "D4", mod = "FWB", data = Passed_Study_One_Data, std = TRUE)
Moderation_PO_FWB_Pos_Po

Mod_Pos_Po = interact_plot(GLM_Moderation_FWB_Pos_Po, pred = "D4", modx = "FWB", plot.points = FALSE, interval = TRUE, color.class = c('#91bfdb','#af8dc3', '#fc8d59'), x.label = "Political Orientation (Conservative High)", y.label = "Positive Potential", legend.main = "Free Will Beliefs", main.title = "Figure 9: Moderation of Political Orientation and Positive Potential by Free Will Beliefs") + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text=element_text(size=16,face = "bold"), axis.title=element_text(size=16,face = "bold"), legend.text=element_text(size=14), legend.title = element_text(size=16))


grid.arrange(Mod_Best_Fut, Mod_Pos_Po)


Moderation_PHNS <- glm(Positive_Potential ~ FWB + PHNS + (FWB*PHNS), data = Passed_Study_One_Data)
summary(Moderation_PHNS)


###FWB and Best Future Moderated by PO###
Moderation_FWB_Best_Future = psych::mediate(y = "Best_Future", x = "FWB", mod = "D4", data = Passed_Study_One_Data, std = TRUE)
Moderation_FWB_Best_Future

###FWB and Worst Future Moderated by PO###
Moderation_FWB_Worst_Future = psych::mediate(y = "Worst_Future", x = "FWB", mod = "D4", data = Passed_Study_One_Data, std = TRUE)
Moderation_FWB_Worst_Future

###FWB and Best Future Moderated by PO###
Moderation_FWB_Positive_Potential = psych::mediate(y = "Positive_Potential", x = "FWB", mod = "D4", data = Passed_Study_One_Data, std = TRUE)
Moderation_FWB_Positive_Potential

GG_FWB_LOT_POS = ggplot(Passed_Study_One_Data, aes(x=FWB, y=Positive_Potential)) + geom_jitter(aes(col = LOT), size = 3) + scale_color_continuous(low = "blue", high = "white") + 
  ggtitle("Free Will, Optimism, and Positive Potential") + xlab("Free Will Beliefs") + ylab("Positive Potential")



GLM_Moderation_FWB_Liber_Pos_Po = glm(Best_Future ~ AL*FWB, data = Passed_Study_One_Data)
reghelper::beta(GLM_Moderation_FWB_Liber_Pos_Po, x = FALSE, y = TRUE)
summary(GLM_Moderation_FWB_Liber_Pos_Po)

#######################################################################################################################################
#####################################Extras: Individual Correlations###################################################################
#######################################################################################################################################


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
cor.test(Passed_Study_One_Data$D4, Passed_Study_One_Data$RWA) #0.35#

cor.test(Passed_Study_One_Data$D4, Passed_Study_One_Data$Best_Future_Likelihood)
cor.test(Passed_Study_One_Data$D4, Passed_Study_One_Data$Worst_Future_Likelihood)


cor.test(Passed_Study_One_Data$FWB, Passed_Study_One_Data$Best_Future_Likelihood)
cor.test(Passed_Study_One_Data$FWB, Passed_Study_One_Data$Worst_Future_Likelihood)
###Free Will and Time Estimates###
cor.test(Passed_Study_One_Data$Present, Passed_Study_One_Data$FWB)
cor.test(Passed_Study_One_Data$Best_Future, Passed_Study_One_Data$FWB)
cor.test(Passed_Study_One_Data$Worst_Future, Passed_Study_One_Data$FWB)

cor.test(Passed_Study_One_Data$Positive_Potential, Passed_Study_One_Data$FWB)
cor.test(Passed_Study_One_Data$Negative_Potential, Passed_Study_One_Data$FWB)
cor.test(Passed_Study_One_Data$Absolute_Potential, Passed_Study_One_Data$FWB)

###Political Orientation and Time Estimates###
cor.test(Passed_Study_One_Data$Present, Passed_Study_One_Data$D4)
cor.test(Passed_Study_One_Data$Best_Future, Passed_Study_One_Data$D4)
cor.test(Passed_Study_One_Data$Worst_Future, Passed_Study_One_Data$D4)

cor.test(Passed_Study_One_Data$Positive_Potential, Passed_Study_One_Data$D4)
cor.test(Passed_Study_One_Data$Negative_Potential, Passed_Study_One_Data$D4)
cor.test(Passed_Study_One_Data$Absolute_Potential, Passed_Study_One_Data$D4)

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


cor.test(Passed_Study_One_Data$Worst_Future_Likelihood, Passed_Study_One_Data$Worst_Future)
cor.test(Passed_Study_One_Data$Best_Future_Likelihood, Passed_Study_One_Data$Worst_Future)

cor.test(Passed_Study_One_Data$Best_Future_Likelihood, Passed_Study_One_Data$Best_Future)
cor.test(Passed_Study_One_Data$Worst_Future_Likelihood, Passed_Study_One_Data$Best_Future)


cor.test(Passed_Study_One_Data$Best_Future_Likelihood, Passed_Study_One_Data$Positive_Potential)
cor.test(Passed_Study_One_Data$Worst_Future_Likelihood, Passed_Study_One_Data$Positive_Potential)


###Write XLSX###
#write.csv(as.data.frame(Full_Correlation), "C:/Users/bmoli/Documents/Dissertation/Study One Correlation.csv", na = "")
#write.csv(as.data.frame(Pub_Cluster_Means), "C:/Users/bmoli/Documents/Dissertation/Study One Cluster Means Table.csv", na = "")
#?write.csv

Demographics = Passed_Study_One_Data %>% dplyr::select(Age = D1, Gender = D2, Race = D3, PO = D4, Affiliation = D5, Education = D6, SES = D7, SSES = D8, Citizen = D9, Region = D10, RA = D11, Religiousity = D12)
mean(Demographics$Age)
questionr::freq(Demographics$Gender)
questionr::freq(Demographics$PO)
questionr::freq(Demographics$SES)
questionr::freq(Demographics$Education)
questionr::freq(Demographics$Race)
#lsr::table(Demographics$SSES)
mean(Demographics$Religiousity)

    