###Install.Packages to Install Important Packages###
#install.packages("dplyr") #For Data manipulation#
#install.packages("plyr") #For Data manipulation#
#install.packages("psych") #For Psychology Based Statistics and Analyses#
#install.packages("tidyr") #For Data Cleaning#
#install.packages("readxl") #To Read in Excel Documents#
#install.packages("rematch") #For readxl#
#install.packages("lsr") #For questionr::freq function#
#install.packages("missForest") #Uses Nonparametric Random Forest modeling to perform missing data imputation#
#install.packages("nFactors") #Scree Test and Factor Dimension Extraction#
#install.packages("Hmisc") #For Correlation Matrix with P values#
#install.packages("ggplot2") #For Visualization of Data#
#install.packages("RColorBrewer") #Color Pallets for ggplot2
#install.packages("mediation") #To Conduct Mediational Analyses#
#install.packages("questionr") #questionr::frequency Function#
#install.packages("DMwR") #KNN Imputation
#install.packages("imputeMissings") #Median/Mode Imputation#
#install.packages("corrplot") #For Correlation Plot#
#install.packages("lavaan") #For Strucutural Equation Modeling#
#install.packages("semTools")
#install.packages("semPlot") #To plot sem Models#
#install.packages("reghelper") #For Standardized Beta#
#install.packages("rcompanion") #For Normality Transformations#
#install.packages("gridExtra") #grid.arrange#
#install.packages("ppcor") #For Partial Correlations#
#install.packages("jtools") #Simple slopes#
#install.packages("CTT") #CTT: Classical Test Theory#
#install.packages("foreach") #Parallel Computing#
#install.packages("doParallel") #Parallel Computing#
#install.packages("parallel") #Parallel Computing#
#install.packages("microbenchmark") #Parallel Computing#
#install.packages("ggraph")
#install.packages("tidyverse")
#install.packages("tidygraph")


###Load Necessary Packages into Workspace###
library(Hmisc)
library(plyr)
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
library(lavaan)
library(semTools)
library(semPlot)
library(reghelper)
library(rcompanion)
library(gridExtra)
library(ppcor)
library(jtools)
#library(CTT)
library(foreach)
library(doParallel)
library(parallel)
library(microbenchmark)
library(ggraph)


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
"Worst_Future_Click_Count", "FWB_1", "FWB_2", "FWB_3", "FWB_4", "FWB_5", "FWB_6", "FWB_7", "FWB_8", "PIP_1",	"PIP_2",	"PIP_3",	"PIP_4",	"PIP_5",	"PIP_6",	"PIP_7",	"PIP_8",	"PIP_9",	"PIP_10",	"PIP_11",	"PIP_12",	"PIP_13",	"PIP_14",	"PIP_15",	"PIP_16",	"PIP_17",	"PIP_18",	"PIP_19",	"PIP_20",	"PIP_21",	"PIP_22",	"PIP_23",	"PIP_24",	"PIP_25",	"PIP_26",
"PIP_27", "PIP_28",	"PIP_29",	"PIP_30", "ADM_1", "ADM_2", "ADM_3", "ADM_4", "ADM_5", "ADM_6", "Gov_1", "Gov_2", "Gov_3",	"Gov_4",	"Gov_5",	"Gov_6", "LOT_1", "LOT_2", "LOT_3", "LOT_4", "LOT_5", "LOT_6", "SVS_1", "SVS_2", "SVS_3", "SVS_4", "SVS_5", "SVS_6", "SVS_7", "SVS_8", "SVS_9", "SVS_10", "D1",
"D2", "D3", "D3_TXT", "D4", "D5", "D5_TXT", "D6", "D6_TXT", "D7", "D8", "D9", "D10", "D10_TXT", "D11", "D11_TXT", "D12", "D13", "D14", "D15", "Comments", "Code", "Society_Random_Order", "Survey_Random_Order1", "Survey_Random_Order2")

###Load XLSX File with Data from Dropbox###
#?read_excel #Use this function to read in Excel Document
Raw_Study_Two_Data <- read_excel("C:/Users/bmoli/Documents/Dissertation/Dissertation Study 2 (Full) w Randomization.xlsx", range = "A4:EG507", sheet = "Dissertation Study 2 w Randomiz", col_names = Var_Col)


##Verify Structure of the Data##
str(Raw_Study_Two_Data, list.len = 140)
summary(Raw_Study_Two_Data$Duration_Secs/60) ##Average Time to Complete Study in Minutes##
questionr::freq(is.na(Raw_Study_Two_Data))

#################################################################################################################################################################
#######################################################Recode Data Variables for Cleanining######################################################################
#################################################################################################################################################################

######################################################Free Will Beliefs Scale#################################################################
Raw_Study_Two_Data$FWB_1 = recode(Raw_Study_Two_Data$FWB_1, `Strongly Disagree` = 1, `Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Agree` = 6, `Strongly Agree` = 7) #People have complete control over the decisions they make#
#summary(Raw_Study_Two_Data$FWB_1) #Mean: 5.2#

Raw_Study_Two_Data$FWB_2 = recode(Raw_Study_Two_Data$FWB_2, `Strongly Disagree` = 1, `Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Agree` = 6, `Strongly Agree` = 7) #People must take full responsibility for any bad choices they make#
#summary(Raw_Study_Two_Data$FWB_2) #Mean: 5.8#

Raw_Study_Two_Data$FWB_3 = recode(Raw_Study_Two_Data$FWB_3, `Strongly Disagree` = 1, `Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Agree` = 6, `Strongly Agree` = 7) #People can overcome any obstacles if they truly wan to#
#summary(Raw_Study_Two_Data$FWB_3) #Mean: 5.1#

Raw_Study_Two_Data$FWB_4 = recode(Raw_Study_Two_Data$FWB_4, `Strongly Disagree` = 1, `Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Agree` = 6, `Strongly Agree` = 7) #People are totally responsible for the bad things they do#
#summary(Raw_Study_Two_Data$FWB_4) #Mean: 5.3#

Raw_Study_Two_Data$FWB_5 = recode(Raw_Study_Two_Data$FWB_5, `Strongly Disagree` = 1, `Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Agree` = 6, `Strongly Agree` = 7) #People have complete free will#
#summary(Raw_Study_Two_Data$FWB_5) #Mean: 5.0#

Raw_Study_Two_Data$FWB_6 = recode(Raw_Study_Two_Data$FWB_6, `Strongly Disagree` = 1, `Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Agree` = 6, `Strongly Agree` = 7) #Criminals are totally responsible for the bad things they do#
#summary(Raw_Study_Two_Data$FWB_6) #Mean: 5.3#

Raw_Study_Two_Data$FWB_7 = recode(Raw_Study_Two_Data$FWB_7, `Strongly Disagree` = 1, `Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Agree` = 6, `Strongly Agree` = 7) #People are always at fault for their bad behavior#
#summary(Raw_Study_Two_Data$FWB_7) #Mean: 5.0#

Raw_Study_Two_Data$FWB_8 = recode(Raw_Study_Two_Data$FWB_8, `Strongly Disagree` = 1, `Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Agree` = 6, `Strongly Agree` = 7) #Strength of will can always overcome the body's desires#
#summary(Raw_Study_Two_Data$FWB_8) #Mean: 4.9#



#################################################################Government Trust Survey#########################################################################
##High = More Trust/Support##
Raw_Study_Two_Data$Gov_1 = recode(Raw_Study_Two_Data$Gov_1, `Strongly Disagree` = 1, `Moderately Disagree` = 2, `Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Moderately Agree` = 6, `Strongly Agree` = 7) #People can generally trust the government to do what is right#
#summary(Raw_Study_Two_Data$Gov_1) #Mean: 4.4#

Raw_Study_Two_Data$Gov_2 = recode(Raw_Study_Two_Data$Gov_2, `Strongly Disagree` = 1, `Moderately Disagree` = 2, `Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Moderately Agree` = 6, `Strongly Agree` = 7) #Hardly anyone in the government is corrupt#
#summary(Raw_Study_Two_Data$Gov_2) #Mean: 2.6#

Raw_Study_Two_Data$Gov_3 = recode(Raw_Study_Two_Data$Gov_3, `Strongly Disagree` = 7, `Moderately Disagree` = 6, `Disagree` = 5, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 3, `Moderately Agree` = 2, `Strongly Agree` = 1) #Reversed: The government exists for the benefit of a few big interest groups#
#summary(Raw_Study_Two_Data$Gov_3) #Mean: 3.1#

Raw_Study_Two_Data$Gov_4 = recode(Raw_Study_Two_Data$Gov_4, `Strongly Disagree` = 1, `Moderately Disagree` = 2, `Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Moderately Agree` = 6, `Strongly Agree` = 7) #The government is very capable of achieving its goals#
#summary(Raw_Study_Two_Data$Gov_4) #Mean: 4.7#

Raw_Study_Two_Data$Gov_5 = recode(Raw_Study_Two_Data$Gov_5, `Strongly Disagree` = 1, `Moderately Disagree` = 2, `Disagree` = 3, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 5, `Moderately Agree` = 6, `Strongly Agree` = 7) #When the government passes a new law or policy, it is generally implemented as it was intended#
#summary(Raw_Study_Two_Data$Gov_5) #Mean: 3.7#

Raw_Study_Two_Data$Gov_6 = recode(Raw_Study_Two_Data$Gov_6, `Strongly Disagree` = 7, `Moderately Disagree` = 6, `Disagree` = 5, `Neither Agree nor Disagree` = 4, `Slightly Agree` = 3, `Moderately Agree` = 2, `Strongly Agree` = 1) #Reversed: Government policies and laws are often implemented with damaging effects to society#
#summary(Raw_Study_Two_Data$Gov_6) #Mean: 3.3#



#################################################################Life Orientation Test#########################################################################
##High = Optimistic##
Raw_Study_Two_Data$SVS_1 = recode(Raw_Study_Two_Data$SVS_1, `Opposed to my Principles` = 0, `Not At All Important` = 1, `Not Very Important` = 2, `Slightly Important` = 3, `Moderately Important` = 4, `Very Important` = 5, `Highly Important` = 6, `Of Supreme Importance` = 7) #POWER (social power, authority, wealth)#
#summary(Raw_Study_Two_Data$SVS_1) #Mean 3.2#

Raw_Study_Two_Data$SVS_2 = recode(Raw_Study_Two_Data$SVS_2, `Opposed to my Principles` = 0, `Not At All Important` = 1, `Not Very Important` = 2, `Slightly Important` = 3, `Moderately Important` = 4, `Very Important` = 5, `Highly Important` = 6, `Of Supreme Importance` = 7) #ACHIEVEMENT (success, capability, ambition, influence on people and events)#
#summary(Raw_Study_Two_Data$SVS_2) #Mean 4.6#

Raw_Study_Two_Data$SVS_3 = recode(Raw_Study_Two_Data$SVS_3, `Opposed to my Principles` = 0, `Not At All Important` = 1, `Not Very Important` = 2, `Slightly Important` = 3, `Moderately Important` = 4, `Very Important` = 5, `Highly Important` = 6, `Of Supreme Importance` = 7) #HEDONISM (gratification of desires, enjoyment in life, self-indulgence)#
#summary(Raw_Study_Two_Data$SVS_3) #Mean 3.5#

Raw_Study_Two_Data$SVS_4 = recode(Raw_Study_Two_Data$SVS_4, `Opposed to my Principles` = 0, `Not At All Important` = 1, `Not Very Important` = 2, `Slightly Important` = 3, `Moderately Important` = 4, `Very Important` = 5, `Highly Important` = 6, `Of Supreme Importance` = 7) #STIMULATION (daring, a varied and challenging life, an exciting life)#
#summary(Raw_Study_Two_Data$SVS_4) #Mean 3.9#

Raw_Study_Two_Data$SVS_5 = recode(Raw_Study_Two_Data$SVS_5, `Opposed to my Principles` = 0, `Not At All Important` = 1, `Not Very Important` = 2, `Slightly Important` = 3, `Moderately Important` = 4, `Very Important` = 5, `Highly Important` = 6, `Of Supreme Importance` = 7) #SELF-DIRECTION (creativity, freedom, curiosity, independence, chosing one's own goals)#
#summary(Raw_Study_Two_Data$SVS_5) #Mean 5.3#

Raw_Study_Two_Data$SVS_6 = recode(Raw_Study_Two_Data$SVS_6, `Opposed to my Principles` = 0, `Not At All Important` = 1, `Not Very Important` = 2, `Slightly Important` = 3, `Moderately Important` = 4, `Very Important` = 5, `Highly Important` = 6, `Of Supreme Importance` = 7) #UNIVERSALISM (broadmindedness, beauty of nature and arts, social justice, a world at peace, equality, wisdom, unity with nature, environmental protection)#
#summary(Raw_Study_Two_Data$SVS_6) #Mean 4.9#

Raw_Study_Two_Data$SVS_7 = recode(Raw_Study_Two_Data$SVS_7, `Opposed to my Principles` = 0, `Not At All Important` = 1, `Not Very Important` = 2, `Slightly Important` = 3, `Moderately Important` = 4, `Very Important` = 5, `Highly Important` = 6, `Of Supreme Importance` = 7) #BENEVOLENCE (helpfulness, honesty, forgiveness, loyalty, responsibility)#
#summary(Raw_Study_Two_Data$SVS_7) #Mean 5.4#

Raw_Study_Two_Data$SVS_8 = recode(Raw_Study_Two_Data$SVS_8, `Opposed to my Principles` = 0, `Not At All Important` = 1, `Not Very Important` = 2, `Slightly Important` = 3, `Moderately Important` = 4, `Very Important` = 5, `Highly Important` = 6, `Of Supreme Importance` = 7) #TRADITION (respect for tradition, humbleness, accepting one's portion in life, devotion, modesty)#
#summary(Raw_Study_Two_Data$SVS_8) #Mean 4.2#

Raw_Study_Two_Data$SVS_9 = recode(Raw_Study_Two_Data$SVS_9, `Opposed to my Principles` = 0, `Not At All Important` = 1, `Not Very Important` = 2, `Slightly Important` = 3, `Moderately Important` = 4, `Very Important` = 5, `Highly Important` = 6, `Of Supreme Importance` = 7) #CONFORMITY (obedience, honoring parents and elders, self-discipline, politeness)#
#summary(Raw_Study_Two_Data$SVS_9) #Mean 3.6#

Raw_Study_Two_Data$SVS_10 = recode(Raw_Study_Two_Data$SVS_10, `Opposed to my Principles` = 0, `Not At All Important` = 1, `Not Very Important` = 2, `Slightly Important` = 3, `Moderately Important` = 4, `Very Important` = 5, `Highly Important` = 6, `Of Supreme Importance` = 7) #SECURITY (national security, family security, social order, cleanliness, reciprocation of favors)#
#summary(Raw_Study_Two_Data$SVS_10) #Mean 4.9#


#################################################################Life Orientation Test#########################################################################
##High = Optimistic##
Raw_Study_Two_Data$LOT_1 = recode(Raw_Study_Two_Data$LOT_1, `Strongly Disagree` = 1, `Moderately Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree Nor Disagree` = 4, `Slightly Agree` = 5, `Moderately Agree` = 6, `Strongly Agree` = 7) #In uncertain times, I usually expect the best#
#summary(Raw_Study_Two_Data$LOT_1) #Mean 4.6#

Raw_Study_Two_Data$LOT_2 = recode(Raw_Study_Two_Data$LOT_2, `Strongly Disagree` = 7, `Moderately Disagree` = 6, `Slightly Disagree` = 5, `Neither Agree Nor Disagree` = 4, `Slightly Agree` = 3, `Moderately Agree` = 2, `Strongly Agree` = 1) #REVERSED: If something can go wrong for me, it will#
#summary(Raw_Study_Two_Data$LOT_2) #Mean 4.2#

Raw_Study_Two_Data$LOT_3 = recode(Raw_Study_Two_Data$LOT_3, `Strongly Disagree` = 1, `Moderately Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree Nor Disagree` = 4, `Slightly Agree` = 5, `Moderately Agree` = 6, `Strongly Agree` = 7) #I'm always optimistic about my future#
#summary(Raw_Study_Two_Data$LOT_3) #Mean 4.9#

Raw_Study_Two_Data$LOT_4 = recode(Raw_Study_Two_Data$LOT_4, `Strongly Disagree` = 7, `Moderately Disagree` = 6, `Slightly Disagree` = 5, `Neither Agree Nor Disagree` = 4, `Slightly Agree` = 3, `Moderately Agree` = 2, `Strongly Agree` = 1) #REVERSED: I hardly ever expect things to go my way#
#summary(Raw_Study_Two_Data$LOT_4) #Mean 4.2#

Raw_Study_Two_Data$LOT_5 = recode(Raw_Study_Two_Data$LOT_5, `Strongly Disagree` = 7, `Moderately Disagree` = 6, `Slightly Disagree` = 5, `Neither Agree Nor Disagree` = 4, `Slightly Agree` = 3, `Moderately Agree` = 2, `Strongly Agree` = 1) #REVERSED: I rarely count on good things happening to me#
#summary(Raw_Study_Two_Data$LOT_5) #Mean 4.3#

Raw_Study_Two_Data$LOT_6 = recode(Raw_Study_Two_Data$LOT_6, `Strongly Disagree` = 1, `Moderately Disagree` = 2, `Slightly Disagree` = 3, `Neither Agree Nor Disagree` = 4, `Slightly Agree` = 5, `Moderately Agree` = 6, `Strongly Agree` = 7) #Overall, I expect more good things to happen to me than bad#
#summary(Raw_Study_Two_Data$LOT_6) #Mean 5.1#

#######################################################################ADM###################################################################################################
#Obama#
summary(Raw_Study_Two_Data$ADM_1) #2 NA#
hist(Raw_Study_Two_Data$ADM_1, main = "Barack Obama")

#Trump#
summary(Raw_Study_Two_Data$ADM_2) #2 NA#
hist(Raw_Study_Two_Data$ADM_2, main = "Donald Trump")

#Johnson#
summary(Raw_Study_Two_Data$ADM_3) #172 NA, Mean: -20.1#
hist(Raw_Study_Two_Data$ADM_3, main = "Gary Johnson")

#Bernie#
summary(Raw_Study_Two_Data$ADM_4) #21 NA#
hist(Raw_Study_Two_Data$ADM_4, main = "Bernie Sanders")

#Hilary#
summary(Raw_Study_Two_Data$ADM_5) #5 NA#
hist(Raw_Study_Two_Data$ADM_5, main = "Hilary Clinton")

#Bush#
summary(Raw_Study_Two_Data$ADM_6) #5 NA#
hist(Raw_Study_Two_Data$ADM_6, main = "George Bush")

###################################################################Demographics##############################################################################################
summary(Raw_Study_Two_Data$D1) #Age: Median 33 and Average 37#

Raw_Study_Two_Data$D2 = as.factor(Raw_Study_Two_Data$D2) #Gender#
#questionr::freq(Raw_Study_Two_Data$D2, sort = "dec") #58% Female#

Raw_Study_Two_Data$D3 = as.factor(Raw_Study_Two_Data$D3) #Race#
#questionr::freq(Raw_Study_Two_Data$D3, sort = "dec") #73% White#

Raw_Study_Two_Data$D4 = recode(Raw_Study_Two_Data$D4, `Strongly Liberal` = 1, `Moderately Liberal` = 2, `Slightly Liberal` = 3, `Moderate or Centrist` = 4, `Slightly Conservative` = 5, `Moderately Conservative` = 6, `Strongly Conservative` = 7) #Political Orientation#
#summary(Raw_Study_Two_Data$D4) #Average 3.52#

Raw_Study_Two_Data$D5 = as.factor(Raw_Study_Two_Data$D5) #Poltical Party#
#questionr::freq(Raw_Study_Two_Data$D5, sort = "dec") #47% Democrat, 26% Rep, and 8% Libertarian#

Raw_Study_Two_Data$D6 = recode(Raw_Study_Two_Data$D6, `Less than High School` = 1, `High School or GED` = 2, `Some College or Two Year Degree` = 3, `Bachelor's or Four Year Degree` = 4, `Graduate or Professional Degree` = 5, `Other (Please Specify)` = NULL) #Education#
#summary(Raw_Study_Two_Data$D6) #Mean 3.6# #Mostly Some College or Degree#

Raw_Study_Two_Data$D7 = recode(Raw_Study_Two_Data$D7, `Less than $20,000` = 1, `Between $20,000 and $40,000` = 2, `Between $40,000 and $65,000` = 3, `Between $65,000 and $105,000` = 4, `Between $105,000 and $160,000` = 5, `More than $160,000` = 6) #Income#
#summary(Raw_Study_Two_Data$D7) #Mean: 3.00#

Raw_Study_Two_Data$D8 = recode(Raw_Study_Two_Data$D8, `Poor/Struggling` = 1, `Working Class/Lower Middle Class` = 2, `Middle Class` = 3, `Upper Middle Class` = 4, `Wealthy/Affluent` = 5) #Subjective SES#
#summary(Raw_Study_Two_Data$D8) #Mean: 2.60#

Raw_Study_Two_Data$D9 = recode(Raw_Study_Two_Data$D9, `Yes` = TRUE, `No` = FALSE) #Is US Citizen#
#questionr::freq(Raw_Study_Two_Data$D9) #SIX FALSE to be EXCLUDED#

Raw_Study_Two_Data$D10 = as.factor(Raw_Study_Two_Data$D10) #Region#
#questionr::freq(Raw_Study_Two_Data$D10) #Disproportionate Number from South#

Raw_Study_Two_Data$D11 = as.factor(Raw_Study_Two_Data$D11) #Religious Affiliation#
#questionr::freq(Raw_Study_Two_Data$D11)

Raw_Study_Two_Data$D12 = recode(Raw_Study_Two_Data$D12, `Not At All Religious` = 1, `Not Religious` = 2, `Somewhat Not Religious` = 3, `Neither Very Religious nor Not Religious` = 4, `Somewhat Religious` = 5, `Religious` = 6, `Devoutly Religious` = 7) #Religiousity#
#summary(Raw_Study_Two_Data$D12) #Mean: 3.4; Median: 3#

Raw_Study_Two_Data$D13 = recode(Raw_Study_Two_Data$D13, `Never` = 1, `Seldom` = 2, `Often` = 3, `Very Often` = 4) #Religious Service Attendance#
#summary(Raw_Study_Two_Data$D13) #Mean: 1.9#

Raw_Study_Two_Data$D14 = recode(Raw_Study_Two_Data$D14, `Never` = 1, `Seldom` = 2, `Often` = 3, `Very Often` = 4) #Prayer#
#summary(Raw_Study_Two_Data$D14) #Mean: 2.3#

Raw_Study_Two_Data$D15 = recode(Raw_Study_Two_Data$D15, `Never` = 1, `Seldom` = 2, `Often` = 3, `Very Often` = 4) #Other Church Activities#
#summary(Raw_Study_Two_Data$D15) #Mean 1.7#

Raw_Study_Two_Data$Society_Random_Order = as.factor(Raw_Study_Two_Data$Society_Random_Order) #Random Assignment of Future/Present Variables#
#questionr::freq(Raw_Study_Two_Data$Society_Random_Order) #Roughly Even#

Raw_Study_Two_Data$Survey_Random_Order1 = as.factor(Raw_Study_Two_Data$Survey_Random_Order1) #Random Order of Other Variables#
#questionr::freq(Raw_Study_Two_Data$Survey_Random_Order1) #Roughly Even; Numerous Combinations with Low N's; 2 Levels#

Raw_Study_Two_Data$Survey_Random_Order2 = as.factor(Raw_Study_Two_Data$Survey_Random_Order2) #Random Order of Other Variables#
#questionr::freq(Raw_Study_Two_Data$Survey_Random_Order2) #Roughly Even; Numerous Combinations with Low N's; 24 Levels#
#nlevels(Raw_Study_Two_Data$Survey_Random_Order2) #24 Levels#


###Verify Structure of dataset###
str(Raw_Study_Two_Data, list.len = 140)
#questionr::freq(is.na(Raw_Study_Two_Data[, 23:29]))

##########Manipulation Check Statistics##############
questionr::freq(Raw_Study_Two_Data$Manip_Check_1) #97% Passed#
questionr::freq(Raw_Study_Two_Data$Manip_Check_2) #83% Passed#
questionr::freq(Raw_Study_Two_Data$Manip_Check_3) #96% Passed#
nrow(Raw_Study_Two_Data)


##Create New Logical Variable to Indicate if Passed Manipulation Check One##
Raw_Study_Two_Data = mutate(Raw_Study_Two_Data, Manip_Check_1_Passed = if_else(Manip_Check_1 == 13, 1, 0))
#questionr::freq(Raw_Study_Two_Data$Manip_Check_1_Passed) #487 Passed#
#cor.test(as.numeric(Raw_Study_Two_Data$Manip_Check_1_Passed), Raw_Study_Two_Data$Duration_Secs)


##Create New Logical Variable to Indicate if Passed Manipulation Check Two##
Raw_Study_Two_Data = mutate(Raw_Study_Two_Data, Manip_Check_2_Passed = if_else(Manip_Check_2 == -51, 1, 0))
#questionr::freq(Raw_Study_Two_Data$Manip_Check_2_Passed) #421 Passed#
#cor.test(as.numeric(Raw_Study_Two_Data$Manip_Check_2_Passed), Raw_Study_Two_Data$Duration_Secs)


##Create New Logical Variable to Indicate if Passed Manipulation Check Three##
Raw_Study_Two_Data = mutate(Raw_Study_Two_Data, Manip_Check_3_Passed = if_else(Manip_Check_3 == 67, 1, 0))
#questionr::freq(Raw_Study_Two_Data$Manip_Check_3_Passed) #483 Passed#
#cor.test(as.numeric(Raw_Study_Two_Data$Manip_Check_3_Passed), Raw_Study_Two_Data$Duration_Secs)


##Passed all Three Attention Checks##
Raw_Study_Two_Data = mutate(Raw_Study_Two_Data, PASSED_ALL_ATTN = if_else(Manip_Check_3_Passed == 1 & Manip_Check_2_Passed == 1 & Manip_Check_1_Passed == 1, 1, 0))
#questionr::freq(Raw_Study_Two_Data$PASSED_ALL_ATTN) #399 (79.2%) Passed all Three#


############################################################################################################################################################################
#####################################################################Missing Data Random Forest Model#######################################################################
############################################################################################################################################################################


Missing_Data_RF_FRAME <- dplyr::select(Raw_Study_Two_Data, Present_1, Present_2, Present_3, Present_4, Present_5, Present_6, Manip_Check_1_Passed, Present_First_Click, Present_Last_Click, Present_Page_Submit, Present_Click_Count, Best_Future_1, Best_Future_2, Best_Future_3, Best_Future_4,
Best_Future_5, Best_Future_6, Best_Future_Likelihood, Manip_Check_2_Passed, Best_Future_First_Click, Best_Future_Last_Click, Best_Future_Page_Submit, Best_Future_Click_Count, Worst_Future_1, Worst_Future_2, Worst_Future_3, Worst_Future_4, Worst_Future_5, Worst_Future_6, Worst_Future_Likelihood,
Manip_Check_3_Passed, Worst_Future_First_Click, Worst_Future_Last_Click, Worst_Future_Page_Submit, Worst_Future_Click_Count, PASSED_ALL_ATTN, FWB_1, FWB_2, FWB_3, FWB_4, FWB_5, FWB_6, FWB_7, FWB_8, PIP_1,	PIP_2,	PIP_3,	PIP_4,	PIP_5,	PIP_6,	PIP_7,	PIP_8,	PIP_9,	PIP_10,	PIP_11,
PIP_12,	PIP_13,	PIP_14,	PIP_15,	PIP_16,	PIP_17,	PIP_18,	PIP_19,	PIP_20,	PIP_21,	PIP_22,	PIP_23,	PIP_24,	PIP_25,	PIP_26, PIP_27, PIP_28, PIP_29, PIP_30, ADM_1, ADM_2, ADM_3, ADM_4, ADM_5, ADM_6, Gov_1, Gov_2, Gov_3, Gov_4, Gov_5, Gov_6, LOT_1, LOT_2, LOT_3, LOT_4, LOT_5, LOT_6, SVS_1, SVS_2, SVS_3, SVS_4, SVS_5, SVS_6, SVS_7, SVS_8, SVS_9, SVS_10,
D1, D4, D6, D7, D8, D12, D13, D14, D15)

##Check All Numeric##
str(Missing_Data_RF_FRAME)


##Number of Missing Values in Frame##
questionr::freq(is.na(Missing_Data_RF_FRAME)) #1335/55944 = 2.4%#
ncol(Missing_Data_RF_FRAME)


####################################################################Test Model#########################################################
set.seed(18)
No_Missing_RF_FRAME = as.data.frame(na.omit(Missing_Data_RF_FRAME))
questionr::freq(is.na(No_Missing_RF_FRAME))

New_Missing_RF_Frame = prodNA(No_Missing_RF_FRAME, noNA = 0.024)
questionr::freq(is.na(New_Missing_RF_Frame))
str(New_Missing_RF_Frame, list.len = 110)

nrow(New_Missing_RF_Frame)


##Removing NAs these frames are the same##
#mean(No_Missing_RF_FRAME == New_Missing_RF_Frame, na.rm = TRUE)

####Parallel Computing to Speed up Processing of Random Forest#####
detectCores() #Number of Cores on Computer#
registerDoParallel(cores = detectCores() - 1) #Use all cores on Computer minus
#foreach(i=1:3) %dopar% sqrt(i)
#?missForest


###Random Forest##
#Accuracy_Model = missForest(New_Missing_RF_Frame, maxiter = 100, ntree = 100, variablewise = TRUE, xtrue = No_Missing_RF_FRAME)
#MSE_by_Variable <- Accuracy_Model$OOBerror #Mean Square Error by Variable# #Higher Indicates more Error#
#RF Imputed#
#Test_Imputed = as.tbl(round(as.data.frame(Accuracy_Model$ximp)))
#str(Test_Imputed)

###K Nearest Neigbors Imputation###
KNN_Imputation_Model <- knnImputation(New_Missing_RF_Frame, k = 10, scale = T)

##Mediant Imputation##
Median_Imputation <- imputeMissings::impute(New_Missing_RF_Frame, flag = FALSE)
#RF_Imputation <- imputeMissings::impute(New_Missing_RF_Frame, method = "randomForest", flag = FALSE)


#mean(round(No_Missing_RF_FRAME) == Test_Imputed)
#nrmse(round(Accuracy_Model$ximp), round(New_Missing_RF_Frame), xtrue = round(No_Missing_RF_FRAME))

#mean(round(KNN_Imputation_Model) == Test_Imputed)
nrmse(KNN_Imputation_Model, round(New_Missing_RF_Frame), xtrue = round(No_Missing_RF_FRAME))

#mean(round(Median_Imputation) == Test_Imputed)
nrmse(Median_Imputation, round(New_Missing_RF_Frame), xtrue = round(No_Missing_RF_FRAME))


#regr.eval(trues = No_Missing_RF_FRAME, preds = Test_Imputed) ##Random Forest##
#regr.eval(RF_Imputation, No_Missing_RF_FRAME)
regr.eval(trues = No_Missing_RF_FRAME, preds = KNN_Imputation_Model) ##KNN##
regr.eval(trues = No_Missing_RF_FRAME, preds= Median_Imputation) ##Median/Mode##

#warnings() #Can only include numeric and logical variables#


###############################################################Random Forest Model to Predict Missing Numeric values############################################################
##DMwR Could also be used for a Knn Imputation Method##
set.seed(18) #For Reproducability#
Missing_Data_RF_Model <- missForest(as.matrix(Missing_Data_RF_FRAME), maxiter = 100, ntree = 100, variablewise = FALSE, parallelize = 'variables')
Missing_Data_RF_Model$OOBerror #Normalized Root Mean Squared Error to computer percentage out of bag error#

#microbenchmark(missForest(as.matrix(Missing_Data_RF_FRAME), maxiter = 100, ntree = 100, variablewise = FALSE, parallelize = 'variables'), times = 5)
#102.2251 Mean time in seconds to run with Parallel

Imputed_Data <- as.tbl(round(as.data.frame(Missing_Data_RF_Model$ximp))) #Assign to tibble object with imputation#
table(is.na(Imputed_Data)) #No Missing Data Points#
str(Imputed_Data) #See Data Structure#
class(Imputed_Data) #Is Tibble


############################################################################Rebuild Dataset with Imputed Data##################################################################
#which(colnames(Raw_Study_Two_Data)=="Best_Future_1")
#which(colnames(Imputed_Data)=="D12")

#str(Imputed_Data, list.len = 110)
Imputed_Raw_Data_Study_Two <- cbind(Raw_Study_Two_Data[, 1:11], Imputed_Data[,1:103], Raw_Study_Two_Data[, 114:116], Imputed_Data[,104], Raw_Study_Two_Data[, 115:116], Imputed_Data[,108], Raw_Study_Two_Data[, 118:119], Imputed_Data[,105], Raw_Study_Two_Data[, 121], Imputed_Data[106:107], Raw_Study_Two_Data[, 124:128], Imputed_Data[,108:111], Raw_Study_Two_Data[, 132:137])
str(Imputed_Raw_Data_Study_Two, list.len = 150)


#####Create Subset for all participants who passed ATTN Checks#####
##Use dplyr::filter##
Passed_Study_Two_Data = subset(Imputed_Raw_Data_Study_Two, PASSED_ALL_ATTN == 1 & D9 == "TRUE")
summary(Passed_Study_Two_Data$Duration_Secs/60)
#str(Passed_Study_Two_Data, list.len = 150) #395 Cases#

#############################################################################################################################################
####################################################Scale escriptives########################################################################
#############################################################################################################################################


##################################################Chronbach's Alpha Reliability Assessment##############################################
##Use dplyr::select to subset of scale items##
Passed_FWB_Items = dplyr::select(Passed_Study_Two_Data, FWB_1, FWB_2, FWB_3, FWB_4, FWB_5, FWB_6, FWB_7, FWB_8)
Passed_Political_Items = dplyr::select(Passed_Study_Two_Data, PIP_1,	PIP_2,	PIP_3,	PIP_4,	PIP_5,	PIP_6,	PIP_7,	PIP_8,	PIP_9,	PIP_10,	PIP_11, PIP_12,	PIP_13,	PIP_14,	PIP_15,	PIP_16,	PIP_17,	PIP_18,	PIP_19,	PIP_20,	PIP_21, PIP_22,	PIP_23,	PIP_24,	PIP_25,	PIP_26, PIP_27, PIP_28, PIP_29, PIP_30) #
Passed_Gov_Items = dplyr::select(Passed_Study_Two_Data, Gov_1, Gov_2, Gov_3, Gov_4, Gov_5, Gov_6)
Passed_SVS = dplyr::select(Passed_Study_Two_Data, SVS_1, SVS_2, SVS_3, SVS_4, SVS_5, SVS_6, SVS_7, SVS_8, SVS_9, SVS_10)
Passed_LOT = dplyr::select(Passed_Study_Two_Data, LOT_1, LOT_2, LOT_3, LOT_4, LOT_5, LOT_6)
Passed_Present <- dplyr::select(Passed_Study_Two_Data, Present_1, Present_2, Present_3, Present_4, Present_5, Present_6)
Passed_Best_Future <- dplyr::select(Passed_Study_Two_Data, Best_Future_1, Best_Future_2, Best_Future_3, Best_Future_4, Best_Future_5, Best_Future_6)
Passed_Worst_Future <- dplyr::select(Passed_Study_Two_Data, Worst_Future_1, Worst_Future_2, Worst_Future_3, Worst_Future_4, Worst_Future_5, Worst_Future_6)
Passed_ADM_Ratings <- dplyr::select(Passed_Study_Two_Data, ADM_1, ADM_2, ADM_3, ADM_4, ADM_5, ADM_6)
Passed_Self_Enhancement <- dplyr::select(Passed_Study_Two_Data, SVS_1, SVS_2)
Passed_Openness_to_Change <- dplyr::select(Passed_Study_Two_Data, SVS_3, SVS_4, SVS_5)
Passed_Self_Transcendence <- dplyr::select(Passed_Study_Two_Data, SVS_6, SVS_7)
Passed_Conservation <- dplyr::select(Passed_Study_Two_Data, SVS_8, SVS_9, SVS_10)


##psych::alpha to compute Cronbach's Alpha and GUttman's Lambda 6##
psych::alpha(Passed_FWB_Items) #FWB 0.9 Alpha#
psych::alpha(Passed_LOT) #PHNS 0.88 Alpha#
psych::alpha(Passed_Present) #Present 0.91 Alpha#
psych::alpha(Passed_Best_Future) #Best Future 0.94 Alpha#
psych::alpha(Passed_Worst_Future) #Worst Future 0.91 Alpha#
#psych::alpha(Passed_Gov_Items) #Worst Future 0.91 Alpha#


##########################################################Compute Scale Means######################################################################
#Use dplyr::mutate#

##Free Will Beliefs Scale##
Passed_Study_Two_Data = dplyr::mutate(Passed_Study_Two_Data, FWB = (FWB_1 + FWB_2 + FWB_3 + FWB_4 + FWB_5 + FWB_6 + FWB_7 + FWB_8)/8)
summary(Passed_Study_Two_Data$FWB) #Mean: 5.2#


##Life Orientation Test##
Passed_Study_Two_Data = dplyr::mutate(Passed_Study_Two_Data, LOT = (LOT_1 + LOT_2 + LOT_3 + LOT_4 + LOT_5 + LOT_6)/6)
summary(Passed_Study_Two_Data$LOT) #Mean: 4.5#


###############################################################Correlations##################################################################################################################################
Descriptive_Corr = Passed_Study_Two_Data %>%
  dplyr::select(FWB, D4, LOT, Gov_1, Gov_2, Gov_3, Gov_4, Gov_5, Gov_6, ADM_1, ADM_2, ADM_3, ADM_4, ADM_5, ADM_6, D12, D13, D14, D15, SVS_1, SVS_2, SVS_3, SVS_4, SVS_5, SVS_6, SVS_7, SVS_8, SVS_9, SVS_10)

#Scale_Correlation = corr.test(Descriptive_Corr)
#print(Scale_Correlation)
#corrplot(corr = Scale_Correlation$r, method = "number", type = "lower", p.mat = Scale_Correlation$p, sig.level = 0.5, insig = "pch")

Scale_correlation2 = as.tbl(corstars(Descriptive_Corr))
#Scale_Correlation$r
#Scale_Correlation$p


###############################################################################################################################
##############################################Confirmatory Factor Analysis######################################################
###############################################################################################################################
Present_CFA_Model <- 'Present =~ Present_1 + Present_2 + Present_3 + Present_4 + Present_5 + Present_6'
Best_Future_CFA_Model <- 'Best_Future =~ Best_Future_1 + Best_Future_2 + Best_Future_3 + Best_Future_4 + Best_Future_5 + Best_Future_6'
Worst_Future_CFA_Model <- 'Worst_Future =~ Worst_Future_1 + Worst_Future_2 + Worst_Future_3 + Worst_Future_4 + Worst_Future_5 + Worst_Future_6'


##Present CFA##
CFA_Present <- sem(Present_CFA_Model, data = Passed_Study_Two_Data)
summary(CFA_Present)
fitMeasures(CFA_Present, c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"))
semPaths(CFA_Present, "std", curvePivot = TRUE, layout = "tree2")


##Best_Future CFA##
CFA_Best_Future <- sem(Best_Future_CFA_Model, data = Passed_Study_Two_Data)
summary(CFA_Best_Future)
fitMeasures(CFA_Best_Future, c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic", "pclose"))
semPaths(CFA_Best_Future, "std", curvePivot = TRUE, layout = "tree2")


##Worst_Future CFA##
CFA_Worst_Future <- sem(Worst_Future_CFA_Model, data = Passed_Study_Two_Data)
summary(CFA_Worst_Future)
fitMeasures(CFA_Worst_Future, c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"))
semPaths(CFA_Worst_Future, "std", curvePivot = TRUE, layout = "tree2")


###############################################################################################################################
##############################################Exploratory Factor Analysis######################################################
###############################################################################################################################
#use fa() from package 'psych'##

##Verify Structure##
str(Passed_Study_Two_Data, list.len = 150)

###Create Scree Plots with Eigen Values for Each Grouping###
Present_Eigen_Scree <- psych::scree(as.matrix(Passed_Present), main = "Present Scree Plot") #One Solution Indicated#
Best_Future_Eigen_Scree <- psych::scree(as.matrix(Passed_Best_Future), main = "Best Future Scree Plot") #One Solution Indicated#
Worst_Future_Eigen_Scree <- psych::scree(as.matrix(Passed_Worst_Future), main = "Worst Future Scree Plot") #One Solution Indicated#

par(mfrow = c(1, 2))
PIP_Eigen_Scree <- psych::scree(as.matrix(Passed_Political_Items), main = "Political Policies Scree Plot", factors = FALSE) #Six Solution Indicated#
#Gov_Eigen_Scree <- psych::scree(as.matrix(Passed_Gov_Items), main = "Government Trust Scree Plot", factors = FALSE) #Two Solution Indicated#
SVS_Eigen_Scree <- psych::scree(as.matrix(Passed_SVS), main = "Schwartz Values Scree Plot", factors = FALSE) #Three Solution Indicated#
par(mfrow = c(1, 1))

ADM_Eigen_Scree <- psych::scree(as.matrix(Passed_ADM_Ratings), main = "Political_Figures Scree Plot") #Three Solution Indicated#



###Very Simple Structure###
#Present_VSS = vss(as.matrix(Passed_Present), n=3, rotate = "Promax", fm = "ml")#Very simple structure#
#Present_VSS #Contains Fit Statistics#
#Best_Future_VSS = vss(as.matrix(Passed_Best_Future), n=3, rotate = "Promax", fm = "ml") #Very simple structure#
#Best_Future_VSS #Contains Fit Statistics#
#Worst_Future_VSS = vss(as.matrix(Passed_Worst_Future), n=3, rotate = "Promax", fm = "ml") #Very simple structure#
#Worst_Future_VSS #Contains Fit Statistics#

#PIP_VSS = vss(as.matrix(Passed_Political_Items), n=6, rotate = "Promax", fm = "ml") #Very simple structure#


###Extract One Factor Solutions###
Present_Factor_Analysis <- fa(Passed_Present, nfactors = 1, fm = "ml", alpha = .08, rotate = "Promax")
Best_Future_Factor_Analysis <- fa(Passed_Best_Future, nfactors = 1, fm = "ml", alpha = .08, rotate = "Promax")
Worst_Future_Factor_Analysis <- fa(Passed_Worst_Future, nfactors = 1, fm = "ml", alpha = .08, rotate = "Promax")


##Political Policies##
PIP_Factor_Analysis <- fa(Passed_Political_Items, nfactors = 6, fm = "ml", alpha = .08, rotate = "Promax")
#PIP_Factor_Analysis <- fa(Passed_Political_Items, nfactors = 4, fm = "ml", alpha = .08, rotate = "Promax")
PIP_Pattern = PIP_Factor_Analysis$loadings
PIP_Pattern_Sig = abs(PIP_Factor_Analysis$loadings) >= 0.4

#write.csv(PIP_Pattern, "C:/Users/bmoli/Documents/Dissertation/PIP Rotated Factor Matrix.csv", na = "")


##Gov Trust##
Gov_Factor_Analysis <- fa(Passed_Gov_Items, nfactors = 2, fm = "ml", alpha = .08, rotate = "Promax")
Gov_Pattern = Gov_Factor_Analysis$loadings
Gov_Pattern_Sig = abs(Gov_Factor_Analysis$loadings) >= 0.4

#One Factor#
#One_Gov_Factor_Analysis <- fa(Passed_Gov_Items, nfactors = 1, fm = "ml", alpha = .08, rotate = "Promax")


##SVS Values##
#SVS_Factor_Analysis <- fa(Passed_SVS, nfactors = 3, fm = "ml", alpha = .08, rotate = "Promax")
#SVS_Pattern = SVS_Factor_Analysis$loadings
#SVS_Pattern_Sig = abs(SVS_Factor_Analysis$loadings) >= 0.4
#write.csv(SVS_Pattern, "C:/Users/bmoli/Documents/Dissertation/SVS Rotated Factor Matrix.csv", na = "")

##ADM Figures##
ADM_Factor_Analysis <- fa(Passed_ADM_Ratings, nfactors = 1, fm = "ml", alpha = .08, rotate = "Promax")
ADM_Pattern = ADM_Factor_Analysis$loadings
#ADM_Pattern_Sig = abs(ADM_Factor_Analysis$loadings) >= 0.4

corstars(Passed_ADM_Ratings)
summary(Passed_ADM_Ratings)


#############################################################Political Issues and Policies Subscales##########################################
##Break Into Designated Subsets by Factor Analysis##
Core_Conservatism_Items <- dplyr::select(Passed_Study_Two_Data, PIP_3, PIP_6, PIP_9, PIP_10, PIP_11, PIP_12, PIP_25, PIP_26, PIP_30)
Social_Safety_Net_Items <- dplyr::select(Passed_Study_Two_Data, PIP_5, PIP_15, PIP_20, PIP_21, PIP_22, PIP_27)
Liberal_Social_Items <- Passed_Study_Two_Data %>% dplyr::select(PIP_1, PIP_4, PIP_7, PIP_28) %>% mutate(PIP_4 = -1 * PIP_4) ##Four Reverse Scored##
Socialism_Items <- Passed_Study_Two_Data %>% dplyr::select(PIP_2, PIP_17, PIP_23, PIP_29) %>% mutate(PIP_2 = -1 * PIP_2) ##Two Reverse Scored##
Anti_Trump_Globalism_Items <- Passed_Study_Two_Data %>% dplyr::select(PIP_13, PIP_14, PIP_18, PIP_19) %>% mutate(PIP_14 = -1 * PIP_14)


##psych::alpha to compute Cronbach's Alpha and GUttman's Lambda 6##
psych::alpha(Core_Conservatism_Items) #FWB 0.82 Alpha#
psych::alpha(Social_Safety_Net_Items) #FWB 0.82 Alpha#
psych::alpha(Liberal_Social_Items) #FWB 0.80 Alpha#
psych::alpha(Socialism_Items) #FWB 0.80 Alpha#
psych::alpha(Anti_Trump_Globalism_Items) #FWB 0.77 Alpha#


###Core Conservative Average Calculation###
Passed_Study_Two_Data = mutate(Passed_Study_Two_Data, Core_Conservatism = (PIP_3 + PIP_6 + PIP_9 + PIP_10 + PIP_11 + PIP_12 + PIP_25 + PIP_26 + PIP_30)/9)
summary(Passed_Study_Two_Data$Core_Conservatism) #Mean: 8.8#

Passed_Study_Two_Data = mutate(Passed_Study_Two_Data, Support_Social_Safety_Net = (PIP_5 + PIP_15 + PIP_20 + PIP_21 + PIP_22 + PIP_27)/6)
summary(Passed_Study_Two_Data$Support_Social_Safety_Net) #Mean: 34.3#

Passed_Study_Two_Data = mutate(Passed_Study_Two_Data, Liberal_Social_Values = (PIP_1 + (-1 * PIP_4) + PIP_7 + PIP_28)/4)
summary(Passed_Study_Two_Data$Liberal_Social_Values) #Mean: 24.6#

Passed_Study_Two_Data = mutate(Passed_Study_Two_Data, Socialism = ((-1 * PIP_2) + PIP_17 + PIP_23 + PIP_29)/4)
summary(Passed_Study_Two_Data$Socialism) #Mean: -12.4#

Passed_Study_Two_Data = mutate(Passed_Study_Two_Data, Anti_Trump_Globalism = (PIP_13 + (-1 * PIP_14) + PIP_18 + PIP_19)/4)
summary(Passed_Study_Two_Data$Anti_Trump_Globalism) #Mean: 16.0#


#############################################################Government Trust and Effectiveness Subscales##########################################
##Break Into Designated Subsets by Factor Analysis##
Effective_Government_Items <- Passed_Study_Two_Data %>% dplyr::select(Gov_1, Gov_2, Gov_4, Gov_5) %>% mutate(Gov_1 = -1*Gov_1)
Destructive_Government_Items <- Passed_Study_Two_Data %>% dplyr::select(Gov_3, Gov_6)
#Passed_Gov_Items <- mutate(Passed_Gov_Items, Gov_1 = -1*Gov_1)

##psych::alpha to compute Cronbach's Alpha and GUttman's Lambda 6##
#psych::alpha(Effective_Government_Items) #FWB 0.74 Alpha#
psych::alpha(Passed_Gov_Items) #0.71 Alpha#
#psych::alpha(Destructive_Government_Items)
corr.test(Passed_Gov_Items) ##r = 0.37**##


Passed_Study_Two_Data = dplyr::mutate(Passed_Study_Two_Data, Gov_Trust = (Gov_1 + Gov_2 + Gov_3 + Gov_4 + Gov_5 + Gov_6)/6)
#summary(Passed_Study_Two_Data$Gov_Trust)

############################################################Schwartz Values Survey Subscales#######################################################
#SVS_Factor_One <- dplyr::select(Passed_Study_Two_Data, SVS_1, SVS_2, SVS_3, SVS_4)
#SVS_Factor_Two <- dplyr::select(Passed_Study_Two_Data, SVS_5, SVS_6, SVS_7)
#SVS_Factor_Three <- dplyr::select(Passed_Study_Two_Data, SVS_8, SVS_9, SVS_10)


##psych::alpha to compute Cronbach's Alpha and GUttman's Lambda 6##
psych::alpha(Passed_Self_Enhancement) # 0.57 Alpha#
psych::alpha(Passed_Openness_to_Change) # 0.59 Alpha#
psych::alpha(Passed_Self_Transcendence) # 0.66 Alpha#
psych::alpha(Passed_Conservation) # 0.78 Alpha#
psych::alpha(Passed_SVS) # 0.72 Alpha#


###Compute Variables###
Passed_Study_Two_Data = dplyr::mutate(Passed_Study_Two_Data, Self_Enhancement = (SVS_1 + SVS_2)/2)
#summary(Passed_Study_Two_Data$Self_Enhancement) #Mean: 3.88#
Passed_Study_Two_Data = dplyr::mutate(Passed_Study_Two_Data, Openness_to_Change = (SVS_3 + SVS_4 + SVS_5)/3)
#summary(Passed_Study_Two_Data$Openness_to_Change) #Mean: 4.25#
Passed_Study_Two_Data = dplyr::mutate(Passed_Study_Two_Data, Self_Transcendence = (SVS_6 + SVS_7)/2)
#summary(Passed_Study_Two_Data$Self_Transcendence) #Mean: 5.18#
Passed_Study_Two_Data = dplyr::mutate(Passed_Study_Two_Data, Conservation = (SVS_8 + SVS_9 + SVS_10)/3)
#summary(Passed_Study_Two_Data$Conservation) #Mean: 4.24#


####################################################################ADM Difference###################################################################
Obama_v_Trump = Passed_ADM_Ratings %>% dplyr::select(ADM_1, ADM_2) %>% mutate(ADM_1 = -1*ADM_1)

##psych::alpha to compute Cronbach's Alpha and GUttman's Lambda 6##
psych::alpha(Obama_v_Trump) #0.80 Alpha#

Passed_Study_Two_Data = mutate(Passed_Study_Two_Data, Current_Versus_Previous_ADM = ADM_2 - ADM_1)
#summary(Passed_Study_Two_Data$Current_Versus_Previous_ADM) #Mean: -65.65#

#######################################################Compute Composite Variables for Present and Futures###########################################
Passed_Study_Two_Data = dplyr::mutate(Passed_Study_Two_Data, Present = (Present_1 + Present_2 + Present_3 + Present_4 + Present_5 + Present_6)/6)
#summary(Passed_Study_Two_Data$Present) #Mean: 17.68#
Passed_Study_Two_Data = dplyr::mutate(Passed_Study_Two_Data, Best_Future = (Best_Future_1 + Best_Future_2 + Best_Future_3 + Best_Future_4 + Best_Future_5 + Best_Future_6)/6)
#summary(Passed_Study_Two_Data$Best_Future) #Mean: 63.77#
Passed_Study_Two_Data = dplyr::mutate(Passed_Study_Two_Data, Worst_Future = (Worst_Future_1 + Worst_Future_2 + Worst_Future_3 + Worst_Future_4 + Worst_Future_5 + Worst_Future_6)/6)
#summary(Passed_Study_Two_Data$Worst_Future) #Mean: -53.35#


##TO BE REMOVED: Number of People where the Worst Future is Better than the Best##
questionr::freq(Passed_Study_Two_Data$Worst_Future > Passed_Study_Two_Data$Best_Future)
##Number of People where the present is higher than the best possible future##
questionr::freq(Passed_Study_Two_Data$Present > Passed_Study_Two_Data$Best_Future)


###Drop Those lower whose best future is lower than the worst###
Passed_Study_Two_Data = dplyr::filter(Passed_Study_Two_Data, Worst_Future < Best_Future)
#dim(Passed_Study_Two_Data) #375 Final Participants (74% of beginning)#


#####Compute Difference Variables#####
Passed_Study_Two_Data = dplyr::mutate(Passed_Study_Two_Data, Positive_Potential = Best_Future - Present) #Positive Potential: Best Future Minus Present#
#summary(Passed_Study_Two_Data$Positive_Potential) #Mean: 48.75#
##Negative Reversed to be on a similar scale: HIGHER NUMBERS EQUAL MORE NEGATIVE POTENTIAL##
Passed_Study_Two_Data = dplyr::mutate(Passed_Study_Two_Data, Negative_Potential = -1*(Worst_Future - Present)) #Negative Potential: Worst Future Minus Present#
#summary(Passed_Study_Two_Data$Negative_Potential) #Mean: -75.67#
Passed_Study_Two_Data = dplyr::mutate(Passed_Study_Two_Data, Absolute_Potential = abs(Best_Future - Worst_Future)) #Absolute Potential: Best Future - Worst#
#summary(Passed_Study_Two_Data$Absolute_Potential) #Mean: 124.4#


##Number of people with more Positive than absolue potential##
questionr::freq(Passed_Study_Two_Data$Positive_Potential > Passed_Study_Two_Data$Absolute_Potential) #14#
##Number of people with more Negative than absolue potential##
questionr::freq(Passed_Study_Two_Data$Negative_Potential < Passed_Study_Two_Data$Absolute_Potential) #27#


###############################################################################################################################################################
#############################################################Normality Assumptions/Analysis####################################################################
###############################################################################################################################################################
#####Density Plots#####
hist(Passed_Study_Two_Data$FWB, main = "Free Will Histogram")
#hist(sqrt(Passed_Study_Two_Data$FWB))
plot(density(Passed_Study_Two_Data$FWB), main = "Free Will Density")
psych::describe(Passed_Study_Two_Data$FWB)

hist(Passed_Study_Two_Data$D4, main = "Political Orientation Histogram")
plot(density(Passed_Study_Two_Data$D4), main = "Political Orientation Density")
psych::describe(Passed_Study_Two_Data$D4)

hist(Passed_Study_Two_Data$Best_Future, main = "Best Future Histogram")
#hist(sqrt(Passed_Study_Two_Data$Best_Future))
plot(density(Passed_Study_Two_Data$Best_Future), main = "Best Future Density")
psych::describe(Passed_Study_Two_Data$Best_Future)

hist(Passed_Study_Two_Data$Worst_Future, main = "Worst Future Histogram")
plot(density(Passed_Study_Two_Data$Worst_Future), main = "Worst Future Density")
psych::describe(Passed_Study_Two_Data$Worst_Future)

hist(Passed_Study_Two_Data$Present, main = "Present Histogram")
plot(density(Passed_Study_Two_Data$Present), main = "Present Density")
psych::describe(Passed_Study_Two_Data$Present)

hist(Passed_Study_Two_Data$Positive_Potential, main = "Positive Potential Histogram")
plot(density(Passed_Study_Two_Data$Positive_Potential), main = "Positive Potential Density")
psych::describe(Passed_Study_Two_Data$Positive_Potential)

hist(Passed_Study_Two_Data$Negative_Potential, main = "Negative Potential Histogram")
plot(density(Passed_Study_Two_Data$Negative_Potential), main = "Negative Potential Density")
psych::describe(Passed_Study_Two_Data$Negative_Potential)

hist(Passed_Study_Two_Data$Absolute_Potential, main = "Absolute Potential Histogram")
plot(density(Passed_Study_Two_Data$Absolute_Potential), main = "Absolute Potential Density")
psych::describe(Passed_Study_Two_Data$Absolute_Potential)

hist(Passed_Study_Two_Data$Current_Versus_Previous_ADM, main = "Current Versus Previous President Histogram")
psych::describe(Passed_Study_Two_Data$Current_Versus_Previous_ADM)

hist(Passed_Study_Two_Data$LOT, main = "Trait Optimism")
plot(density(Passed_Study_Two_Data$LOT))
psych::describe(Passed_Study_Two_Data$LOT)


############Create Transformed Variables##################
#?transformTukey #Tukeys Ladder of Powers#

##Transform Skewed Free Will Beliefs##
Passed_Study_Two_Data$TR_FWB <- transformTukey(Passed_Study_Two_Data$FWB)
hist(Passed_Study_Two_Data$TR_FWB)
plot(density(Passed_Study_Two_Data$TR_FWB))

##Transform the very Skewed Best Future##
Passed_Study_Two_Data$TR_Best_Future <- recode(Passed_Study_Two_Data$Best_Future, `0` = 0.00001)
Passed_Study_Two_Data$TR_Best_Future <- transformTukey(Passed_Study_Two_Data$TR_Best_Future)
questionr::freq(is.nan(Passed_Study_Two_Data$TR_Best_Future))
hist(Passed_Study_Two_Data$TR_Best_Future)
plot(density(Passed_Study_Two_Data$TR_Best_Future))

##Transform the very Skewed Worst Future##
Passed_Study_Two_Data$TR_Worst_Future <- recode(Passed_Study_Two_Data$Worst_Future, `0` = 0.00001)
Passed_Study_Two_Data$TR_Worst_Future <- transformTukey(Passed_Study_Two_Data$TR_Worst_Future, start = -30, end = 30)
questionr::freq(is.nan(Passed_Study_Two_Data$TR_Worst_Future))
hist(Passed_Study_Two_Data$TR_Worst_Future)
plot(density(Passed_Study_Two_Data$TR_Worst_Future))


#######################Correlations###################################
Descriptive_Corr_2 = Passed_Study_Two_Data %>%
  dplyr::select(`Conservative Political Orientation` = D4, `Free Will Beliefs` = FWB, Present, `Best Future` = Best_Future, `Worst Future` = Worst_Future, `Best Future Likelihood` = Best_Future_Likelihood, `Worst Future Likelihood` = Worst_Future_Likelihood, `Positive Potential` = Positive_Potential, `Negative Potential` = Negative_Potential, `Absolute Potential` = Absolute_Potential, `Core Conservatism` = Core_Conservatism, `Support for Social Safety Net` = Support_Social_Safety_Net, `Liberal Social Values` = Liberal_Social_Values, Socialism, `Anti Trump Globalism` = Anti_Trump_Globalism, `Self Enhancement` = Self_Enhancement, `Openness to Change` = Openness_to_Change, `Self-Transcendence` = Self_Transcendence, `Conservation` = Conservation, `Trust in Government` = Gov_Trust, `Trait Optimism` = LOT, `Age` = D1, `Religiousity` = D12)
#`Drug Legalization` = PIP_8, PIP_16, PIP_24,

Descriptives = psych::describe(Descriptive_Corr_2 )

#write.csv(Descriptives, "C:/Users/bmoli/Documents/Dissertation/Study Two Descriptive Statistics.csv", na = "")

##Pearson Correlations##
Full_Correlation = as.tbl(corstars(Descriptive_Corr_2))
class(Full_Correlation)



##Spearman Correlations##
Spearman_Full_Correlation = as.tbl(spearman_corstars(Descriptive_Corr_2))
class(Spearman_Full_Correlation)

levels(Passed_Study_Two_Data$Survey_Random_Order1)

##Controlling for order does not make FWB Significant##
corr.test(Passed_Study_Two_Data %>% 
            dplyr::filter(Survey_Random_Order1 == "FreeWillSurvey|PoliticalIssues/Policies") %>%
            dplyr::select(Best_Future, Worst_Future, FWB, D4))

corr.test(Passed_Study_Two_Data %>% 
            dplyr::filter(FWB > 3) %>%
            dplyr::select(Best_Future, Worst_Future, FWB, D4))

hist(Passed_Study_Two_Data$TR_FWB)

cor.test(Passed_Study_Two_Data$TR_FWB, Passed_Study_Two_Data$Best_Future)


Means_by_PO_for_Subscales = Passed_Study_Two_Data %>%
  mutate(Short_Political_Orientation = recode(D4, `1` = "Liberal", `2` = "Liberal", `3` = "Moderate", `4` = "Moderate", `5` = "Moderate", `6` = "Conservative", `7` = "Conservative")) %>%
  group_by(Short_Political_Orientation) %>%
  summarise(N = n(), Mean_Core_Conservatism = mean(Core_Conservatism), Mean_Safety_Net = mean(Support_Social_Safety_Net), Mean_Liberal_Values = mean(Liberal_Social_Values), Mean_Socialism = mean(Socialism), Mean_Globalism = mean(Anti_Trump_Globalism), FWB = mean(FWB), Present = mean(Present), Best_Future = mean(Best_Future), Worst_Future = mean(Worst_Future), Trump_v_Obama = mean(Current_Versus_Previous_ADM), Gary_Johnson = mean(ADM_3))


######################Visual Aides###################################

GG_FWB_PO_POS_PO = ggplot(Passed_Study_Two_Data, aes(x=FWB, y=Positive_Potential)) + geom_jitter(aes(col = D4), size = 3) + geom_smooth(method = "lm") +
  scale_color_continuous(name = "Political Orientation", labels = c("Strongly Liberal", "Moderately Liberal", "Slightly Liberal", "Moderate", "Slightly Conservative", "Moderately Conservative", "Strongly Conservative"), low = "blue", high = "red") + 
  ggtitle("Free Will, Political Orientation, and Positive Potential") + xlab("Free Will Beliefs") + ylab("Positive Potential") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text=element_text(size=12,face = "bold"), axis.title=element_text(size=14,face = "bold"), legend.text=element_text(size=7), legend.title = element_text(size=10))

GG_FWB_PO_NEG_PO = ggplot(Passed_Study_Two_Data, aes(x=FWB, y=Negative_Potential)) + geom_jitter(aes(col = D4), size = 3) + geom_smooth(method = "lm") +
  scale_color_continuous(name = "Political Orientation", labels = c("Strongly Liberal", "Moderately Liberal", "Slightly Liberal", "Moderate", "Slightly Conservative", "Moderately Conservative", "Strongly Conservative"), low = "blue", high = "red") + 
  ggtitle("Free Will, Political Orientation, and Negative Potential") + xlab("Free Will Beliefs") + ylab("Negative Potential") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text=element_text(size=12,face = "bold"), axis.title=element_text(size=14,face = "bold"), legend.text=element_text(size=7), legend.title = element_text(size=10))

GG_FWB_PO_PRES = ggplot(Passed_Study_Two_Data, aes(x=FWB, y=Present)) + geom_jitter(aes(col = D4), size = 3) + geom_smooth(method = "lm") +
  scale_color_continuous(name = "Political Orientation", labels = c("Strongly Liberal", "Moderately Liberal", "Slightly Liberal", "Moderate", "Slightly Conservative", "Moderately Conservative", "Strongly Conservative"), low = "blue", high = "red") +  
  ggtitle("Free Will, Political Orientation, and Present") + xlab("Free Will Beliefs") + ylab("Present") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text=element_text(size=12,face = "bold"), axis.title=element_text(size=14,face = "bold"), legend.text=element_text(size=7), legend.title = element_text(size=10))

GG_FWB_PO_BEST = ggplot(Passed_Study_Two_Data, aes(x=FWB, y=Best_Future)) + geom_jitter(aes(col = D4), size = 3) + geom_smooth(method = "lm") +
  scale_color_continuous(name = "Political Orientation", labels = c("Strongly Liberal", "Moderately Liberal", "Slightly Liberal", "Moderate", "Slightly Conservative", "Moderately Conservative", "Strongly Conservative"), low = "blue", high = "red") +  
  ggtitle("Free Will, Political Orientation, and Best Future") + xlab("Free Will Beliefs") + ylab("Best Future") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text=element_text(size=12,face = "bold"), axis.title=element_text(size=14,face = "bold"), legend.text=element_text(size=7), legend.title = element_text(size=10))

GG_FWB_PO_WORST = ggplot(Passed_Study_Two_Data, aes(x=FWB, y=Worst_Future)) + geom_jitter(aes(col = D4), size = 3) + geom_smooth(method = "lm") +
  scale_color_continuous(name = "Political Orientation", labels = c("Strongly Liberal", "Moderately Liberal", "Slightly Liberal", "Moderate", "Slightly Conservative", "Moderately Conservative", "Strongly Conservative"), low = "blue", high = "red") +  
  ggtitle("Free Will, Political Orientation, and Worst Future") + xlab("Free Will Beliefs") + ylab("Worst Future") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text=element_text(size=12,face = "bold"), axis.title=element_text(size=14,face = "bold"), legend.text=element_text(size=7), legend.title = element_text(size=10))

GG_PO_BEST = ggplot(Passed_Study_Two_Data, aes(x=D4, y=Best_Future)) + geom_jitter(aes(col = FWB), size = 3) + geom_smooth(method = "lm") +
  scale_color_continuous(name = "Free Will Beliefs", labels = c("Low Free Will", "", "", "Moderate Free Will", "", "", "High Free Will"), low = "red", high = "yellow") + 
  ggtitle("Political Orientation, Free Will, and Best Future") + xlab("Conservatism") + ylab("Best Future") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text=element_text(size=12,face = "bold"), axis.title=element_text(size=14,face = "bold"), legend.text=element_text(size=7), legend.title = element_text(size=10))

GG_FWB_ADM_POS_PO = ggplot(Passed_Study_Two_Data, aes(x=FWB, y=Positive_Potential)) + geom_jitter(aes(col = Current_Versus_Previous_ADM), size = 3) + scale_color_continuous(low = "blue", high = "red") + 
  ggtitle("Free Will, Administation Difference, and Positive Potential") + xlab("Free Will Beliefs") + ylab("Positive Potential") + labs(col = "Trump V. Obama")


########################Standardized Potential##############################

#Ratio_Positive_Negative_Potential = Passed_Study_Two_Data$Positive_Potential/Passed_Study_Two_Data$Negative_Potential
#summary(Ratio_Positive_Negative_Potential)

Standardized_Positive_Potential = abs(Passed_Study_Two_Data$Positive_Potential/Passed_Study_Two_Data$Absolute_Potential)
summary(Standardized_Positive_Potential)
table(abs(Standardized_Positive_Potential)>1)
plot(Standardized_Positive_Potential)


Standardized_Negative_Potential = abs(Passed_Study_Two_Data$Negative_Potential/Passed_Study_Two_Data$Absolute_Potential)
summary(Standardized_Negative_Potential)
table(abs(Standardized_Negative_Potential)>1)

Absolute_Potential = abs(Passed_Study_Two_Data$Absolute_Potential)


Standardized_Potential = as.data.frame(cbind(Standardized_Positive_Potential, Standardized_Negative_Potential, Absolute_Potential))
#Standardized_Potential = dplyr::filter(Standardized_Potential, Standardized_Positive_Potential < 1, Standardized_Negative_Potential < 1)
Standardized_Potential$Combined = Standardized_Potential$Standardized_Positive_Potential + Standardized_Potential$Standardized_Negative_Potential
summary(Standardized_Potential$Standardized_Positive_Potential)
summary(Standardized_Potential$Standardized_Negative_Potential)
questionr::freq(Standardized_Potential$Combined > 1)
Standardized_Potential = mutate(Standardized_Potential, Log_Potential_Greater = if_else(Combined > 1, 1, 0))

corstars(Standardized_Potential)

##################################################################################################################################################################
#########################################################Test of Mediational Relationship#########################################################################
##################################################################################################################################################################
#?psych::mediate

###FWB Mediated by PO###
Mediation_FWB_PO_Potential = psych::mediate(y = "Positive_Potential", x = "FWB", m = "D4", data = Passed_Study_Two_Data, std = TRUE)
Mediation_FWB_PO_Potential

###PO Mediated by FWB###
Alternative_Mediation_PO_POS_Potential = psych::mediate(y = "Positive_Potential", x = "D4", m = "FWB", data = Passed_Study_Two_Data, std = TRUE)
Alternative_Mediation_PO_POS_Potential


##################################################################################################################################################################
#########################################################Secondary Mediational Tests#############################################################################
##################################################################################################################################################################

###FWB Mediated by LOT and PO###
Alternative_Mediation_LOT_PO_POS_Potential = psych::mediate(y = "Positive_Potential", x = "FWB", m = c("LOT", "D4"), data = Passed_Study_Two_Data, std = TRUE)
Alternative_Mediation_LOT_PO_POS_Potential

###FWB Mediated by ADM and PO###
#Alternative_Mediation_ADM_PO_POS_Potential = psych::mediate(y = "Positive_Potential", x = "FWB", m = c("D4", "Current_Versus_Previous_ADM", "LOT"), data = Passed_Study_Two_Data, std = TRUE)
#Alternative_Mediation_ADM_PO_POS_Potential

###
B <- glm(D4 ~ FWB, data = Passed_Study_Two_Data)
#reghelper::beta(B, x = FALSE, y = TRUE)
summary(B)
C <- glm(Positive_Potential ~ D4 + FWB, data = Passed_Study_Two_Data)
#reghelper::beta(C, x = FALSE, y = TRUE)
summary(C)

###Test Mediation with covariate###
ADM_Covariate_Mediation = mediation::mediate(B, C, treat = "FWB", mediator = "D4", covariates = Passed_Study_Two_Data$Current_Versus_Previous_ADM)
summary(ADM_Covariate_Mediation)
mean(ADM_Covariate_Mediation$covariates)


###FWB and Present Mediated by PO###
Mediation_FWB_Present = psych::mediate(y = "Present", x = "FWB", m = "D4", data = Passed_Study_Two_Data, std = TRUE)
Mediation_FWB_Present

Mod_Gov_Trust = psych::mediate(y = "Positive_Potential", x = "FWB", mod = "Gov_Trust", data = Passed_Study_Two_Data, std = TRUE)

GLM_Gov_Trust = glm(Positive_Potential ~ FWB*Gov_Trust, data = Passed_Study_Two_Data)
reghelper::beta(GLM_Gov_Trust, x = FALSE, y = TRUE)

interact_plot(GLM_Gov_Trust, pred = "FWB", modx = "Gov_Trust", plot.points = FALSE, interval = TRUE, color.class = c('#91bfdb','#af8dc3', '#fc8d59'), x.label = "Free Will Beliefs", y.label = "Positive Potential", legend.main = "Trust in Government", main.title = "Figure 12: Moderation of Free Will Beliefs and Positive Potential by Government Trust") + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text=element_text(size=16,face = "bold"), axis.title=element_text(size=16,face = "bold"), legend.text=element_text(size=14), legend.title = element_text(size=16))

psych::mediate(y = "Positive_Potential", x = "FWB", mod = "Gov_5", data = Passed_Study_Two_Data, std = TRUE)

GLM_Gov_Trust2 = glm(Present ~ FWB*Gov_Trust, data = Passed_Study_Two_Data)
reghelper::beta(GLM_Gov_Trust2, x = FALSE, y = TRUE)

interact_plot(GLM_Gov_Trust2, pred = "FWB", modx = "Gov_Trust", plot.points = FALSE, interval = TRUE, color.class = c('#91bfdb','#af8dc3', '#fc8d59'), x.label = "Free Will Beliefs", y.label = "Positive Potential", legend.main = "Trust in Government", main.title = "Figure #: Moderation of Free Will Beliefs and Positive Potential by Government Trust") 


########################################################################################################################################
######################################Test of Structural Equation Model#################################################################
########################################################################################################################################
#?lavaan
str(Passed_Study_Two_Data, list.len = 160)

#Passed_Study_Two_Data = mutate(Passed_Study_Two_Data, Diff_Soc_1 = Best_Future_1 - Present_1, Diff_Soc_2 = Best_Future_2 - Present_2, Diff_Soc_3 = Best_Future_3 - Present_3, Diff_Soc_4 = Best_Future_4 - Present_4, Diff_Soc_5 = Best_Future_5 - Present_5, Diff_Soc_6 = Best_Future_6 - Present_6)

##Subset for Scaled Model Data##
Model_Study_Two_Data = Passed_Study_Two_Data %>%
  dplyr::select(FWB_1, FWB_2, FWB_3, FWB_4, FWB_5, FWB_6, FWB_7, FWB_8, D4, Best_Future_1, Best_Future_2, Best_Future_3, Best_Future_4, Best_Future_5, Best_Future_6, Present_1, Present_2, Present_3, Present_4, Present_5, Present_6, PIP_1,	PIP_2,	PIP_3,	PIP_4,	PIP_5,	PIP_6,	PIP_7,
                PIP_8,	PIP_9,	PIP_10,	PIP_11, PIP_12,	PIP_13,	PIP_14,	PIP_15,	PIP_16,	PIP_17,	PIP_18,	PIP_19,	PIP_20,	PIP_21,	PIP_22,	PIP_23,	PIP_24,	PIP_25,	PIP_26, PIP_27, PIP_28, PIP_29, PIP_30, Positive_Potential, Negative_Potential, FWB, Core_Conservatism, Support_Social_Safety_Net, Liberal_Social_Values, Socialism, Anti_Trump_Globalism, SVS_1, SVS_2, SVS_3, SVS_4, SVS_5, SVS_6, SVS_7, SVS_8, SVS_9, SVS_10) %>%
  dplyr::mutate(Diff_Soc_1 = (Best_Future_1 - Present_1)/10, Diff_Soc_2 = (Best_Future_2 - Present_2)/10, Diff_Soc_3 = (Best_Future_3 - Present_3)/10, Diff_Soc_4 = (Best_Future_4 - Present_4)/10, Diff_Soc_5 = (Best_Future_5 - Present_5)/10, Diff_Soc_6 = (Best_Future_6 - Present_6)/10,
         PIP_1 = PIP_1/10,	PIP_2 = PIP_2/-10,	PIP_3 = PIP_3/10,	PIP_4 = PIP_4/-10,	PIP_5 = PIP_5/10,	PIP_6 = PIP_6/10,	PIP_7 = PIP_7/10,	PIP_8 = PIP_8/10,	PIP_9 = PIP_9/10,	PIP_10 = PIP_10/10,	PIP_11 = PIP_11/10,
         PIP_12 = PIP_12/10,	PIP_13 = PIP_13/10,	PIP_14 = PIP_14/-10,	PIP_15 = PIP_15/10,	PIP_16 = PIP_16/10,	PIP_17 = PIP_17/10,	PIP_18 = PIP_18/10,	PIP_19 = PIP_19/10,	PIP_20 = PIP_20/10,	PIP_21 = PIP_21/10,	PIP_22 = PIP_22/10,	PIP_23 = PIP_23/10,	PIP_24 = PIP_24/10,	PIP_25 = PIP_25/10,	PIP_26 = PIP_26/10, PIP_27 = PIP_27/10, PIP_28 = PIP_28/10, PIP_29 = PIP_29/10, PIP_30 = PIP_30/10,
         Positive_Potential = Positive_Potential/10, Negative_Potential = Negative_Potential/10, Core_Conservatism = Core_Conservatism/10, Support_Social_Safety_Net = Support_Social_Safety_Net/10, Liberal_Social_Values = Liberal_Social_Values/10, Socialism = Socialism/10, Anti_Trump_Globalism = Anti_Trump_Globalism/10,
         Present_1 = Present_1/10, Present_2 = Present_2/10, Present_3 = Present_3/10, Present_4 = Present_4/10, Present_5 = Present_5/10, Present_6 = Present_6/10)


str(Model_Study_Two_Data)

Model_Study_Two_Data$Malhanobis <- mahalanobis(Model_Study_Two_Data[, 1:51], center = colMeans(Model_Study_Two_Data[, 1:51]), cov(Model_Study_Two_Data[, 1:51]), inverted = FALSE)
summary(Model_Study_Two_Data$Malhanobis)
hist(Model_Study_Two_Data$Malhanobis)

head(Model_Study_Two_Data %>%
  dplyr::select(Malhanobis) %>%
  arrange(desc(Malhanobis)), n = 30)


Distance_Correlation_matrix = as.tbl(corstars(Model_Study_Two_Data))

#Model_Study_Two_Data = dplyr::filter(Model_Study_Two_Data, Malhanobis < 100)


Societal_Positive_Potential_Model <- '
      #Latent Factors#
      FWB =~ FWB_1 + FWB_2 + FWB_3 +  FWB_4 + FWB_5 + FWB_6 + FWB_7 + FWB_8
      PO =~ D4
      D4~~ 0.9*D4
      Pos_Potential =~ Diff_Soc_1 + Diff_Soc_2 + Diff_Soc_3 + Diff_Soc_4 + Diff_Soc_5 + Diff_Soc_6
      Core_Conservatism =~ PIP_3 + PIP_6 + PIP_9 + PIP_10 + PIP_11 + PIP_12 + PIP_25 + PIP_26 + PIP_30
      Social_Safety_Net =~ PIP_5 + PIP_15 + PIP_20 + PIP_21 + PIP_22 + PIP_27
      Liberal_Social_Values =~ PIP_1 + PIP_4 + PIP_7 + PIP_28
      Socialism =~ PIP_2 + PIP_17 + PIP_23 + PIP_29
      Pro_Globalism =~ PIP_13 + PIP_14 + PIP_18 + PIP_19
      #Regression Paths#
      Pos_Potential ~ FWB + PO
      PO ~ FWB
      Core_Conservatism ~ Pos_Potential
      Social_Safety_Net ~ Pos_Potential
      Liberal_Social_Values ~ Pos_Potential
      Socialism ~ Pos_Potential
      Pro_Globalism ~ Pos_Potential
      #Covariances among Policy Positions#
      Core_Conservatism ~~ Social_Safety_Net + Liberal_Social_Values + Socialism + Pro_Globalism
      Social_Safety_Net ~~ Liberal_Social_Values + Socialism + Pro_Globalism
      Liberal_Social_Values ~~ Socialism + Pro_Globalism
      Socialism ~~ Pro_Globalism
'


#?sem
#?fitMeasures

Societal_Positive_Potential = sem(Societal_Positive_Potential_Model, data = Model_Study_Two_Data)
summary(Societal_Positive_Potential, standardized=TRUE, ci = TRUE)
fitMeasures(Societal_Positive_Potential, c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"))
modificationindices(Societal_Positive_Potential, minimum.value = 60)
Residuals_Model = as.tbl(as.data.frame(residuals(Societal_Positive_Potential, type = "standardized")$cov))
parameterEstimates(Societal_Positive_Potential)


#?modificationindices

semPaths(Societal_Positive_Potential, what = "std", curvePivot = TRUE, layout = "tree2", bifactor = "g", residuals = FALSE)

#?semPlot

#ggsem(Societal_Positive_Potential)
#?semPaths
#semPlotModel(Societal_Positive_Potential)


varTable(Societal_Positive_Potential)



##############Alternative Model#####################
Alternative_Positive_Potential_Model <- '
#Latent Factors#
FWB =~ FWB_1 + FWB_2 + FWB_3 +  FWB_4 + FWB_5 + FWB_6 + FWB_7 + FWB_8
PO =~ D4
D4~~ 0.9*D4
Pos_Potential =~ Diff_Soc_1 + Diff_Soc_2 + Diff_Soc_3 + Diff_Soc_4 + Diff_Soc_5 + Diff_Soc_6
Core_Conservatism =~ PIP_3 + PIP_6 + PIP_9 + PIP_10 + PIP_11 + PIP_12 + PIP_25 + PIP_26 + PIP_30
Social_Safety_Net =~ PIP_5 + PIP_15 + PIP_20 + PIP_21 + PIP_22 + PIP_27
Liberal_Social_Values =~ PIP_1 + PIP_4 + PIP_7 + PIP_28
Socialism =~ PIP_2 + PIP_17 + PIP_23 + PIP_29
Pro_Globalism =~ PIP_13 + PIP_14 + PIP_18 + PIP_19
#Regression Paths#
Pos_Potential ~ PO + FWB
FWB ~ PO
Core_Conservatism ~ Pos_Potential
Social_Safety_Net ~ Pos_Potential
Liberal_Social_Values ~ Pos_Potential
Socialism ~ Pos_Potential
Pro_Globalism ~ Pos_Potential
#Covariances among Policy Positions#
Core_Conservatism ~~ Social_Safety_Net + Liberal_Social_Values + Socialism + Pro_Globalism
Social_Safety_Net ~~ Liberal_Social_Values + Socialism + Pro_Globalism
Liberal_Social_Values ~~ Socialism + Pro_Globalism
Socialism ~~ Pro_Globalism
'

Alternative_Positive_Potential = sem(Alternative_Positive_Potential_Model, data = Model_Study_Two_Data )
summary(Alternative_Positive_Potential)
#fitMeasures(Alternative_Positive_Potential, c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"))
Alt_Fit_Compare <- compareFit(Societal_Positive_Potential, Alternative_Positive_Potential, nested = TRUE)


###Alternate Present Model###
Alternative_Present_Model <- '
#Latent Factors#
FWB =~ FWB_1 + FWB_2 + FWB_3 +  FWB_4 + FWB_5 + FWB_6 + FWB_7 + FWB_8
PO =~ D4
D4~~ 0.9*D4
Present =~ Present_1 + Present_2 + Present_3 + Present_4 + Present_5 + Present_6
Core_Conservatism =~ PIP_3 + PIP_6 + PIP_9 + PIP_10 + PIP_11 + PIP_12 + PIP_25 + PIP_26 + PIP_30
Social_Safety_Net =~ PIP_5 + PIP_15 + PIP_20 + PIP_21 + PIP_22 + PIP_27
Liberal_Social_Values =~ PIP_1 + PIP_4 + PIP_7 + PIP_28
Socialism =~ PIP_2 + PIP_17 + PIP_23 + PIP_29
Pro_Globalism =~ PIP_13 + PIP_14 + PIP_18 + PIP_19
#Regression Paths#
Present ~ PO + FWB
FWB ~ PO
Core_Conservatism ~ Present
Social_Safety_Net ~ Present
Liberal_Social_Values ~ Present
Socialism ~ Present
Pro_Globalism ~ Present
#Covariances among Policy Positions#
Core_Conservatism ~~ Social_Safety_Net + Liberal_Social_Values + Socialism + Pro_Globalism
Social_Safety_Net ~~ Liberal_Social_Values + Socialism + Pro_Globalism
Liberal_Social_Values ~~ Socialism + Pro_Globalism
Socialism ~~ Pro_Globalism
'

Alternative_Present = sem(Alternative_Present_Model, data = Model_Study_Two_Data )
summary(Alternative_Present)
fitMeasures(Alternative_Present, c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"))
fitMeasures(Societal_Positive_Potential, c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"))


##Compare with Positive Potential vs. Present Alone##
Fit_Compare <- compareFit(Societal_Positive_Potential, Alternative_Present, nested = FALSE)
#?compareFit


####Modfication####
Modified_Societal_Positive_Potential_Model <- '
#Latent Factors#
FWB =~ FWB_1 + FWB_2 + FWB_3 +  FWB_4 + FWB_5 + FWB_6 + FWB_7 + FWB_8
PO =~ D4
D4~~ 0.9*D4
Pos_Potential =~ Diff_Soc_1 + Diff_Soc_2 + Diff_Soc_3 + Diff_Soc_4 + Diff_Soc_5 + Diff_Soc_6
Core_Conservatism =~ PIP_3 + PIP_6 + PIP_9 + PIP_10 + PIP_11 + PIP_12 + PIP_25 + PIP_26 + PIP_30
Social_Safety_Net =~ PIP_5 + PIP_15 + PIP_20 + PIP_21 + PIP_22 + PIP_27
Liberal_Social_Values =~ PIP_1 + PIP_4 + PIP_7 + PIP_28
Socialism =~ PIP_2 + PIP_17 + PIP_23 + PIP_29
Pro_Globalism =~ PIP_13 + PIP_14 + PIP_18 + PIP_19
#Regression Paths#
Pos_Potential ~ FWB + PO
PO ~ FWB
Core_Conservatism ~ Pos_Potential
Social_Safety_Net ~ Pos_Potential
Liberal_Social_Values ~ Pos_Potential
Socialism ~ Pos_Potential
Pro_Globalism ~ Pos_Potential
Pos_Potential ~ Core_Conservatism + Liberal_Social_Values + Socialism + Pro_Globalism + Social_Safety_Net
#Covariances among Policy Positions#
Core_Conservatism ~~ Social_Safety_Net + Liberal_Social_Values + Socialism + Pro_Globalism
Social_Safety_Net ~~ Liberal_Social_Values + Socialism + Pro_Globalism
Liberal_Social_Values ~~ Socialism + Pro_Globalism
Socialism ~~ Pro_Globalism
'

Modified_Societal_Positive_Potential = sem(Modified_Societal_Positive_Potential_Model, data = Model_Study_Two_Data)
summary(Modified_Societal_Positive_Potential, standardized=TRUE)
fitMeasures(Modified_Societal_Positive_Potential, c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"))
modificationindices(Modified_Societal_Positive_Potential, minimum.value = 40)
#Residuals_Model = as.tbl(as.data.frame(residuals(Societal_Positive_Potential, type = "standardized")$cov))

semPaths(Modified_Societal_Positive_Potential, "std", curvePivot = TRUE, layout = "tree2", bifactor = "g")


####Conservatism Values Model#########
Conservatism_Positive_Potential_Model <- '
#Latent Factors#
FWB =~ FWB_1 + FWB_2 + FWB_3 +  FWB_4 + FWB_5 + FWB_6 + FWB_7 + FWB_8
PO =~ D4
D4~~ 0.9*D4
Pos_Potential =~ Diff_Soc_1 + Diff_Soc_2 + Diff_Soc_3 + Diff_Soc_4 + Diff_Soc_5 + Diff_Soc_6
Core_Conservatism =~ PIP_3 + PIP_6 + PIP_9 + PIP_10 + PIP_11 + PIP_12 + PIP_25 + PIP_26 + PIP_30
#Regression Paths#
Pos_Potential ~ FWB + PO
PO ~ FWB
Core_Conservatism ~ Pos_Potential
'

Conservatism_Positive_Potential = sem(Conservatism_Positive_Potential_Model, data = Model_Study_Two_Data )
summary(Conservatism_Positive_Potential)
fitMeasures(Conservatism_Positive_Potential, c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"))


####Socialism Values Model#########
Socialism_Positive_Potential_Model <- '
#Latent Factors#
FWB =~ FWB_1 + FWB_2 + FWB_3 +  FWB_4 + FWB_5 + FWB_6 + FWB_7 + FWB_8
PO =~ D4
D4~~ 0.9*D4
Pos_Potential =~ Diff_Soc_1 + Diff_Soc_2 + Diff_Soc_3 + Diff_Soc_4 + Diff_Soc_5 + Diff_Soc_6
Socialism =~ PIP_2 + PIP_17 + PIP_23 + PIP_29
#Regression Paths#
Pos_Potential ~ FWB + PO
PO ~ FWB
Socialism ~ Pos_Potential
'

Socialism_Positive_Potential = sem(Socialism_Positive_Potential_Model, data = Model_Study_Two_Data )
summary(Socialism_Positive_Potential)
fitMeasures(Socialism_Positive_Potential, c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"))
semPaths(Socialism_Positive_Potential)


####Social Values Model#########
Social_Positive_Potential_Model <- '
#Latent Factors#
FWB =~ FWB_1 + FWB_2 + FWB_3 +  FWB_4 + FWB_5 + FWB_6 + FWB_7 + FWB_8
PO =~ D4
D4~~ 0.9*D4
Pos_Potential =~ Diff_Soc_1 + Diff_Soc_2 + Diff_Soc_3 + Diff_Soc_4 + Diff_Soc_5 + Diff_Soc_6
Liberal_Social_Values =~ PIP_1 + PIP_4 + PIP_7 + PIP_28
#Regression Paths#
Pos_Potential ~ FWB + PO
PO ~ FWB
Liberal_Social_Values ~ Pos_Potential
'

Social_Positive_Potential = sem(Social_Positive_Potential_Model, data = Model_Study_Two_Data )
summary(Social_Positive_Potential)
fitMeasures(Social_Positive_Potential, c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"))
semPaths(Social_Positive_Potential, what = "std", curve = TRUE, layout = "tree2")


####Social Values Model#########
Safety_Net_Positive_Potential_Model <- '
#Latent Factors#
FWB =~ FWB_1 + FWB_2 + FWB_3 +  FWB_4 + FWB_5 + FWB_6 + FWB_7 + FWB_8
PO =~ D4
D4~~ 0.9*D4
Pos_Potential =~ Diff_Soc_1 + Diff_Soc_2 + Diff_Soc_3 + Diff_Soc_4 + Diff_Soc_5 + Diff_Soc_6
Social_Safety_Net =~ PIP_5 + PIP_15 + PIP_20 + PIP_21 + PIP_22 + PIP_27
#Regression Paths#
Pos_Potential ~ FWB + PO
PO ~ FWB
Social_Safety_Net ~ Pos_Potential
'

Safety_Net_Positive_Potential = sem(Safety_Net_Positive_Potential_Model, data = Model_Study_Two_Data )
summary(Safety_Net_Positive_Potential)
fitMeasures(Safety_Net_Positive_Potential, c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"))
semPaths(Safety_Net_Positive_Potential, what = "std", curve = TRUE, layout = "tree2")


####Globalism Values Model#########
Globalism_Positive_Potential_Model <- '
#Latent Factors#
FWB =~ FWB_1 + FWB_2 + FWB_3 +  FWB_4 + FWB_5 + FWB_6 + FWB_7 + FWB_8
PO =~ D4
D4~~ 0.9*D4
Pos_Potential =~ Diff_Soc_1 + Diff_Soc_2 + Diff_Soc_3 + Diff_Soc_4 + Diff_Soc_5 + Diff_Soc_6
Pro_Globalism =~ PIP_13 + PIP_14 + PIP_18 + PIP_19
#Regression Paths#
Pos_Potential ~ FWB + PO
PO ~ FWB
Pro_Globalism ~ Pos_Potential
'

Globalism_Positive_Potential = sem(Globalism_Positive_Potential_Model, data = Model_Study_Two_Data )
summary(Globalism_Positive_Potential)
fitMeasures(Globalism_Positive_Potential, c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"))
semPaths(Globalism_Positive_Potential, what = "std", curve = TRUE, layout = "tree2")



####Other Models#####
Safety_Conservative_Positive_Potential_Model <- '
#Latent Factors#
FWB =~ FWB_1 + FWB_2 + FWB_3 +  FWB_4 + FWB_5 + FWB_6 + FWB_7 + FWB_8
PO =~ D4
D4~~ 0.9*D4
Pos_Potential =~ Diff_Soc_1 + Diff_Soc_2 + Diff_Soc_3 + Diff_Soc_4 + Diff_Soc_5 + Diff_Soc_6
Social_Safety_Net =~ PIP_5 + PIP_15 + PIP_20 + PIP_21 + PIP_22 + PIP_27
Core_Conservatism =~ PIP_3 + PIP_6 + PIP_9 + PIP_10 + PIP_11 + PIP_12 + PIP_25 + PIP_26 + PIP_30
#Regression Paths#
Pos_Potential ~ FWB + PO
PO ~ FWB
Social_Safety_Net ~ Pos_Potential
Core_Conservatism ~ Pos_Potential
#Covariances#
Social_Safety_Net ~~ Core_Conservatism
'

Safety_Conservative_Positive_Potential = sem(Safety_Conservative_Positive_Potential_Model, data = Model_Study_Two_Data )
summary(Safety_Conservative_Positive_Potential)
fitMeasures(Safety_Conservative_Positive_Potential, c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
semPaths(Safety_Conservative_Positive_Potential)


####SVS Model####
SVS_Model <- '
#Latent Factors#
FWB =~ FWB_1 + FWB_2 + FWB_3 +  FWB_4 + FWB_5 + FWB_6 + FWB_7 + FWB_8
PO =~ D4
D4~~ 0.9*D4
Pos_Potential =~ Diff_Soc_1 + Diff_Soc_2 + Diff_Soc_3 + Diff_Soc_4 + Diff_Soc_5 + Diff_Soc_6
Self_Enhancement =~ SVS_1 + SVS_2
Openness_to_Experience =~ SVS_3 + SVS_4 + SVS_5
Self_Transcendence =~ SVS_6 + SVS_7
Conservation =~ SVS_8 + SVS_9 + SVS_10
#Regression Paths#
Pos_Potential ~ FWB + PO
PO ~ FWB
Self_Enhancement ~ Pos_Potential
Openness_to_Experience ~ Pos_Potential
Self_Transcendence  ~ Pos_Potential
Conservation  ~ Pos_Potential
#Covariances among Policy Positions#
Self_Enhancement ~~ Openness_to_Experience + Self_Transcendence + Conservation
Openness_to_Experience ~~ Self_Transcendence + Conservation
Self_Transcendence ~~ Conservation 
'

SEM_SVS_Model = sem(SVS_Model, data = Model_Study_Two_Data)
summary(SEM_SVS_Model, standardized=TRUE)
fitMeasures(SEM_SVS_Model, c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"))
modificationindices(SEM_SVS_Model, minimum.value = 40)
#Residuals_Model = as.tbl(as.data.frame(residuals(Societal_Positive_Potential, type = "standardized")$cov))

semPaths(SEM_SVS_Model, "std", curvePivot = TRUE, layout = "tree2", residuals = FALSE)
semPaths(SEM_SVS_Model, "std", curvePivot = TRUE, layout = "tree")

###Path Models####
Path_Societal_Positive_Potential = '
#Regression Paths#
Positive_Potential ~ FWB + D4
D4 ~ FWB
Core_Conservatism ~ Positive_Potential
Support_Social_Safety_Net ~ Positive_Potential
Liberal_Social_Values ~ Positive_Potential
Socialism ~ Positive_Potential
Anti_Trump_Globalism ~ Positive_Potential
#Positive_Potential ~ Anti_Trump_Globalism + Liberal_Social_Values + Core_Conservatism
#Covariances#
Core_Conservatism ~~ Support_Social_Safety_Net + Liberal_Social_Values + Socialism + Anti_Trump_Globalism
Support_Social_Safety_Net ~~ Liberal_Social_Values + Socialism + Anti_Trump_Globalism
Liberal_Social_Values ~~ Socialism + Anti_Trump_Globalism
Socialism ~~ Anti_Trump_Globalism
'

Path_Analysis_Conservative_Positive_Potential = sem(Path_Societal_Positive_Potential, data = Model_Study_Two_Data )
summary(Path_Analysis_Conservative_Positive_Potential)
fitMeasures(Path_Analysis_Conservative_Positive_Potential, c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
semPaths(Path_Analysis_Conservative_Positive_Potential, curvePivot = TRUE, layout = "tree2")
modificationindices(Path_Analysis_Conservative_Positive_Potential, minimum.value = 100)



#######################Primary Hypothesis Correlations#################################
cor.test(Passed_Study_Two_Data$FWB, Passed_Study_Two_Data$Present) #2A#
cor.test(Passed_Study_Two_Data$FWB, Passed_Study_Two_Data$Best_Future)#2B#

pcor.test(Passed_Study_Two_Data$FWB, Passed_Study_Two_Data$Present, Passed_Study_Two_Data$Current_Versus_Previous_ADM) #2A#
pcor.test(Passed_Study_Two_Data$D4, Passed_Study_Two_Data$Present, Passed_Study_Two_Data$Current_Versus_Previous_ADM) #2A#
#pcor.test(Passed_Study_Two_Data$D4, Passed_Study_Two_Data$Best_Future, Passed_Study_Two_Data$Current_Versus_Previous_ADM) #2A#
#pcor.test(Passed_Study_Two_Data$FWB, Passed_Study_Two_Data$Best_Future, Passed_Study_Two_Data$Current_Versus_Previous_ADM) #2A#


cor.test(Passed_Study_Two_Data$D4, Passed_Study_Two_Data$Present) #1A#
cor.test(Passed_Study_Two_Data$D4, Passed_Study_Two_Data$Best_Future) #1B#


cor.test(Passed_Study_Two_Data$FWB, Passed_Study_Two_Data$Positive_Potential)#3B#
cor.test(Passed_Study_Two_Data$D4, Passed_Study_Two_Data$Positive_Potential)#3A#

pcor.test(Passed_Study_Two_Data$D4, Passed_Study_Two_Data$Positive_Potential, Passed_Study_Two_Data$Current_Versus_Previous_ADM) #2A#
pcor.test(Passed_Study_Two_Data$FWB, Passed_Study_Two_Data$Positive_Potential, Passed_Study_Two_Data$Current_Versus_Previous_ADM) #2A#


cor.test(Passed_Study_Two_Data$D4, Passed_Study_Two_Data$Current_Versus_Previous_ADM)


######Moderations#############
GLM_Mod_Best_Future = glm(Best_Future ~ D4*FWB, data = Passed_Study_Two_Data)
reghelper::beta(GLM_Mod_Best_Future, x = FALSE, y = TRUE)

Mod_Best_Fut = interact_plot(GLM_Mod_Best_Future, pred = "D4", modx = "FWB", plot.points = FALSE, interval = TRUE, color.class = c('#91bfdb','#af8dc3', '#fc8d59'), x.label = "Political Orientation (Conservative High)", y.label = "Best Future", legend.main = "Free Will Beliefs", main.title = "Figure 14: Moderation of Political Orientation and Best Future by Free Will Beliefs (Study Two)") + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text=element_text(size=16,face = "bold"), axis.title=element_text(size=16,face = "bold"), legend.text=element_text(size=14), legend.title = element_text(size=16))

GLM_Mod_Pos_Po = glm(Positive_Potential ~ D4*FWB, data = Passed_Study_Two_Data)
reghelper::beta(GLM_Mod_Pos_Po, x = FALSE, y = TRUE)

Mod_Pos_Po = interact_plot(GLM_Mod_Pos_Po, pred = "D4", modx = "FWB", plot.points = FALSE, interval = TRUE, color.class = c('#91bfdb','#af8dc3', '#fc8d59'), x.label = "Political Orientation (Conservative High)", y.label = "Positive Potential", legend.main = "Free Will Beliefs", main.title = "Figure 15: Moderation of Political Orientation and Positive Potential by Free Will Beliefs (Study Two)") + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text=element_text(size=16,face = "bold"), axis.title=element_text(size=16,face = "bold"), legend.text=element_text(size=14), legend.title = element_text(size=16))

GLM_Mod_Present = glm(Present ~ D4*FWB, data = Passed_Study_Two_Data)
reghelper::beta(GLM_Mod_Present, x = FALSE, y = TRUE)

grid.arrange(Mod_Best_Fut, Mod_Pos_Po)

#interact_plot(GLM_Mod_Present, pred = "D4", modx = "FWB", plot.points = FALSE, interval = TRUE, color.class = c('#91bfdb','#af8dc3', '#fc8d59'), x.label = "Political Orientation (Conservative High)", y.label = "Present", legend.main = "Free Will Beliefs", main.title = "Figure #: Moderation of Political Orientation and Present by Free Will Beliefs") 

###Write XLSX###
#write.csv(Full_Correlation, "C:/Users/bmoli/Documents/Dissertation/Study Two Correlation.csv", na = "")
#?write.csv

cor.test(Passed_Study_Two_Data$Positive_Potential, Passed_Study_Two_Data$Current_Versus_Previous_ADM)



Demographics = Passed_Study_Two_Data %>% dplyr::select(Age = D1, Gender = D2, Race = D3, PO = D4, Affiliation = D5, Education = D6, SES = D7, SSES = D8, Citizen = D9, Region = D10, RA = D11, Religiousity = D12)
mean(Demographics$Age)
questionr::freq(Demographics$Gender)
questionr::freq(Demographics$PO)
questionr::freq(Demographics$SES)
questionr::freq(Demographics$Education)
questionr::freq(Demographics$Race)
#lsr::table(Demographics$SSES)
mean(Demographics$Religiousity)