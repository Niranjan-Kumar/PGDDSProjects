
# Assignment_ Group_Work_ Due December 24,2017.-HR_Analysis of Churn
install.packages('MASS')
install.packages('ggplot2')
install.packages("car")
install.packages('caTools')
install.packages("dplyr")# merge dataFrames
install.packages("rio")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("gridExtra")
library(MASS)
library(ggplot2)
library(car)
library(caTools)
library("dplyr")
library(lubridate)
library(rio)
library(tidyverse)
  

setwd("~/Downloads/PA-I_Case_Study_HR_Analytics")
#_______________________________________________________________________________________________
# There are several files that we need to read in and we need the working directory
# The files are employee_survey_data.csv,general_data.csv, in_time.csv, manager_survey_data.csv
# and out_time.csv
#_______________________________________________________________________________________________
# Company and Employee information
general_data<-read.csv("general_data.csv", stringsAsFactors = FALSE)
manager_survey_data<-read.csv("manager_survey_data.csv", stringsAsFactors = FALSE)
employee_survey_data<-read.csv("employee_survey_data.csv",stringsAsFactors =FALSE)

#----------------------------------------------------------------------------------------------
# Employee Timecards
in_time<-read.csv("in_time.csv", stringsAsFactors = FALSE)
out_time<-read.csv("out_time.csv", stringsAsFactors = FALSE)
# The two dataframes for in_time and out_time need to be exploited for key_value 
# of time the worker in the company each day. 
# long hours and perhaps not recognized
#________________________________________________________________________________________________
#_______________________________________________________________________________________________
# Structure , number of rows and variables
#General_Data from the Company
str(general_data)# 4410 obs, 24 variables
sum(is.na(general_data))# total of 28 NA
nrow(unique(general_data))# 4410- no duplicates
summary(general_data)# The NA are in years worked and number of companies worked at
boxplot(general_data$MonthlyIncome)# WOW there is a severe disproportion-outliers at top making more than 150,000
table(general_data$Attrition)# 19.22% attrition rate. This is very high.
boxplot(general_data$DistanceFromHome)# Seems reasonable? what does it mean to be at distance x from office?
boxplot(general_data$EmployeeCount)
boxplot(general_data$Age)# relatively young -Mid-30's
boxplot(general_data$NumCompaniesWorked)# who has worked in more than 8 companies-others look ok
boxplot(general_data$StandardHours)
boxplot(general_data$PercentSalaryHike)# very good % salary hike
table(general_data$Over18)# All workers are over 18-we can remove this variable
boxplot(general_data$TotalWorkingYears)# Lots of outliers-people with more than 28 years of experience
table(general_data$Department)
#--------------------------------------------------------------------------------------------
#'''Human Resources      Research & Development                  Sales 
#         189                   2883                              1338''' 
#---------------------------------------------------------------------------------------------

table(general_data$Education)# well educated
#  1    2    3    4    5 
# 510  846 1716 1194  144
#---------------------------------------------------------------------------------------------
table(general_data$EducationField)# Biomedical Company-Pharmaceutica-drug development and sales
#______________________________________________________________________________________________
#Human Resources    Life Sciences        Marketing          Medical            Other 
#      81             1818                  477              1392              246 
#Technical Degree 
#     396 
#-----------------------------------------------------------------------------------------------
# attriton rate 
sales_attr<-subset(general_data,general_data$Department=="Sales")
table(sales_attr$Attrition)#17.68%(201/1137)-Seems to be par with Indusry, sligthly higher.
# what about peolpe with 2 years experience?
sales_attr_2<-subset(sales_attr,sales_attr$TotalWorkingYears<="2")
table(sales_attr_2$Attrition)#21.65% (105/485)#WOW this is high!!!!-people who stay pass the 2 year mark in sales do not quit
table(sales_attr_2$YearsAtCompany<="2")#195 have more than 2 year working here. 395 Have less than 2 years working here

#----------------------------------------------------------------------------------------------------
R_D_attr<-subset(general_data,general_data$Department=="Research & Development")
table(R_D_attr$Attrition)#18.64%(453/2430)-seems to be in accord with the Indusry
#What about people with 4 years in this job?
R_D_attr_4<-subset(R_D_attr,R_D_attr$TotalWorkingYears<="4")
table(R_D_attr_4$Attrition)#18.05%(294/1628)This is very Hight 

#----------------------------------------------------------------------------------------------------
HR_attr<-subset(general_data,general_data$Department=="Human Resources")
table(HR_attr$Attrition)#43.18% WOW THIS OUT OF CONTROL!!!!(57/132)
HR_attr_2<-subset(HR_attr,HR_attr$TotalWorkingYears<="2")
table(HR_attr_2$Attrition)#54.76%-(23/42-WOW!!!!-Badly Managed in People Value)
#_________________________________________________________________________________________________________________(general_data$Attrition&general_data=="Sales")
# there are 8 chr variables that need to be made into factors and the convert to numeric
# using bbinary coding or dummy variables note that the employeeID is one of the variables
View(general_data)# There are 3 columsn that are constant and can be removed: 8-EmployeeCount=1,
#                 # 16-Over18=Y(all employess are older than 18) and 18-Standard hours=8.0
general_data<-general_data[,-c(8,16,18)]
names(general_data)
dim(general_data)#4410 rows with 21 variables
str(general_data)# We need to convert the chr variables to factor variables to use them in ggplot2
#-----------------------------------------------------------------------------------------------
#CONVERTING 7 CHARACTER VARIABLES TO 7 Factor Variables-Easy of plotting onggplot2
general_data$Attrition<-as.factor(general_data$Attrition)
general_data$BusinessTravel<-as.factor(general_data$BusinessTravel)
general_data$Department<-as.factor(general_data$Department)
general_data$EducationField<-as.factor(general_data$EducationField)
general_data$Gender<-as.factor(general_data$Gender)
general_data$JobRole<-as.factor(general_data$JobRole)
general_data$MaritalStatus<-as.factor(general_data$MaritalStatus)
head(general_data)
summary(general_data)# Attrition (711/3699=19.22%)

#Employee_Surveys____________________________________________________________________________________________
str(employee_survey_data)#4410 obs, 4 int variables: EmployeeID,EnvironmentSatisfaction, JobSatisfaction,WorkLifeBalance
sum (is.na(employee_survey_data)) #83 cases_However do not remove NA until final dataset
unique(employee_survey_data)
str(employee_survey_data)
summary(employee_survey_data)
boxplot(employee_survey_data[,-1])# WorklifeBalance has very interesting pattern. Employees do not feel that they have a good
#----------------------------------------------------------------------------------------------
# Manager Survey_Data
# Manegerial Surveys are next:
#______________________________________________________________________________________________
str(manager_survey_data)#4410 obs, 3 variables, EmployeeID, Job Involment and PerformanceRating
summary(manager_survey_data)# no NA's present
boxplot(manager_survey_data[,-1])# Performance rating has an interesting pattern This may create bad "morale"
#_________________________________________________________________________________________________
# We can merge the two surveys into a single data frame merge by EmployeeID
#________________________________________________________________________________________________
surveys<-merge(employee_survey_data,manager_survey_data,by="EmployeeID")
head(surveys)
str(surveys)#4410 observation of 6 variables-all integer variables
#----------------------------------------------------------------------------------------------------
#Now we are going to clean and extract useful information from the in_time and out_time data frames
#______________________________________________________________________________________________________
#
#in_time
#_________________________________________________________________________________________________
str(in_time)#4410 observations 262 variables. These are time variables so we need the package 
names(in_time)[1]<-paste("EmployeeID")
View(in_time)# There are 12 sanctioned Holidays in the work schedule and they are logical variables
in_time_no_holidays<- in_time[, -c(2,11,19,47,88,143,187,198,224,225,226,258)]
View(in_time_no_holidays)# We removed the columns where the holidays were recorded as logical
#_________________________________________________________________________________________________
# out_time
#__________________________________________________________________________________________________
str(out_time)#4410 observations, 262 variables
names(out_time)[1]<-paste("EmployeeID")
out_time_no_holidays<-out_time[,-c(2,11,19,47,88,143,187,198,224,225,226,258)]
View(out_time_no_holidays)
#___________________________________________________________________________________

#'''After a careful review of both in_time_no_holidays and out_time_no_holidays we can make the
#assumption that NA represents an absence from work and the worker is not shown cheking in or out
#therefore an absence from work is equivalent to workin 0 hours.
#The NA in each dataframe will be replaced with 2015-12-30 24:00:00 When we do this, we will be 
#able to do some mathematical calculations and the total time work will be calculated as 0 which
#we will interprete as absent from work.'''

#-----------------------------------------------------------------------------------
in_time_no_holidays[is.na(in_time_no_holidays)]<-"2015-12-30 24:00:00"
View(in_time_no_holidays)
sum(is.na(in_time_no_holidays))#0. this means we have removed all NA values
out_time_no_holidays[is.na(out_time_no_holidays)]<-"2015-12-30 24:00:00"
View(out_time_no_holidays)
sum(is.na(out_time_no_holidays))#0 this means all NA were removed from the data.Frame
#------------------------------------------------------------------------------------------------
#'''Now we want to convert all the in_time_no_holidays ans out_time_no_holidays data frames
# to Dates using the as.POSIXct function together with the lapply function'''
#_______________________________________________________________________________________________
# We must remember that the times are in seconds and we need to convert to hours by dividing by 3600 s
# seconds in one hour.
#************************************************************************
#_____________________________________________________________________________________
#the line below worked in getting time from in_time
in_time_no_holidays[,-1]<-as.data.frame(lapply(in_time_no_holidays[,-1],as.POSIXct,format='%Y-%m-%d %H:%M:%OS',origin="2011-01-01 00:00"))
class(in_time_no_holidays[,2])# checking the class and format
str(in_time_no_holidays)
head(in_time_no_holidays)
#
# now we are going to convert the out_time_no_holidays 
out_time_no_holidays[,-1]<-as.data.frame(lapply(out_time_no_holidays[,-1],as.POSIXct,format="%Y-%m-%d %H:%M%OS", origin="2011-01-01 00:00"))
str(out_time_no_holidays)#4410 obs of 250 variable, one for each day of work-year.
head(out_time_no_holidays)
View((out_time_no_holidays))

#*****************************************************************************************
worked_hours<-as.data.frame((out_time_no_holidays[,-1]-in_time_no_holidays[,-1])/3600)
View(worked_hours)# Be careful that this is not numeric
str(worked_hours)
# we need to convert the hour to numeric
worked_hours_numeric<-lapply(worked_hours,as.numeric)
worked_hours_numeric_2<-lapply(worked_hours_numeric,round,digits=2)
str(worked_hours_numeric_2)
employee_worked_hrs<-as.data.frame(worked_hours_numeric_2)
str(employee_worked_hrs)#4410 obs with 249 variables. We are missing the Employee #
#We are adding the EmployeeID to the frame that has the total worked hours per day
#We are adding the Employee label to the first column

View(employee_worked_hrs)
# There are 249 days available to work at 8 hour per day. this is a total of 1992 hours
# if the worker does overtime, the hours will be more than 1992.?or we can look at the average/day
#________________________________________________________________________________________
# Remember that we have removed the EmployeeID to avoid errors in calculations-Put it back to merge
#--------------------------------------------
# The lines below calculate the average time per day worked by each employee.
employee_worked_hrs$AVG_hours<-round(rowMeans(employee_worked_hrs),2)
View(employee_worked_hrs$AVG_hours)
summary(employee_worked_hrs$AVG_hour)#See table Below

#------------------------------------------------------------------------------------------------
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.410   6.270   7.000   7.307   7.890  10.930 
#________________________________________________________________________________________________
# 
#    COMPUTING OVERTIME AS A LOGICAL VARIABLE
#############################################################################
# Note:knowing that the day is a standard 8.0 hour day, we will create a logical
# call Over_Time that is 1 if AVG_Time>8 and 0 otherwise.
employee_worked_hrs$Over_Time<-ifelse(employee_worked_hrs$AVG>8.0,1,0)
table(employee_worked_hrs$Over_Time)#1064 are working over_time ( 1064/(1064+3346))=24.13%
#---------------------------------------------------------------------------------------
# We are going to make a data.frame witht hw Avg and the Over_Time variables
# we call it critical_time
critical_time<-cbind(employee_worked_hrs$AVG_hours,employee_worked_hrs$Over_Time)
View(critical_time)
critical_time<-as.data.frame(cbind(in_time[,1],critical_time))# We are adding the EmployeeID
names(critical_time)<-paste(c("EmployeeID","AVG_hours","Over_Time"))
str(critical_time)
critical_time$Over_Time<-as.factor(critical_time$Over_Time)# Making the Over_Time a Factor
View(critical_time)# this data will be merge with general_data and surveys
#------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
data1<-merge(general_data,surveys,by="EmployeeID")
data2<-merge(data1,critical_time,by="EmployeeID")
#We are going to copy data2 into data
data<-data2
summary(data)
dim(data)#4410 observation with 28 variables
names(data)
#___________________________________________________________________________________________
# Let us check on the number of NA 
sum(is.na(data))#111 or 0.093%
dim(data)#4410*27=119070( 111/119070=0.00093)
data=na.omit(data)# how many observations will disappear?(1-(4300/4410))*100%=2.5% OK Take complete cases
str(data)
#____________________________________________________________________________________________________________

# EDA-ANALYSIS of the the DATA SET

#_______________________________________________________________________________________________________

#Lets us look at the Attrition percentage for the data
ggplot(data, aes(Attrition,fill=Attrition))+geom_bar()
#Calculate the ratios for Attrition
prop.table(table(data$Attrition))
#______________________________________________________________________________
#    Attrition
#    No       Yes 
#0.8383721 0.1616279 
# 16.16% of the Employees are leaving!!!!
#______________________________________________________________________________
#Let us look at each variable and see its influence on the Attrition of the organization

#Age: We see that majority of employees leaving the org are around 30 Years (Fig 1).
#Business Travel: Among people who leave, most travel(Fig 1).
#AVG_hours: We are not able to see any distinguishable feature here(Fig 1).
#Department: Among people attrited employees from HR dept. are less.It is because of low proportion of HR in the organization(Fig 1).

library(ggplot2)
library(grid)
library(gridExtra)
agePlot <- ggplot(data,aes(Age,fill=Attrition))+geom_density()+facet_grid(~Attrition)+coord_flip()
travelPlot <- ggplot(data,aes(BusinessTravel,fill=Attrition))+geom_bar()+coord_flip()
AVG_hoursPlot <- ggplot(data,aes(AVG_hours,Attrition))+geom_point(size=4,alpha = 0.05)+coord_flip()
depPlot <- ggplot(data,aes(Department,fill = Attrition))+geom_bar()+coord_flip()
grid.arrange(agePlot,travelPlot,AVG_hoursPlot,depPlot,ncol=2,top = "Fig 1")
#________________________________________________________________________________________
#Please notice that the Research & Deveolpment Department
#Has quite an impact on Attrition rate, while the age where
#Attriton is peaking is around 30 years and most of the
#Attriton comes between ages 20 to 30
# Interesting about seniority??WE NEED TO EXPLORE
#_________________________________________________________________________________________
# How Is overtime affecting the attrition rate?
plot1 <-data %>%
  ggplot(aes(x = Over_Time, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = 0.3) +
  labs(y = "Percentage", fill= "Over_Time") +
  facet_grid(~Attrition) +
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.3)) + 
  ggtitle("Attrition")+coord_flip()


plot2 <-data %>%
  group_by(Over_Time, Attrition) %>%
  tally() %>%
  ggplot(aes(x = Over_Time, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  geom_text(aes(label = n), vjust = -0.3, position = position_dodge(0.9))+
  labs(x="Over_time", y="Number Attrition")+
  ggtitle("Attrition in regards to Over_time")+coord_flip()

grid.arrange(plot1,plot2)

#_______________________________________________________________________
# The graph shows that people with lots of Over_Time work
#are leaving at a very high rate. Almost 2.7 times the avg rate

####################################################################

# Taking it forward, see the influence of variables viz. DistanceFromHome,
# Education, EducationField, Joblevel, Jobrole and other variables on the Attrition of the organization
#________________________________________________________________________

library(cowplot)

# Barcharts for categorical features 
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")


plot_grid(ggplot(data, aes(x=Education,fill=Attrition))+ geom_bar(), 
          ggplot(data, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(data, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(data, aes(x=JobLevel,fill=Attrition))+ geom_bar(),
          ggplot(data, aes(x=Gender,fill=Attrition))+ geom_bar(),
          ggplot(data, aes(x=factor(MaritalStatus),fill=Attrition))+ geom_bar(),
          align = "h")   

plot_grid(ggplot(data, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(data, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(data, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(data, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(data, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(data, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 



# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")


plot_grid(ggplot(data, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(data, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(data, aes(MonthlyIncome,fill=Attrition))+ geom_histogram(binwidth = 2000),
          ggplot(data, aes(x="",y=MonthlyIncome,fill=Attrition))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(data, aes(NumCompaniesWorked,fill=Attrition))+ geom_histogram(binwidth = 10),
          ggplot(data, aes(x="",y=NumCompaniesWorked,fill=Attrition))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


plot_grid(ggplot(data, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(data, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(data, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# converting target variable telecom from No/Yes character to factorwith levels 0/1 
data$Attrition<- ifelse(data$Attrition=="Yes",1,0)

# For variables having only two levels, converting to 0/1
data$Gender<- ifelse(data$Gender=="Female",1,0)

# creating a dataframe of categorical features
data_chr<- data[,c(4,5,7,8,10,11,12,16,22,23,24,25,26)]

# converting categorical attributes to factor
data_fact<- data.frame(sapply(data_chr, function(x) factor(x)))
str(data_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =data))[,-1]))


################################################################
# Feature standardisation

# Normalising continuous features 

data$MonthlyIncome<- scale(data$MonthlyIncome) 

# Final dataset
hrdata<- cbind(data[,-c(1,4,5,7,8,10,11,12,16,22,23,24,25,26)],dummies) 
View(hrdata) #4300 obs. of  57 variables
str(hrdata)
hrdata$Over_Time <- as.numeric(hrdata$Over_Time)
########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(hrdata$Attrition, SplitRatio = 0.7)

train = hrdata[indices,]

test = hrdata[!(indices),]

########################################################################
# Logistic Regression: 

#The first model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2089.9....56 coeff..Null deviance: 2661.4...Residual deviance: 1975.9

# Running stepAIC function
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
library(car)
vif(model_2)


#EducationField.xLife.Sciences & EducationField.xMedical has similar trends in significance and colinearity
# but EducationField.xLife.Sciences is slightly higher, Excluding EducationField.xLife.Sciences
model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_Time + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.x5 + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)
summary(model_3)

vif(model_3) # cannot exclude any more variable based on vif 

#Excluding EducationField.xMedical  due to lowest significance

model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_Time + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.x5 + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_4)

#Excluding EducationField.xMarketing
model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_Time + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.x5 + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_5)

#Excluding EducationField.xOther
model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_Time + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.x5 + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_6)

#Excluding EducationField.xTechnical.Degree 
model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_Time + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.x5 + JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_7)

#Excluding StockOptionLevel.x3 
model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_Time + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.x5 + JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_8)

#Excluding Education.x5  
model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_Time + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_9)
#Excluding MaritalStatus.xMarried   
model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_Time + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_10)

#Excluding JobRole.xResearch.Scientist 
model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_Time + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_11)

#Excluding JobRole.xHuman.Resources
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_Time + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobLevel.x2 + JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_12)

#Excluding  StockOptionLevel.x1 
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_Time + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobLevel.x2 + JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_13)

#Excluding JobRole.xManager 
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_Time + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_14)

#Excluding JobLevel.x2 
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_Time + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_15)

#Excluding JobInvolvement.x3 
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_Time + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobRole.xSales.Executive +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 , family = "binomial", 
                data = train)

summary(model_16)

#Excluding JobRole.xResearch.Director
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_Time + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xManufacturing.Director + JobRole.xSales.Executive +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 , family = "binomial", 
                data = train)

summary(model_17)

#Excluding JobRole.xSales.Executive
model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_Time + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 +
                  JobSatisfaction.x4 +WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 , family = "binomial", 
                data = train)

summary(model_18)

#Excluding BusinessTravel.xTravel_Rarely 
model_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_Time + BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director +
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 +JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 , family = "binomial", 
                data = train)

summary(model_19)

########################################################################
# Now we have 19 variables and all are highly significant, can't remove any
# So creating final model with 19 significant variables

final_model<- model_19

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities 

test_predict = predict(final_model, type = "response",newdata = test) 

# Let's see the summary 

summary(test_predict)
test$probability <- test_predict
View(test)

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_predict >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)


#######################################################################


library(caret)

test_confusion <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_confusion

#######################################################################

# Sensitivity is very low. So let's choose a different cutoff value

# First let's create a function to find the accuracy, sensitivity and specificity
# for a given cutoff

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_predict >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_predict)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

cutoff

# Let's choose a cutoff value of 0.194 for final model

test_cutoff_attrition <- factor(ifelse(test_predict >=0.194, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


# Exporting Data for Excel Analysis (KS, Gain, Lift etc.) #

myeval <- matrix(nrow = length(test_predict),ncol = 2)
myeval[,1] <- test_predict
myeval[,2] <- test_actual_attrition
colnames(myeval) <- c("Predicted_Prob","Actual_Labels")
write.csv(myeval,"myeval.csv")


# KS -statistic - Test Data #

library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)



# Lift & Gain Chart 

# Loading dplyr package 
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_predict, groups = 10)
Attrition_decile

Gain <- c(0,Attrition_decile$Gain)
Deciles <- c(0,Attrition_decile$bucket)
plot(y=Gain,x=Deciles,type ="l",lwd = 2,xlab="Bucket",ylab="Gain",main = "Gain Chart")

Random_Gain <- seq(from=0,to=100,by=10)
lines(y=Random_Gain,x=Deciles,type ="l",lwd = 2, col="red")

Perfect_Gain <- vector(mode = "numeric", length = 11)
for (i in 2:11){Perfect_Gain[i] <- 100*min(1,129*(i-1)/209)}
lines(y=Perfect_Gain,x=Deciles,type ="l",lwd = 2, col="darkgreen")



legend("bottomright",col=c("darkgreen","black","red"),lwd =c(2,2,2,2),c("Perfect Model","Actual Model","Random Model"), cex = 0.7)

# plotting the lift chart
Lift <- Gain/Random_Gain
Random_Lift <- Random_Gain/Random_Gain

plot(y=Lift,x=Deciles,type ="l",ylim=c(0,3.5),lwd = 2,xlab="Bucket",ylab="Lift",main = "Lift Chart",ylim<-c())
lines(y=Random_Lift,x=Deciles,type ="l",lwd = 2, col="red")

legend("topright",col=c("black","red"),lwd =c(2,2,2),c("Actual Model","Random Model"), cex = 0.7)



#######################################################################

