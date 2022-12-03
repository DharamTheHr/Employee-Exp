# Importing CSV data set 

data <- read.csv(file.choose())

#Validate for Correctness
# ------------------------------------------
# count of rows and columns
dim(data)

# view top 5 rows of the data set 
head(data, 5)

# view last 10 rows of the data set 
tail(data, 10)


# Data Sanity Check 
# ------------------------------------

# understanding types of variables
str(data) 

# Obtain the count of demographic variables in the data 

names(data)
data0<-data[c(1,2,3,4,5)]
data0
summary(data0)

#Insight 1- maximum employee are from the R&D departments 
#Insight 2- maximum employees have 5 to 10 yrs of exp 
#Insight 3- maximum employees are from 31 to 40 years of age and minimum employees are from 50 to 60 yrs of age
#Insight 4- maximum employees are male
#Insight 5- maximum employee are graduate and minimum are phD holders

#----------------------------------------------------------------------

# Visual and Metric Approach 

# Visual approach of department 

data_dept <- table(data$Department)
data_dept

perc_label <- round(100*data_dept/sum(data_dept),1)
perc_label

pie_lable<- paste(perc_label, "%")
pie_lable

pie(data_dept,col = rainbow(length(data_dept)), labels = pie_lable, main = "Number of employees in each department")
legend("bottomleft", c("Hr", "R&D","Sales"), fill = rainbow(length(data_dept)), cex = 0.5)

# Visual Approach of Education 

data_edu <- table(data$Education)

perc_label<-round(100*data_edu/sum(data_edu),1)

pie_lable1 <- paste(perc_label,"%") 

pie(data_edu, col = rainbow(length(data_edu)), labels = pie_lable1, main = "Education levels")
legend("bottomleft", c("Diploma", "Persuing Graduation", "Graduation", "Post Graduation", "P.hd"), fill =rainbow(length(data_edu)), cex = 0.6 )

# visual approach pf experience 

data_exp <-table(data$Experience.intervals)
perc_level2<-round(100*data_exp/sum(data_exp),1)
pie_lable2<- paste(perc_level2, "%")

pie(data_exp, col = rainbow(length(data_exp)), labels = pie_lable2, main = "Experience in years")
legend("bottomleft", c("upto 5 years", "5-10 years", "11-20 years", "21-40 years"), fill = rainbow(length(data_exp)), cex = 0.6 )

data_age <-table(data$age.intervals)
perc_level3<-round(100*data_age/sum(data_age),1)
pie_lable3<- paste(perc_level3, "%")

pie(data_age, col = rainbow(length(data_age)), labels = pie_lable3, main = "Age in years")
legend("bottomleft", c("18-30", "30-40", "40-50", "50-60"), fill = rainbow(length(data_age)), cex = 0.6 )



# Insigths : The maximum number of employee are in the age group of 30 to 40 yrs and the minimum numbers of employee are in the age group of 50 to 60 yrs 

#Visual approach to gender

data_gender <-table(data$Gender)
perc_level4<-round(100*data_gender/sum(data_gender),1)
pie_lable4<- paste(perc_level4, "%")

pie(data_gender, col = rainbow(length(data_gender)), labels = pie_lable4, main = "Gender")
legend("bottomleft", c("Female", "male"), fill = rainbow(length(data_gender)), cex = 0.6 )

#metric approach to department 

#to round of all the numbers to two value after decimal 

options(digits = 3)
tab_dep <- table(data$Department)
tab_dep
tab1<-prop.table(tab_dep)*100       
tab1 

#Matric approach to education 

tab_edu<- table(data$Education)
tab2<-prop.table(tab_edu)*100
tab2 

#Matric Approach to Exp 

tab_exp<- table(data$Experience.intervals)
tab3<-prop.table(tab_exp)*100
tab3 

#Matric approach to table 

tab_age<- table(data$age.intervals)
tab4<-prop.table(tab_age)*100
tab4

#Matric approach to gender 

tab_gender<- table(data$Gender)
tab5<-prop.table(tab_gender)*100
tab5

#----------------------Data Cleaning------------------------ 

#check if there are missing values 
sum(is.na(data))

#No missing values in the data 

#---------------------------Feature Engineerin---------------
#Data Transformation 

str(data)

#Explicitly declaring al integer type variable as categorical variables 

data<- lapply(data, factor)
str(data)

# count and percentage of each response 
install.packages("summarytools", dependencies = TRUE)
library(summarytools)
dfSummary(data)

#-------------- Reliability Testing --- ------- 

#store survey response variable for reliability check 

names(data)
data1<-data[c(-1,-2,-3,-4,-5)]
names(data1)
str(data1)
# Explicitly declaring all factor type variables is numeric variables

data1 <-as.data.frame(lapply(data1, as.numeric))

# verify the structure and class of data1 after conversion 
str(data1)
class(data1)

# we will use "psych" library to calculate the alpha for overall data and for individual data as well if required
library(psych)
alpha(data1)

#-------------- Bivariate analysis and Hypothesis Teasting....... 

# Creating table and running chi square test on age.intervals and experience.intervals
#Null hypothesis statement, H0 states that  Age interval and experience interval are two independent variables

#Alternate Hypothesis H1 states that both the variables are associated 

tbl1<-table(data$age.intervals,data$Experience.intervals)
tbl1
chisq.test(tbl1) 

#Insight : Age and exp are two dependent variables 

#Creating table and running chi square test on age.intervals and education 

#Null hypothesis statement, H0 states that  Age interval and education interval are two independent variables

#Alternate Hypothesis H1 states that both the variables are associated 

tbl2<-table(data$age.intervals,data$Education)
tbl2
chisq.test(tbl2)

#insights Age and Education are associated 

# creating the table and running chi square on age.interval and gender 

#Null hypothesis states that age and gender are two independent variables
# Alternate hypothesis H1 states that Age and Gender are associated

tbl3 <-table(data$age.intervals,data$Gender)
tbl3
chisq.test(tbl3)
# Insights: Age and gender are two independent variables 

#Creating table and running chi square test on experience.intervals and education

#Null hypothesis H01 states that Experience and education are two independent variables. 


#Alternate hypothesis H11 states that Experience and Education are two dependent variable.

tbl4<-table(data$Experience.intervals,data$Education)
tbl4
chisq.test(tbl4)

# Insights: Experience and education are two dependent variables 

#Creating table and running chi square test on experience.intervals and gender

#Null hypothesis Ho states that Experience and gender are two independent variables.

#Alternate hypothesisH1 states that experience and gender are two dependent variable. 

tbl5<-table(data$Experience.intervals,data$Gender)
tbl5
chisq.test(tbl5)

#insights Experience and Gender are two Independent variables

# Creating table and running chi square test on education and gender 

#Null hypothesis H01: Education and gender are two independent variables.

#Alternate hypothesis H11, Education and Gender are two dependent Variables. 

tbl6<-table(data$Education,data$Gender)
tbl6
chisq.test(tbl6)

#Insights : Education and Gender are two independent variables. 

#Employee satisfaction may not be based on these two factors collectively. 

###################
#          Dimension Reduction Using Exploratory Factor Analysis
##-------------------------------------------------------------------

#will be using Psych Packages fa.parallel function to execute parallel analysis.

#To determine the number of factors we will use "fa.parallel" function 

library(psych)
fa.parallel(data1,fm='minres',fa='fa')

#To determine which statement lies in which factor
ninefactor<-fa(data1,nfactors=9,rotate = "oblimin",fm="minres")
print(ninefactor)

#To clear the picture we will remove the values below 0.01
print(ninefactor$loadings,cutoff = 0.01)
fa.diagram(ninefactor)

#-----------------------------------------------------------------
#                  Insights 

#Insights for factor 1 (p1=non satisfied, p2=neutral, p3=satisfied)

#We will convert the 5 responses scale into 3 responses scale by converting 
#"Highly agree and agree responses as satisfied"
#"Highly disagree and disagree responses as non-satisfied"
#"Neutral will remain neutral"

#To do that we will follow the process 

#Create table responses of variables in factor 1
v1<-table(data$var.01)
v1 
v2<-table(data$var.02)
v6<-table(data$var.06)
v7<-table(data$var.07)
v15<-table(data$var.15)
v30<-table(data$var.30)

#Get sum of all responses of variables present in factor 1
f1<-v1+v2+v6+v7+v15+v30
f1
class(f1)

#Values of tables can not be parsed using index numbers so, we will convert into matrix
m1<-as.matrix(f1)
m1

#Calculate the percentage of all the elements in matrix
sum_m1<-sum(m1)
sum_m1

#To calculate the percentage 
p<-(m1/sum_m1)*100
p

#To categorize into 3 response scale 

#satisfied employees 
p3<-p[4,]+p[5,]
p3

#Non-satisfied employees 
p1<-p[1,]+p[2,]
p1

#Neutral employees 
p2<-p[3,]
p2

# From the output we can calculate that 
# Around 37% of employees are non-satisfied with the organization's objectives and fair policies 
# Around 42% of the employees are satisfied with the organization's objectives and fair policies 

#Insights for factor 2 (p1=non satisfied, p2=neutral, p3=satisfied) 

#Create table responses of variables in factors 2 
v3<-table(data1$var.03)
v4<-table(data1$var.04)
v5<-table(data1$var.05)
v11<-table(data1$var.11)
f2<-v3+v4+v5+v11
m2<-as.matrix(f2)
sum_m2<-sum(m2)
p<-(m2/sum_m2)*100
p1<-p[1,]+p[2,]
p2<-p[3,]
p3<-p[4,]+p[5,]
p1
p2
p3

# Around 40% of employees are non-satisfied wit the working environment and balance 
# Around 41% of employees are satisfied with the working environment and balance 


#Insights for factor 3 (p1=non satisfied, p2=neutral, p3=satisfied) 

#Create table responses of variables in factor 3 
v12<-table(data1$var.12)
v16<-table(data1$var.16)
v17<-table(data1$var.17)
v22<-table(data1$var.22)

f3<-v12+v16+v17+v22
m3<-as.matrix(f3)
sum_m3<-sum(m3)
p<-(m3/sum_m3)*100
p1<-p[1,]+p[2,]
p3<-p[3,]
p3<-p[4,]+p[5,]
p1
p2
p3

# Around 40% of the employees are non satisfied with the feedback and appreciation they get for their work 
# Around 40% of the employees are satisfied with the feedback and appreciation they get for their work

##Insights for factor 4 (p1=non satisfied, p2=neutral, p3=satisfied) 

v23<-table(data1$var.23)
v26<-table(data1$var.26)
v28<-table(data1$var.28)
f4<-v23+v26+v28 
m4<-as.matrix(f4)
sum_m4<-sum(m4)
p<-(m4/sum_m4)*100
p1<-p[1,]+p[2,]
p2<-p[3,]
p3<-p[4,]+p[5,]
p1
p2
p3

#Around 42% of the employees are non-satisfied with growth and opportunities 
#Around 39% of employees are satisfied wit the growth and opportunities  

##Insights for factor 5 (p1=non satisfied, p2=neutral, p3=satisfied) 

v13<-table(data1$var.13)
v14<-table(data1$var.14)
v29<-table(data1$var.29)
f5<-v13+v14+v29 
m5<-as.matrix(f5)
sum_m5<-sum(m5)
p<-(m5/sum_m5)*100
p1<-p[1,]+p[2,]
p2<-p[3,]
p3<-p[4,]+p[5,]
p1
p2
p3

# Around 39% of the employees are non-satisfied with the motivation to work at the organization 
# Around 40% of the employees are satisfied with the motivation to work at the organization 

# Insights for factor 6 (p1=non satisfied, p2=neutral, p3=satisfied)

var8<-table(data1$var.08)
var9<-table(data1$var.09)
var10<-table(data$var.10)
f6<-var8+var9+var10
m6<-as.matrix(f6)
sum_m6<-sum(m6)
p<-(m6/sum_m6)*100
p1<-p[1,]+p[2,]
p2<-p[3,]
p3<-p[4,]+p[5,]
p1
p2
p3

#Around 39% of employees are not satisfied with the saying "Better Place to work
#Around 41% of the the employees are satisfied with the saying "Better place to work

# Insights for factor 7 (p1=non satisfied, p2=neutral, p3=satisfied) 

var18<-table(data1$var.18)
var25<-table(data1$var.25)
f7<-var18+var25
m7<-as.matrix(f7)
sum_m7<-sum(m7)
p<-(m7/sum_m7)*100
p1<-p[1,]+p[2,]
p2<-p[3,]
p3<-p[4,]+p[5,]
p1
p2
p3

#Around 42% of the employees are not satisfied with the work given 
#Around 40% of the employees are satisfied with the work given 

# Insights for factor 8 (p1=non satisfied, p2=neutral, p3=satisfied)

var19<-table(data1$var.19)
var21<-table(data1$var.21)
f8<-var19+var21 
m8<-as.matrix(f8)
sum_m8<-sum(m8)
p<-(m8/sum_m8)*100
p1<-p[1,]+p[2,]
p2<-p[3,]
p3<-p[4,]+p[5,]
p1
p2
p3

#Around 41% of the employees are not happy with the Incentives and other facilities. 
#Around 39% of the employees are happy with the Incentives and other facilities. 

# Insights for factor 9 (p1=non satisfied, p2=neutral, p3=satisfied)

var24<-table(data1$var.24)
var27<-table(data1$var.27)
f9<-var24+var27
m9<-as.matrix(f9)
sum_m9<-sum(m9)
p<-(m9/sum_m9)*100
p1<-p[1,]+p[2,]
p2<-p[3,]
p3<-p[4,]+p[5,]
p1
p2
p3

# Around 39% of the employees are not proud and positive about the organization
# Around 41% of the employees are proud and positive about the organization. 

