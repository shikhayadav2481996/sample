
 
####################  2 // 9  // 2020   #######################

#####################   BORUTA  ###################

install.packages('Boruta')
library(Boruta)

install.packages('mlbench')
library(mlbench)


path = "C:\\Users\\sakshi\\Downloads\\Sonar.csv"
Sonar = read.csv(path, header = T)
Sonar

head(Sonar)
dim(Sonar)
view(Sonar)
str(Sonar)


mdl_bor <- Boruta(Class~., data = Sonar, maxRuns = 450)

mdl_bor$finalDecision

mdl_bor$finalDecision[mdl_bor$finalDecision == 'Tentative']

plot(mdl_bor$finalDecision)
plot(mdl_bor)


# build the boruta on credit risk data and share the output on chat window 
# ess ka output share krna hai mdl_bor$finalDecision

install.packages('Boruta')
library(Boruta)

install.packages('mlbench')
library(mlbench)


path = "C:\\Users\\sakshi\\Downloads\\CreditRisk.csv"
cr = read.csv(path, header = T)
cr

head(cr)
dim(cr)
#view(cr)
str(cr)

Loan_Status~.-Loan_ID# to remove any column from the data set


# Change the data types according to the description

cr$Gender =as.factor(cr$Gender)
cr$Married=as.factor(cr$Married)
cr$Education =as.factor(cr$Education)
cr$Education =as.factor(cr$Education)
cr$Property_Area=as.factor(cr$Property_Area)











Cr$loadid = null# to remove any column from the data set
Loan_Status~.-Loan_ID# to remove any column from the data set





head(bbd)
View(bbd)
dim(bbd)
str(bbd)

# Change the data types according to the description

bbd$observation=as.factor(bbd$observation)
bbd$degree=as.factor(bbd$degree)
bbd$med_check=as.factor(bbd$med_check)
bbd$mst=as.factor(bbd$mst)
bbd$diagnosis=as.factor(bbd$diagnosis)

#Data Clensing/anamolies check

# split the data into numbers columns and factor columns.
#NuLL check on entire data
#zero check on numeric data
#Check which column has '.'

col1=c('stratum','age','school','agp1','agmn','nlv','liv','weight','aglp')
col2=c('observation','degree','med_check','mst','diagnosis')

colnames(col1)[apply(bbd,2,checkzero)]
colnames(col2)[apply(bbd,2,checknull)]

grep('.',bbd)

#Sir's Approach ( split the data into numbers columns and factor columns.)

numcols=colnames(bbd)[sapply(bbd,is.numeric)]
factcols=colnames(bbd)[sapply(bbd,is.factor)]
numcols
factcols

# check null
c=colnames(bbd)[apply(bbd,2,function(x) any(is.na(x)))]
if(length(c)==0)
  print("there are no nulls") else
    print(paste("NULLS in column",c))

# 0 check
c=colnames(bbd)[apply(bbd,2,function(x) any(x<=0))]
print(paste("0's present in column",c))

#'.' Check


for(c in factcols)
{
  if(any(bbd[c][bbd[c]="."])==T)
    print(paste('Columns',c,'contains.'))
}

#
#Check for multicollinearity
corr=cor(bbd[numcols])
corr

corrplot(corr,method="number",type="lower")


#Check for outliers


boxplot(bbd$stratum, horizontal = T,main = title, col= "yellow")
boxplot(bbd$observation, horizontal = T,main = title, col= "blue")
boxplot(bbd$age, horizontal = T,main = title, col= "red")
boxplot(bbd$school, horizontal = T,main = title, col= "green")
boxplot(bbd$degree, horizontal = T,main = title, col= "pink")
boxplot(bbd$med_check, horizontal = T,main = title, col= "light blue")
boxplot(bbd$agp1, horizontal = T,main = title, col= "yellow")
boxplot(bbd$agmn, horizontal = T,main = title, col= "yellow")
boxplot(bbd$nlv, horizontal = T,main = title, col= "yellow")
boxplot(bbd$liv, horizontal = T,main = title, col= "yellow")
boxplot(bbd$weight, horizontal = T,main = title, col= "yellow")
boxplot(bbd$aglp, horizontal = T,main = title, col= "yellow")
boxplot(bbd$mst, horizontal = T,main = title, col= "yellow")
boxplot(bbd$diagnosis, horizontal = T,main = title, col= "yellow")













