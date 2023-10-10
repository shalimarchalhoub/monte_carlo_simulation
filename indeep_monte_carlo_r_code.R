#Digging for Gold

rm(list=ls())

# The simulation uses the inverse transform method

inv_triangle_cdf <- function(P, imin, iml, imax){
  
  
  Piml <- (iml-imin)/(imax-imin)
  
  return(ifelse(P < Piml,
                imin + sqrt(P*(iml-imin)*(imax-imin)),
                imax - sqrt((1-P)*(imax-iml)*(imax-imin))))
}

#no of simulation trials
n=10000

#read in gold income data

gold_summary <- read.csv(file="gold_summary.csv", stringsAsFactors = F)
str(gold_summary)


#set seed for reproducibility
set.seed(42)

#create data frame for income with rows = number of trials and cols = number of years
y1sim <- as.data.frame(matrix(nrow=n,ncol=nrow(gold_summary)))

y2sim <- as.data.frame(matrix(nrow=n,ncol=nrow(gold_summary)))

y3sim <- as.data.frame(matrix(nrow=n,ncol=nrow(gold_summary)))

y4sim <- as.data.frame(matrix(nrow=n,ncol=nrow(gold_summary)))

y5sim <- as.data.frame(matrix(nrow=n,ncol=nrow(gold_summary)))

# Assign Variables in sheet
for (i in 1:nrow(gold_summary)){
  #set attributes for year1
  y1min <- gold_summary$Year1_min[i]
  y1ml <- gold_summary$Year1_ml[i]
  y1max <- gold_summary$Year1_max[i]
  
  #set attributes for year2
  y2min <- gold_summary$Year2_min[i]
  y2ml <- gold_summary$Year2_ml[i]
  y2max <- gold_summary$Year2_max[i]
  
  #set attributes for year3
  y3min <- gold_summary$Year3_min[i]
  y3ml <- gold_summary$Year3_ml[i]
  y3max <- gold_summary$Year3_max[i]
  
  #set attributes for year5
  y4min <- gold_summary$Year4_min[i]
  y4ml <- gold_summary$Year4_ml[i]
  y4max <- gold_summary$Year4_max[i]
  
  #set attributes for year5
  y5min <- gold_summary$Year5_min[i]
  y5ml <- gold_summary$Year5_ml[i]
  y5max <- gold_summary$Year5_max[i]
  
  
  #generate n random numbers (one per trial)
  psim <- runif(n)
  
  #simulate n instances of Year1
  y1sim[,i] <- inv_triangle_cdf(psim,y1min,y1ml,y1max) 
  
  #simulate n instances of Year2
  y2sim[,i] <- inv_triangle_cdf(psim,y2min,y2ml,y2max) 
  
  #simulate n instances of Year3
  y3sim[,i] <- inv_triangle_cdf(psim,y3min,y3ml,y3max) 
  
  #simulate n instances of Year4
  y4sim[,i] <- inv_triangle_cdf(psim,y4min,y4ml,y4max) 
  
  #simulate n instances of Year5
  y5sim[,i] <- inv_triangle_cdf(psim,y5min,y5ml,y5max) 
  
  
}

#Pure income for Year1

#Year 1 income = Budget + (Price of ounce * number of ounces )- 32% - annual cost - legal fees
Year1 = (2200000 +1800*y1sim[,2]*0.68 - y1sim[,3] - y1sim[,4])
Year1

#Statistics Regarding Year1
min(Year1)
max(Year1)
mean(Year1)
median(Year1)
sd(Year1)

#Getting 10% 20% 50% and 90%
quantile(ecdf(Year1),c(0.1,0.2, 0.5,0.25,0.9),type=7)

quantile(ecdf(Year1),0.754,type=7)

#Percentage of Finding Certain Values
data<-rnorm(n)
Year1ecdf <- ecdf(Year1)
inv_ecdf <- function(x)
{
  quantile(data,x)
}

#percentage of not having any gain more than opening budget
print(inv_ecdf(Year1ecdf(2200000)))

#percentage of losing 750000 from opening budget
print(inv_ecdf(Year1ecdf(1450000)))
#ECDF Plot
plot(ecdf(Year1))


#Histogram
hist(Year1, breaks=20, xlab="Pure Revenue",
     main= "Histogram of Year 1 Pure Revenue")



# Pure income for Year 2

#Year 1 income = Budget + (Price of ounce * number of ounces )- 32% - annual cost - legal fees
Year2 = (Year1 + y2sim[,1]*y2sim[,2]*0.68 - y2sim[,3] - y2sim[,4])

Year2

#Statistics Regarding Year2
min(Year2)
max(Year2)
mean(Year2)
median(Year2)
sd(Year2)

#Getting 10% 20% 50% and 90%
quantile(ecdf(Year2),c(0.1,0.25, 0.5,0.9),type=7)


#ECDF Plot
plot(ecdf(Year2))

#Histogram for Year 2
#No Scientific notations
options(scipen=999)
hist(Year2, breaks=20, xlab="Pure Revenue",
     main= "Histogram of Year 2 Pure Revenue")



#Pure income for Year3
Year3 = (Year2 + y3sim[,1]*y3sim[,2]*0.68 - y3sim[,3] - y3sim[,4])
Year3


#Statistics Regarding Year3
min(Year3)
max(Year3)
mean(Year3)
median(Year3)
sd(Year3)

#Getting 10% 20% 50% and 90%
quantile(ecdf(Year3),c(0.1,0.25, 0.5, 0.58, 0.9),type=7)

#percentage of having a revenue of the opening budget and annual fee + Legal fee
# + fund to Deploy 6 new research teams 70% 
print(inv_ecdf(Year3ecdf(6600000)))

#ECDF Plot
plot(ecdf(Year3))




# Pure income for Year 4 if new claim is found in Year3
#we will consider the new value of Year3 as $4.6M - $4.2M
Year3_Plan_A = 4600000-2400000

#Year4 Outcome A revenue will become the new Year3 Revenue + double the income given the new claim, minus legal and annual fees
Year4_OutcomeA = Year3_Plan_A + 2*(y4sim[,1]*y4sim[,2]*0.68) - y4sim[,3] - y4sim[,4]
Year4_OutcomeA

#Statistics Regarding Year4 Outcome A
min(Year4_OutcomeA)
max(Year4_OutcomeA)
mean(Year4_OutcomeA)
median(Year4_OutcomeA)
sd(Year4_OutcomeA)

#Getting 10% 20% 50% and 90%
quantile(ecdf(Year4_OutcomeA),c(0.25, 0.5, 0.8, 0.9),type=7)

#ECDF Plot
plot(ecdf(Year4_OutcomeA))


#Year4 Outcome B revenue will be on the basis that we didn't invest in new teams for Year3
#we have to invest in 6 teams so an expense of $2.4M
Year4_OutcomeB = Year3 + y4sim[,1]*y4sim[,2]*0.68 - y4sim[,3] - y4sim[,4] - 2400000 
Year4_OutcomeB
#Statistics Regarding Year4 Outcome B
min(Year4_OutcomeB)
max(Year4_OutcomeB)
mean(Year4_OutcomeB)
median(Year4_OutcomeB)
sd(Year4_OutcomeB)

#Getting 10% 20% 50% and 90%
quantile(ecdf(Year4_OutcomeB),c(0.25, 0.5 ,0.8, 0.9),type=7)

#ECDF Plot
plot(ecdf(Year4_OutcomeB))



# Pure income for Year 5 given Year4 Plan A
#First case is lawsuit is lost - remove legal cost and do not double wins
Year5_OutcomeA_Lost = Year4_OutcomeA + y5sim[,1]*y5sim[,2]*0.68 - y5sim[,3]
Year5_OutcomeA_Lost

#Statistics Regarding that year
min(Year5_OutcomeA_Lost)
max(Year5_OutcomeA_Lost)
mean(Year5_OutcomeA_Lost)
median(Year5_OutcomeA_Lost)
sd(Year5_OutcomeA_Lost)

#Getting 10% 20% 50% and 90%
quantile(ecdf(Year5_OutcomeA_Lost),c(0.1,0.25, 0.5,0.8,0.9),type=7)




#Second case is lawsuit is won - remove legal cost and double wins
Year5_OutcomeA_Won = Year4_OutcomeA + 2*y5sim[,1]*y5sim[,2]*0.68 - y5sim[,3]
Year5_OutcomeA_Won

#Statistics Regarding that year
min(Year5_OutcomeA_Won)
max(Year5_OutcomeA_Won)
mean(Year5_OutcomeA_Won)
median(Year5_OutcomeA_Won)
sd(Year5_OutcomeA_Won)

#Getting 10% 20% 50% and 90%
quantile(ecdf(Year5_OutcomeA_Won),c(0.1,0.25, 0.5,0.8,0.9),type=7)


#ecdf 
options(scipen=999)
plot(ecdf(Year5_OutcomeA_Won))

#histogram
options(scipen=999)
hist(Year5_OutcomeA_Won, breaks=20, xlab="Pure Revenue",
     main= "Histogram of Year 5 Pure Revenue")


# Pure income for Year 5 given Year4 Plan B
#First case is lawsuit is won but the teams didn't find a new claim
Year5_OutcomeB_Not_Found = Year4_OutcomeB + y5sim[,1]*y5sim[,2]*0.68 - y5sim[,3]
Year5_OutcomeB_Not_Found

#Statistics Regarding that year
min(Year5_OutcomeB_Not_Found)
max(Year5_OutcomeB_Not_Found)
mean(Year5_OutcomeB_Not_Found)
median(Year5_OutcomeB_Not_Found)
sd(Year5_OutcomeB_Not_Found)

#Getting 10% 20% 50% and 90%
quantile(ecdf(Year5_OutcomeB_Not_Found),c(0.1,0.25, 0.5,0.8,0.9),type=7)




#Second case is lawsuit is won and the teams found a new claim
Year5_OutcomeB_Found = Year4_OutcomeB + 2*y5sim[,1]*y5sim[,2]*0.68 - y5sim[,3]
Year5_OutcomeB_Found

#Statistics Regarding that year
min(Year5_OutcomeB_Found)
max(Year5_OutcomeB_Found)
mean(Year5_OutcomeB_Found)
median(Year5_OutcomeB_Found)
sd(Year5_OutcomeB_Found)

#Getting 10% 20% 50% and 90%
quantile(ecdf(Year5_OutcomeB_Found),c(0.1,0.25, 0.5,0.8,0.9),type=7)






