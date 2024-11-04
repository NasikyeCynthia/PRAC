#Setting the working directory
setwd("C:/Users/ADMIN/Desktop/Semester exam")
library(readxl)
Question1 <- read_excel("Question1.xlsx")
View(Question1)
data <- Question1

######Descriptive stastics of all the variables in dataset
summary(data)
#  sr             pop15           pop75            dpi               ddpi       
#Min.   : 0.600   Min.   :21.44   Min.   :0.560   Min.   :  88.94   Min.   : 0.220  
#1st Qu.: 6.970   1st Qu.:26.21   1st Qu.:1.125   1st Qu.: 288.21   1st Qu.: 2.002  
#Median :10.510   Median :32.58   Median :2.175   Median : 695.66   Median : 3.000  
#Mean   : 9.671   Mean   :35.09   Mean   :2.293   Mean   :1106.76   Mean   : 3.758  
#3rd Qu.:12.617   3rd Qu.:44.06   3rd Qu.:3.325   3rd Qu.:1795.62   3rd Qu.: 4.478  
#Max.   :21.100   Max.   :47.64   Max.   :4.700   Max.   :4001.89   Max.   :16.710
#The information above shows the 1st quartile which is 25% of the data, median which is the number in between, mean which is the average of all the data gotten, 3rd quartile which is 75% of the data, max whic the highest number in the data

#Checking for missing data 
sum(is.na(data))
#There is no missing data in the dataset

#Checking for outliers for all the data
outliers_sr <- boxplot.stats(data$sr)$out  
outliers_pop15 <- boxplot.stats(data$pop15)$out 
outliers_pop75 <- boxplot.stats(data$pop75)$out 
outliers_dpi <- boxplot.stats(data$dpi)$out 
outliers_ddpi <- boxplot.stats(data$ddpi)$out 
#Only ddpi has outliers that is 10.2 qnd 16.7
boxplot(data$ddpi, main="ddpi with outliers", boxwex=0.1)

#Transforming data set by removing outliers and handling missing data
#Removing the outliers in ddpi using thresholds
IQR <- IQR(data$ddpi)
#The IQR is 2.475
Tmin =2.002 - (1.5 * 2.475)
Tmax = 4.478 + (1.5 * 2.475)
without <- data$ddpi[which(data$ddpi > Tmin & data$ddpi < Tmax)]
#Visualising after removing otliers
boxplot(without, main="Boxplot showing ddpi without outliers", boxwex=0.1)

#Transforming data set and saving it as csv
write.csv(data, "Q1_nasikye.csv", row.names = FALSE)
view(Q1_nasikye)


#Generating a graph showing distribution of pop15
#Using a boxplot
library(ggplot2)
ggplot(data = data, mapping = aes(y = pop15)) +
  geom_boxplot() +
  labs(y="pop15", 
       title="Distribution of pop15")


##################################QUESTION 2########################################################################################
#Importing the dataset from same directory
library(readxl)
Question2 <- read_excel("Question2.xlsx")
View(Question2)
data2 <- Question2

#Generate a transformed dataset and label it as Q2_lastname
#Check for missing data 
sum(is.na(data2))
#There is no missing data in the dataset

sapply(data2,class)

#Check for outliers
outliers_sesn <- boxplot.stats(data2$Sesn)$out
outliers_locn <- boxplot.stats(data2$locn)$out
outliers_block <- boxplot.stats(data2$block)$out
outliers_rep <- boxplot.stats(data2$rep)$out
outliers_ph <- boxplot.stats(data2$Plants_harvested)$out
outliers_nbt <- boxplot.stats(data2$No_bigtubers)$out
#No bigtubers has 13 outliers
outliers_wbt <- boxplot.stats(data2$Weigh_bigtubers)$out
#Weigh_bigtubers has 17 outliers
outliers_nmt <- boxplot.stats(data2$No_mediumtubers)$out
#No_mediumtubers has 1 outlier
outliers_wmt <- boxplot.stats(data2$Weight_mediumtubers)$out
#Weight_mediumtubers has 1 outlier
outliers_nst <- boxplot.stats(data2$No_smalltubers)$out
outliers_wst <- boxplot.stats(data2$Weight_smalltubers)$out
outliers_ttn <- boxplot.stats(data2$Totaltuberno)$out
outliers_atp <- boxplot.stats(data2$AV_tubers_Plant)$out
outliers_ttw <- boxplot.stats(data2$Total_tubweight)$out
outliers_ps <- boxplot.stats(data2$plotsize)$out
outliers_HEC <- boxplot.stats(data2$HEC)$out
outliers_twph <- boxplot.stats(data2$TotalWeightperhectare)$out
#totalweightperhectare ha 4 outliers
outliers_ttph <- boxplot.stats(data2$TotalTuberperHectare)$out

###The data without comments have no outliers
detect_outlier <- function(x) {
  Quantile1 <- quantile(x, probs=.25) 
  Quantile3 <- quantile(x, probs=.75)
  IQR = Quantile3 - Quantile1
  x > Quantile3 + (IQR * 1.5) | x < Quantile1 - (IQR * 1.5)
}

remove_outlier <- function(dataframe, columns = names(dataframe)) {
  for (col in columns) {
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  
  print("Remove outliers")
  print(dataframe)
}

new_data = remove_outlier(data2, c('No_bigtubers', 'Weigh_bigtubers', 'No_mediumtubers', 'Weight_mediumtubers', 'TotalWeightperhectare'))

#Save the transformed dataset
write.csv(new_data, file = "Q2_nasikye.xlsx")
Q2_nasikye

#District with highest average "totaltuberno" of cassava
max(new_data$Totaltuberno)#Maximum totaltuberno is 443
#finding district with this number
high_ave = filter(new_data, new_data$Totaltuberno == 443)
#Soroti district has the highest average of totaltuberno

#d)
#finding relationship between plants harvested and fertliliser
#Carryout a stastical test for a categorical variable and a continuous variable
onewayanova <- aov(Plants_harvested ~ ferT, data = new_data)
summary(onewayanova)
#Df Sum Sq Mean Sq F value Pr(>F)
#ferT          4     11    2.87   0.064  0.992

library(ggplot2)
#visualing using boxplot
ggplot(data = new_data, mapping = aes(x = ferT, y = Plants_harvested)) +
  geom_boxplot() +
  labs(x="ferT",   
       y="Plants_harvested",
       title="How fertilisation affects plants harvested")
# I would advise farmers to use F3200

