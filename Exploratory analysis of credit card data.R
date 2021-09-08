# Table 1 Enter your R code you used to import the data here
MyURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data"
Data <- read.table(MyURL)

crx<-read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",
                 header = FALSE, sep = ",", quote = "\"'", dec = ".",
                 na.strings = "?", stringsAsFactors = TRUE, 
                 comment.char = "") # reads a file in table format and creates data frame from it 
View(crx)
str(crx)
typeof(crx)
length(crx)
is.list(crx)
class(crx)
dim(crx)
nrow(crx)
ncol(crx)
sapply(crx, class)

# (b)Variable names and types
# Because the data file does not contain the variable names, we will need to explicitly set them using the R function names().
# Use this R code to add names to the dataset
names(crx) <- c("Gender", "Age", "MonthlyExpenses", "MaritalStatus", "HomeStatus", 
                 "Occupation", "BankingInstitution", "YearsEmployed", 
                 "NoPriorDefault", "Employed", "CreditScore", 
                 "DriversLicense", "AccountType", "MonthlyIncome", "AccountBalance", "Approved")

crx$Gender # We usually use the $ operator to address a column by name
crx[["Gender"]] # As data frames are also lists, it is possible to refer to columns (which are elements of such list) using the list notation, 
#i.e. either double square brackets or a $.
head(crx[, c("Gender", "Age", "MonthlyExpenses", "MaritalStatus", "HomeStatus", 
              "Occupation", "BankingInstitution", "YearsEmployed", 
              "NoPriorDefault", "Employed", "CreditScore", 
              "DriversLicense", "AccountType", "MonthlyIncome", "AccountBalance", "Approved")]) #If the data is very large, we can apply head () to shows first 6 rows
# or tail() function to view the lats 6 rows. 

crx[3,2] #Because data frames are rectangular, elements of data frame can be referenced by specifying the row and the column 
# index in single square brackets (similar to matrix).

# When the data is imported using the function read.table(), the variable type is automatically assigned. 
# Data types can also be manually assigned and may need to be re-assigned to perform some types of numerical analysis 
#– such as re-coding a two-level factor into a numeric binary variable, for example. Table 3 Use the following R code to manually define the variables
crx$Gender <- as.factor(crx$Gender)
crx$Gender<-factor(crx$Gender, levels = c("a","b"), labels = c("M", "F"))
crx$Age <- as.numeric(crx$Age)
crx$MonthlyExpenses <- as.integer(crx$MonthlyExpenses)
crx$MaritalStatus <- as.factor(crx$MaritalStatus)
crx$MaritalStatus<-factor(crx$MaritalStatus, levels = c("l","u","y"), labels = c("Married", "Single", "Other"))
crx$HomeStatus <- as.factor(crx$HomeStatus)
crx$HomeStatus<-factor(crx$Hom, levels = c("g","gg","p"), labels = c("Renting", "Own/Buying", "Relatives"))
crx$Occupation <- as.factor(crx$Occupation)
crx$BankingInstitution <- as.factor(crx$BankingInstitution)
crx$YearsEmployed <- as.numeric(crx$YearsEmployed)
crx$NoPriorDefault <- as.factor(crx$NoPriorDefault)
crx$NoPriorDefault<-factor(crx$NoPriorDefault, levels = c("f","t"), labels = c("Defaulted-before", "Never-defaulted-before"))
crx$NoPriorDefault
crx$Employed <- as.factor(crx$Employed)
crx$Employed<-factor(crx$Employed, levels = c("f","t"), labels = c("Unemployed", "Employed"))
crx$CreditScore <- as.numeric(crx$CreditScore)
crx$DriversLicense <- as.factor(crx$DriversLicense)
crx$DriversLicense<-factor(crx$DriversLicense, levels = c("f","t"), labels = c("No", "Yes"))
crx$AccountType <- as.factor(crx$AccountType)
crx$AccountType<-factor(crx$AccountType, levels = c("g","p","s"), labels = c("Savings", "Deposit","Current"))
crx$MonthlyIncome <- as.integer(crx$MonthlyIncome)
crx$AccountBalance <- as.numeric(crx$AccountBalance)
crx$Approved <- as.factor(crx$Approved)
crx$Approved<-factor(crx$Approved, levels = c("-","+"), labels = c("No", "Yes"))
str(crx)

# Records with missing values. There are also a few missing values. 
# For this project, observations with missing values need to be removed.  
# To remove observations with missing values, use the R function na.omit().
crx_omit<-na.omit(crx) 
crx_omit

# In the original data, how many missing values in total are there? Hint: use the function
# summary().  How many records are removed by using the function na.omit()?
summary(crx_omit) 

#To find all the rows in a data frame with at least one NA, try 
#this:lapply() applies the function to each column and returns a list whose i-th element is a vector containing the indices of the elements which have missing values in column i. unlist() turns that list into a vector and unique() gets rid of the duplicates. To learn more about lapply(), see the apply family of functions.
Rows<-unique (unlist (lapply (crx, function (crx) which (is.na (crx)))))
length(Rows)
Rows<-unique (unlist (lapply (crx_omit, function (crx_omit) which (is.na (crx_omit)))))

Colomns<-sapply(crx, function(crx) sum(is.na(crx))) # number of missing values by coloumn
Colomns<-sapply(crx_omit, function(crx_omit) sum(is.na(crx_omit))) # number of missing values by coloumn
Colomns

sum(is.na(crx))
sum(is.na(crx_omit))

summary(crx) 
summary(crx_omit)
str(crx_omit)

# The dataset contains variables with mixed types. Use R function daisy() from package cluster to compute a 
# Gower dissimilarity (distance) matrix between the data records, and refer to the result as “Dist”.  
library(cluster)
is.data.frame(crx_omit)
Dist<-daisy(crx_omit, metric = "gower")

# Use the R code to convert the Gower dissimilarity object into a distance matrix
Dist_matrix<-round(as.matrix(Dist) [1:100, 1:10], 2)
Dist_matrix
head(Dist_matrix, 10) # show the distances for the first 10 rows

# Using the new distance matrix, what is the Gower similarity measure between the 10th and the 60th observation (row)? 
Dist_matrix[c(10,60),]
Dist_matrix<-as.matrix(Dist)

# Use the following R code to visualise the distance matrix
dim <- ncol(Dist_matrix)  # used to define axis in image
image(1:dim, 1:dim, Dist_matrix, axes = FALSE, xlab="", ylab="", col = rainbow(100))

heatmap(Dist_matrix, Rowv=TRUE, Colv="Rowv", symm = TRUE)

# Enter your R code used to calculate the Pearson and then the Spearman correlation matrices 
#using all numerical variables.
Age<-c(crx_omit$Age)
YearsEmployed<-c(crx_omit$YearsEmployed)
CreditScore<-c(crx_omit$CreditScore)
AccountBalance<-c(crx_omit$AccountBalance)
cbind(Age, YearsEmployed, CreditScore, AccountBalance) 

mydata<-cbind(Age, YearsEmployed, CreditScore, AccountBalance)
class(mydata)
typeof(mydata)

mydata_cor<-cor(mydata, method = c("spearman"))
mydata_cor

mydata_cor<-cor(mydata, method = c("pearson"))
mydata_cor

#Use the ggplot2 library to produce box plots for AccountBalance, MonthlyExpenses, CreditScore and Age 
#segmented by approval (variable “Approved”). 
#Insert the R codes and resulting images into the table below.
library(ggplot2)
                
outliers<-boxplot.stats(crx_omit$AccountBalance)$out
length(outliers)
crx_omit$AccountBalance[crx_omit$AccountBalance%in%outliers]<-
  median(crx_omit$AccountBalance)
crx_omit$AccountBalance[crx_omit$AccountBalance%in%outliers] 

ggplot(data = crx_omit) + aes(x=Approved, y=AccountBalance) + 
  aes(color = Approved) + geom_boxplot(outlier.size = 2,
                                       outlier.color = "red", outlier.shape = 3) + 
  geom_jitter(width = 0.1, alpha = 0.05, color = "blue")

ggplot(data = crx_omit) + aes(x=Approved, y=MonthlyIncome) + 
  aes(color = Approved) + geom_boxplot(outlier.size = 2,
                                       outlier.color = "red", outlier.shape = 3) + 
  geom_jitter(width = 0.1, alpha = 0.05, color = "blue")

outliers<-boxplot.stats(crx_omit$MonthlyIncome)$out
length(outliers)
crx_omit$AccountBalance[crx_omit$MonthlyIncome%in%outliers]<-
  median(crx_omit$MonthlyIncome)
crx_omit$AccountBalance[crx_omit$MonthlyIncome%in%outliers] 

ggplot(data = crx_omit) + aes(x=Approved, y=MonthlyIncome) + 
  aes(color = Approved) + geom_boxplot(outlier.size = 2,
                                       outlier.color = "red", outlier.shape = 3) + 
  geom_jitter(width = 0.1, alpha = 0.05, color = "blue")
                
# Box plot for Approved versus MonthlyExpense
outliers<-boxplot.stats(crx_omit$MonthlyExpenses)$out
length(outliers)
                
crx_omit$MonthlyExpenses[crx_omit$MonthlyExpenses%in%outliers]<-
  median(crx_omit$MonthlyExpenses)
crx_omit$MonthlyExpenses[crx_omit$MonthlyExpenses%in%outliers] 

ggplot(data = crx_omit) + aes(x=Approved, y=MonthlyExpenses) + 
  aes(color = Approved) + geom_boxplot(outlier.size = 2,
                                       outlier.color = "red", outlier.shape = 3) + 
  geom_jitter(width = 0.1, alpha = 0.05, color = "blue")
                
# Box plot of  CreditScore versus Approved
outliers<-boxplot.stats(crx_omit$CreditScore)$out
length(outliers)
crx_omit$CreditScore[crx_omit$CreditScore%in%outliers]<-
  median(crx_omit$CreditScore)
crx_omit$CreditScore[crx_omit$CreditScore%in%outliers]
                
ggplot(data = crx_omit) + aes(x=Approved, y=CreditScore) + 
  aes(color = Approved) + geom_boxplot(outlier.size = 2,
                                       outlier.color = "red", outlier.shape = 3) + 
  geom_jitter(width = 0.1, alpha = 0.05, color = "blue")
                
# Box plot of  Age versus Approved
outliers<-boxplot.stats(crx_omit$Age)$out
length(outliers)
crx_omit$Age[crx_omit$Age%in%outliers]<-median(crx_omit$CreditScore)
crx_omit$Age[crx_omit$Age%in%outliers]
                
ggplot(data = crx_omit) + aes(x=Approved, y=Age) + 
  aes(color = Approved) + geom_boxplot(outlier.size = 2,
                                       outlier.color = "red", outlier.shape = 3) + 
  geom_jitter(width = 0.1, alpha = 0.05, color = "blue")
                
#Use the ggplot2 library to produce bar plots for Employed, MaritalStatus, BankingInstitution, and NoPriorDefault, all segmented by approval (variable “Approved”). 
#Insert the R codes and resulting images into the table below.
ggplot(data = crx_omit) + 
  geom_bar(mapping = aes(x = Employed, fill = Approved), color="white")
                
ggplot(data = crx_omit) + 
  geom_bar(mapping = aes(x = Employed, fill = Approved), color="white", position = "fill")
                
ggplot(data = crx_omit) + 
  geom_bar(mapping = aes(x = MaritalStatus, fill = Approved), color="white", position = "fill")
                
ggplot(data = crx_omit) + 
  geom_bar(mapping = aes(x = BankingInstitution, fill = Approved), color="white", position = "fill")
                
ggplot(data = crx_omit) + 
  geom_bar(mapping = aes(x = NoPriorDefault, fill = Approved), color="white", position = "fill")
                
# Use the function table() and calculate the Simple Matching Coefficient (SMC) 
# between NoPriorDefault   and   Approved,   assuming   that   values   “f”   (False)   and   “t”   (True)   for
# NoPriorDefault are associated with “-“ and “+” for Approved, respectively. Discuss the interpretation of the SMC in this scenario. 
# Is Jaccard meaningful in this case?
library(tidyverse)
library(dplyr)

my_data<-as_tibble(crx_omit) # converting into tibble data frame.
mydata_con<-my_data %>% select(9, 16)
my_data_con<-mydata_con %>% slice(1:50)
Xi<-factor(my_data_con$NoPriorDefault, levels = c("Never-defaulted-before","Defaulted-before"), labels = c("0", "1"))
Xi
Xj<-factor(my_data_con$Approved, levels = c("Yes","No"), labels = c("0", "1"))
Xj
CT<-table(Xi,Xj)
CT
SSC <- (CT[1,1]+CT[2,2])/sum(CT)
SSC
1/SSC-1 #disimilariy
                
# The simple matching coefficient (Sokal, 1958) represents the simplest way for measuring of similarity. It does not impose any weigts. By a given variable, 
# it assigns value 1 in case of match and value 0 otherwise. Hierarchical clustering methods require a proximity (dissimilarity) matrix instead of a similarity
# matrix as an entry for the analysis; therefore, dissimilarity D is computed from similarity S according the equation 1/S-1.

# Further explore this dataset (freely), possibly using other types of plots, such as histograms and 
# scatter plots, and try to obtain further insights and hypotheses. For instance, is there any interesting relationship 
# between monthly disposable income (“MonthlyIncome”) and approval (“Approved”), in particular when compared to 
# what you would in principle expect (if anything)? Share your main insights / hypotheses (no more than 1 page).
# with color

#MonthlyIncome and Approved
ggplot(data = crx_omit, mapping = aes(x = MonthlyIncome)) + geom_histogram(binwidth = 200, color = "white",
                                                                   fill = "purple") + theme_minimal() + 
  facet_grid(Approved ~ .) + coord_cartesian(xlim = c(0, 1000))
                
#MonthlyIncome$Gender
ggplot(data = crx_omit, mapping = aes(x = MonthlyIncome)) + geom_histogram(binwidth = 200, color = "white",
                                                                           fill = "red") + theme_minimal() + 
  facet_grid(Gender ~ .) + coord_cartesian(xlim = c(0, 1000))

ggplot(data = crx_omit, mapping = aes(x = MonthlyIncome)) + geom_histogram(binwidth = 30, color = "white",
                                                                           fill = "red") + theme_minimal() + 
  facet_grid(Occupation ~ .) + coord_cartesian(xlim = c(0, 1000))
ggplot(data = crx_omit, mapping = aes(x = YearsEmployed)) + geom_histogram(binwidth = 30, color = "white",
                                                                           fill = "red") + theme_minimal() + 
  facet_grid(HomeStatus ~ .) + coord_cartesian(xlim = c(0, 30))
                
# BoxPlots
ggplot(data = crx_omit) + aes(x=Approved, y=MonthlyIncome) + 
  aes(color = Approved) + geom_boxplot(outlier.size = 2,
                                       outlier.color = "red", outlier.shape = 3) + 
  geom_jitter(width = 0.1, alpha = 0.05, color = "blue")

ggplot(data = crx_omit) + aes(x=Gender, y=MonthlyIncome) + 
  aes(color = Gender) + geom_boxplot(outlier.size = 2,
                                       outlier.color = "red", outlier.shape = 3) + 
  geom_jitter(width = 0.1, alpha = 0.05, color = "blue")

outliers<-boxplot.stats(crx_omit$MonthlyIncome)$out
length(outliers)
crx_omit$AccountBalance[crx_omit$MonthlyIncome%in%outliers]<-
  median(crx_omit$MonthlyIncome)
crx_omit$AccountBalance[crx_omit$MonthlyIncome%in%outliers] 


