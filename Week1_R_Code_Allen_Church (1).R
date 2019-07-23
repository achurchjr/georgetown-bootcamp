#Read data into a dataframe and view head of the data
filename <- "TitanicDataset.csv"
TitanicDF <- read.csv(filename, na.strings = c(""))

#Print the head of the TitanicDF
head(TitanicDF, n=9)

#Change Survived column to ordered factor type
TitanicDF$Survived <- factor(TitanicDF$Survived, ordered = is.ordered(TitanicDF$Survived))
TitanicDF$Survived <- as.factor(TitanicDF$Survived)

#Change Pclass column to ordered factor type
TitanicDF$Pclass <- factor(TitanicDF$Pclass, ordered = is.ordered(TitanicDF$Pclass))
TitanicDF$Pclass <- as.factor(TitanicDF$Pclass)

#Change Name column to character type 
TitanicDF$Name <- as.character(TitanicDF$Name)

#Change Embarked column to character type 
TitanicDF$Embarked <- as.character(TitanicDF$Embarked)

#Verify changed data types
str(TitanicDF)

#Data Cleaning Section

#Remove PassengerId column and verify it has been removed
#We can remove this column and instead use indexing if we need to uniquely identify or count passengers.
#It does not seem necessary, and instead would add unnecessary data for model training.
TitanicDF <- subset(TitanicDF, select = -c(PassengerId))

#Remove Name, Cabin, and Ticket columns from dataframe, verify columns were removed
TitanicDF <- subset(TitanicDF, select = -c(Name, Cabin, Ticket))
str(TitanicDF)

#Round Fare column to 2 decimal places and replace original values with rounded values
TitanicDF$Fare <- round(TitanicDF$Fare, digits = 2)

#Examine first 10 rows of new TitanicDF
head(TitanicDF, n = 10)

#Tell user how many total rows in Titanic dataset
cat("The Titanic dataset has a total of", nrow(TitanicDF)," rows.")

#Tell user how many complete, non-NA, rows in Titanic dataset
cat("The Titanic dataset has", nrow(TitanicDF[complete.cases(TitanicDF),]), "complete (non-NA) rows.")

#Remove all NA rows and verify removal
TitanicDF <- na.omit(TitanicDF)
nrow(TitanicDF)

#Extra step to verify there are no NA values
is.na.data.frame(TitanicDF)

#Create FamilyNum column by adding SibSp + Parch columns, remove SibSp and Parch columns, and verify
TitanicDF$FamilyNum <- TitanicDF$SibSp + TitanicDF$Parch
TitanicDF <- subset(TitanicDF, select = -c(SibSp, Parch))
str(TitanicDF)

#Exploratory Data Analysis (EDA) Part 1
#Look at head of dataframe
View(head(TitanicDF))

#Assigns column names from dataframe to ColNames, to be used in function argument below
ColNames <- names(TitanicDF)

#Create function with one argument, intended to take ColNames as argument input
myfun <- function(a) {
  #Create for loop to iterate through each column in dataframe and print table for each column
  for(col in colnames(TitanicDF)){
    print(paste0("This is a table for column ", col))
    print(table(TitanicDF[col]))
  }
}

#Call myfun function, and pass all column names from dataframe as argument
#This will iterate through each column in dataframe, and print a table for each
myfun(ColNames)

#Create function that takes column names and determine if each column is numeric
#If numeric, print mean, median, mode, var, range. If not numeric, print mode

myfun2 <- function(b) {
  for (varname in ColNames) {
    #only check numeric variables
    if (sapply(TitanicDF[varname], is.numeric)){
      cat("\n", varname, " is numeric\n")
      #get mean
      (Themean <- sapply(TitanicDF[varname], FUN = mean))
      #print mean and column name
      print(paste0("The mean of column ", varname, " is ", Themean))
      #get median
      (Themedian <- sapply(TitanicDF[varname], FUN = median))
      #print median and column name
      print(paste0("The median of column ", varname, " is ", Themedian))
      #get mode
      (Themode <- sapply(TitanicDF[varname], FUN = Mode))
      #print mode
      print(paste0("The mode of column ", varname, " is ", Themode))
      #get variance
      (Thevariance <- sapply(TitanicDF[varname], FUN = var))
      #print variance
      print(paste0("The variance of column ", varname, " is ", Thevariance))
      #get range
      #get min
      (Themin <- sapply(TitanicDF[varname], FUN = min))
      #print min
      print(paste0("The min of column ", varname, " is ", Themin))
      #get max
      (Themax <- sapply(TitanicDF[varname], FUN = max))
      #print max
      print(paste0("The max of column ", varname, " is ", Themax))
    }
    #when variable is non-numeric
    else {
      cat("\n", varname, " is not numeric\n")
      print(paste0("The mode of column ", varname, " is ", Themode))
    }
  }
}

#Test function
myfun2(ColNames)


#Generic function to calculate Mode from https://www.r-bloggers.com/computing-the-mode-in-r/
Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

#Create function that takes age and sex column and performs independent samples t-test
#Age column is numeric and Sex column is binary factor
myfun3 <- function(age, sex){
  t.test(TitanicDF$Age~TitanicDF$Sex)
}

#Test function
myfun3(TitanicDF$Age, TitanicDF$Sex)

#Visual EDA Section


#Install ggplot2
install.packages("ggplot2")

library(ggplot2)

#Creates frequency bar chart of survived column
plot1 <- ggplot(TitanicDF, aes(x=Survived)) + geom_histogram(stat='count', color='black',fill='red',binwidth = 7) + coord_flip() + labs(title = 'Survived histogram plot')
print(plot1)


#Creates horizontal bar chart of Pclass column
plot2 <- ggplot(TitanicDF, aes(x=Pclass)) + geom_bar(fill='blue') + coord_flip() + labs(title="Count of Passengers by Class")
print(plot2)

#Creates pie chart of sex column
bp <- ggplot(TitanicDF, aes(x=Sex, fill=Sex)) + geom_bar(width = 1) + labs(title="Pie Chart of Sex Column")
pie <- bp + coord_polar("y", start=0) + labs(title="Pie Chart of Sex Column")
pie

#histogram of age column
plot4 <- ggplot(TitanicDF, aes(x=Age)) + geom_histogram(color='black',fill='light blue',binwidth = 7) + labs(title = 'Age histogram plot')
print(plot4)

#vis of fare column
plot5 <- ggplot(TitanicDF, aes(x=Fare)) + geom_area(stat = 'bin', binwidth=25, color='black', fill='green') + labs(title = 'Area Plot of Fare')
print(plot5)

#vis of embarked column
plot6 <- ggplot(TitanicDF, aes(x=Embarked)) + geom_bar(color='black', fill='pink') + labs(title = 'Bar Plot of Embarked Column')
print(plot6)

#vis of familynum column
plot7 <- ggplot(TitanicDF, aes(x=FamilyNum)) + geom_count(stat='count', color='light blue') + labs(title = 'Plot of Family Number')
print(plot7)

#Fare correlates to Pclass, using a scatterplot
plot8 <- ggplot(TitanicDF, aes(x=Pclass, y=Fare)) + geom_point(aes(col=Pclass)) + labs(title="Fare as Related to Pclass")
print(plot8)

#Survival by Gender
plot9 <- ggplot(TitanicDF, aes(Sex, Survived)) + geom_count(col='red', show.legend=F) + labs(title='Survival by Gender')
print(plot9)

#Feature Generation
age <- TitanicDF$Age

#Create new AGEBIN column with below categories based on age range
TitanicDF$AGEBIN[age >= 0 & age <=11] <- "Child"
TitanicDF$AGEBIN[age >= 12 & age <=19] <- "Teen"
TitanicDF$AGEBIN[age >= 20 & age <=45] <- "Adult"
TitanicDF$AGEBIN[age >= 46] <- "Late"

#Verify change
View(TitanicDF)

write.csv(TitanicDF, file = 'Allen_Church_Week1_Titanic_Dataset.csv')
