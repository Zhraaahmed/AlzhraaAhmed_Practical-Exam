#1)Read the Data set
setwd('C:/Users/Ayaa Ahmed/Desktop/امتحان العملي')
G3 <- read.csv('G3_sydney_hobart_times.csv')
#2) View Dataset after any change
View(G3)
# Data Pre_processing and Data Analysis Steps Are :
#########1)Data Collection 2)Data Cleaning 3)Processing 4)Modeling 5)Visualization
# Firstly, I'll Start With Data Cleaning and Data Transformation As I Convert Variables From One Format Or Structure Into Another During My Work To Be Suitable For Analysis And Modeling
#3)Get The Summary Of Data To Know if there's Incorrect Types
str(G3)
#After Viewing of The Structure of Data set I noticed That Time Variable Needs A lot of Cleaning;
#4) Removing the Text 'day' on Time Variable 
G3$Time<- gsub("day",'',G3$Time)

#5)Check It's Type
typeof(G3$Time)       #It Still Character So I'll Convert it into "Double" because it contains Decimal Point
G3$Time <- as.numeric(G3$Time)
typeof(G3$Time) 
#6) Now, It's The Time To Complete NAs In Time Variable 
# I'll Compute Their Median And Replace NA With The Median Value, With Removing NA From Variable
med<- median(G3[,'Time'],na.rm = T)
#7)I'll Use is.na To get NA Rows And Filling Them With med Value
G3[is.na(G3$Time),'Time']<-med
#############################"Now Time Variable Is Done Successfully"#########################################################
#Let Us Give A second Look On Dataset after Changes
View(G3)
#Let Us Now Start Working On fleet_start Variable
#8) Sorting it 
fleet_start<- G3[order(G3$fleet_start),]
#9) Sorting Data set based on fleet_start and fleet_finish together 
fleet_start_finish_sorted<- G3[order(G3$fleet_start,G3$fleet_finish),]
fleet_start_finish_sorted
View(G3)
#10) Recoding Fleet_start Column based on Ifelse Condition and to simplify analysis convert into factor
G3$Coded_fleet_start<- as.factor(ifelse(G3$fleet_start<=50,"Early Start","Late Start"))
##########################################"Now fleet_start Variable Is Done Successfully"#####################################
#Let Us Give A Third Look On Dataset after Changes
View(G3)
#Let Us Now Start Working On fleet_finish Variable
#11) Recoding fleet_finish Column based on MultiCondition
G3$Coded_fleet_finish[G3$fleet_finish <= 20]<- "Ended Early"
G3$Coded_fleet_finish[G3$fleet_finish > 20&G3$fleet_finish <200]<- "Reasonable End"
G3$Coded_fleet_finish[G3$fleet_finish>= 200]<- "Late End"
#12) Re-code of the code
G3$Coded_fleet_finish_Recode[G3$Coded_fleet_finish=='Ended Early']<- "1"
G3$Coded_fleet_finish_Recode[G3$Coded_fleet_finish=='Reasonable End']<- "2"
G3$Coded_fleet_finish_Recode[G3$Coded_fleet_finish=='Late End']<- "3"
View(G3)
#13)Define Ordinal Factor
OrderedFactor<- factor(G3$Coded_fleet_start,ordered=T,levels = c("Early Start","Late Start"))
#14)Compare Between Ordinal Factor elements
ord1<- OrderedFactor[3]
ord2<- OrderedFactor[10]
ord1> ord2
#15)Applying lapply On G3 elements And Get List Of Their Types
ls<- lapply(G3,class)
ls
#16) Compute New Variable From an existent one
G3$Fleet_Duration <- abs(G3$fleet_finish-G3$fleet_start)
View(G3)
#17)Use tidyr package to drop all of NAs from the Dataset
install.packages("tidyr")
library(tidyr)
clean<- drop_na(G3)
clean
#18)Sort Row index
rownames(G3)<- NULL
View(G3)
##########################################"Now fleet_finish Variable Is Done Successfully"######################################
#   I'll Now Look If Dataset Contain any more NAs
#19)Get all Rows That Contain NA
G3[!complete.cases(G3),]
#20)Get all Rows That Don't Contain NA 
G3[complete.cases(G3),]
#21)Get all Rows That Contain NA at specific Variable
G3[is.na(G3$fleet_start),]
# I noticed That Time.less.than.3 Contains NAs In all Of Its Rows
#22) Complete it Based on Time Variable
G3$Code.Time.less.than.3<-as.factor(ifelse(G3$Time<3,"Yes","No"))
#23) Add Description to Factor Levels
levels(G3$Code.Time.less.than.3)<-c(0,1)
########################################################
#24) Convert Year Var into Vector
year_vec <- c(G3$Year)
print(year_vec)
#25)print Type 
typeof(year_vec)
#26)Get Minimum element in the vector
mn <- min(year_vec)
mn
#27)Get Maximum element in the vector
mx<- max(year_vec)
mx
##########################################"Now Time.less.than.3 Variable Is Done Successfully"######################################
#28) Get Mean Of Time Less Than Three and Greater Than Three
m1<- mean(G3$Code.Time.less.than.3==0)
m2<- mean(G3$Code.Time.less.than.3==1)
m1
m2
#29) Get Min Of fleet_start
minimum<- min(G3$fleet_start)
minimum
#30) Get Max Of fleet_finish
maximum <- max(G3$fleet_finish)
maximum
#31) Get Early Start Where Code less Than 3 Partial Data Frame 
g3_Part <- G3[G3$Coded_fleet_start=='Early Start'&G3$Code.Time.less.than.3==0,]
View(g3_Part)
#32) Get All Rows Where The Year is 2000 or More
g3_Part_2 <- G3[G3$Year>=2000,]
View(g3_Part_2)
#33) Get fleet Start and fleet end where Coded start is Late Start 
g3_Part_3<- G3[G3$Coded_fleet_start=='Late Start',c(3,4,6)]
View(g3_Part_3)
#34)Ordering Data set Based On Year Variable
G3<-G3[order(G3$Year) , ]
#22) Get First 10 rows of data set
print(head(G3,10))
#35) Get Last 10 rows of data set
print(tail(G3,10))
###############################################################################
#36)Creating Imputation variable to make prediction the Missing Values
library(mice)
pre.imputation<-mice(g3_Part_3,m=5,meth=c("pmm","pmm","logreg"),maxit = 2)
pre.imputation$imp
###############################################################################################
#37)Get Matrix of only integers
g3_num <- matrix(c(year_vec,G3$fleet_start,G3$fleet_finish))
#38) Get Total Of The Column Sum
tc<- colSums(g3_num)
#39) Get Total Of Row Sums
tr<- rowSums(g3_num)
tc
tr
#########################################Data Visualization###############################################
#Secondly, After I Performed Data Cleaning And Processing As Possible As I could
#Let Us Start Data Visualization Which Is Final Step Of Data Analysis
#40)Display The Relationship between fleet_start and fleet_end using Scatter plot
library(ggplot2)
ScatterPlot<- ggplot(G3,aes(x=fleet_start,y=fleet_finish))
ScatterPlot+geom_point()
#41) Display The Numeric Values For Year 
HistoGram<- ggplot(g3_Part_3,aes(fleet_start))+geom_histogram(color='black',fill='red',alpha=0.8)+ggtitle("Fleet Start")
HistoGram
#42) Summarizing Code.Time.less.than.3  Variable 0,1 to Coded fleet_start and coded_fleet_end
BarChart<- ggplot(G3,aes(x=Code.Time.less.than.3,fill=Coded_fleet_start))
BarChart +geom_bar()+labs(y=" Coded Time " )
BarChart +geom_bar() +theme_light()+facet_wrap(~Coded_fleet_finish)
#43) Distribution of Fleet_finish
Sc<- ggplot(g3_Part_3,aes(fleet_start,fleet_finish))
Sc+geom_point(aes(color=Coded_fleet_start))+labs(x="Fleet Start",y="Fleet Finish")+stat_smooth(se=FALSE)
