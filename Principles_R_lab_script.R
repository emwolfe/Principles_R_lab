## ----example------------------------------------------------------------------------
#for example:
2 + 2

#hint: in RStudio Cloud, click on the green triangle in the top right corner of this chunk, then look at the console below for the output!


## ----check_wd-----------------------------------------------------------------------

getwd() #tells you current working directory information

###note that you must tell R to run (either through keyboard shortcuts or by clicking Run) each line of code or nothing will happen!!



## ----load_data----------------------------------------------------------------------

Elwha_data <- read.csv("Elwha_estuary_water_quality_2006-2014.csv",check.names = F) #reading in our .csv
Elwha_data #look at the data frame you created
colnames(Elwha_data)



## ----bonus_clean_up-----------------------------------------------------------------
###THIS IS A BONUS SECTION--IF YOU DO NOT WANT TO ATTEMPT TO CLEAN THE DATA ON YOUR OWN, SKIP THIS CHUNK!###

colnames(Elwha_data)[colnames(Elwha_data) == "Nitrate + Nitrite concentration"] <- "Nitrate_Nitrite concentration" #rename column to remove +
colnames(Elwha_data)[colnames(Elwha_data) == "% Dissolved oxygen"] <- "Perc Dissolved oxygen" #rename column to remove %

names(Elwha_data) <- gsub(" ", "_", names(Elwha_data)) #replace spaces with underscores

Elwha_data[Elwha_data == "NS"] <- NA #replaces "NS", which stands for "no sample was collected" with NA, the correct syntax for R to recognize a missing value

Elwha_data <- Elwha_data[1:147,] #gets rid of a few extra, empty rows that were hanging around for no reason



## ----cleaned_data-------------------------------------------------------------------
###skip this chunk if you did the bonus section###
Elwha_data <- read.csv("cleaned_data.csv",check.names = T) #reading in our .csv


## ----data_look----------------------------------------------------------------------
str(Elwha_data) #tells us about our data frame
Elwha_data[1:5,1:5] #lets us preview the first 5 rows and columns, respectively - [r,c], and the : between numbers means that you are looking at the continuous range between them, like what appears in Excel when you select consecutive cells. You could read it as "through," so that 1:5 would read 1 through 5.


## ----factors------------------------------------------------------------------------

Elwha_data$Dam_Condition <- as.factor(Elwha_data$Dam_Condition) #the dollar sign helps us give R an absolute reference, similar to Excel syntax
Elwha_data$Site_Name <- as.factor(Elwha_data$Site_Name)

Elwha_data$Phosphate_concentration <- as.numeric(Elwha_data$Phosphate_concentration)
Elwha_data$Nitrate_Nitrite_concentration <- as.numeric(Elwha_data$Nitrate_Nitrite_concentration)
Elwha_data$Ammonium_concentration <- as.numeric(Elwha_data$Ammonium_concentration)
Elwha_data$Salinity <- as.numeric(Elwha_data$Salinity)
Elwha_data$Temperature <- as.numeric(Elwha_data$Temperature)
Elwha_data$Turbidity <- as.numeric(Elwha_data$Turbidity)
Elwha_data$Dissolved_oxygen <- as.numeric(Elwha_data$Dissolved_oxygen)
Elwha_data$Perc_Dissolved_oxygen <- as.numeric(Elwha_data$Perc_Dissolved_oxygen)
Elwha_data$pH <- as.numeric(Elwha_data$pH)



## ----structure_data-----------------------------------------------------------------

str(Elwha_data)



## ----mean_pH------------------------------------------------------------------------

mean_pH <- mean(Elwha_data$pH); mean_pH



## ----mean_rm------------------------------------------------------------------------

mean_pH <- mean(Elwha_data$pH, na.rm = TRUE); mean_pH #remove rows within column with value NA



## ----median-------------------------------------------------------------------------

median(Elwha_data$pH, na.rm=TRUE)



## ----min_max------------------------------------------------------------------------

#you can use a semi-colon to combine multiple commands to be more efficient
min(Elwha_data$pH, na.rm = TRUE); max(Elwha_data$pH, na.rm = TRUE)



## ----sd-----------------------------------------------------------------------------

sd_pH <- sd(Elwha_data$pH, na.rm = TRUE); sd_pH



## ----CI-----------------------------------------------------------------------------

mean_pH <- mean(Elwha_data$pH, na.rm = TRUE)
std_pH <-sd(Elwha_data$pH, na.rm = TRUE)
error <- qnorm(0.975)*std_pH/sqrt(147) #147 is the total number of observations (our n), which is found in the str() output
left <- mean_pH-error
right <- mean_pH+error
left; right



## ----t_test-------------------------------------------------------------------------

ttest <- t.test(pH~Dam_Condition, data=Elwha_data); ttest



## ----during_subset------------------------------------------------------------------

during_dam <- subset(Elwha_data,Elwha_data$Dam_Condition=="During removal")
during_dam[1:5,1:5]



## ----ANOVA--------------------------------------------------------------------------

pH_ANOVA <- aov(pH~Site_Name, data=during_dam)
summarypH <- summary(pH_ANOVA); summarypH



## ----plot_pH, fig.align='center'----------------------------------------------------

plot(pH~Site_Name, data=Elwha_data)



## ----libraries, fig.align='center'--------------------------------------------------
chooseCRANmirror(ind=1)
install.packages("plyr",repos='http://cran.us.r-project.org')
install.packages("dplyr",repos='http://cran.us.r-project.org') #different packages house different sets of functions - we need a function in package plyr
library(plyr) #tells R to open the library of functions contained in plyr
library(dplyr) #tells R to open the library of functions contained in dplyr

for_aov_plot <- ddply(during_dam,~Site_Name,summarize,mean=mean(pH, na.rm=TRUE),sd=sd(pH, na.rm = TRUE))

for_aov_plot

install.packages("ggplot2")
library(ggplot2)

plot <- ggplot(for_aov_plot, aes(x=Site_Name, y=mean)) + geom_bar(aes(fill=Site_Name),stat="identity", color="black", position=position_dodge()) + geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)) + labs(x="Sample Site",y="pH",fill="Sample Site")

plot



## ----lm_DO_pH-----------------------------------------------------------------------

DO_pH_reg <- lm(Dissolved_oxygen~pH, data=Elwha_data)
DO_pH_reg
summaryDO <- summary(DO_pH_reg); summaryDO



## ----plot_DO_pH, fig.align='center'-------------------------------------------------

plot(Dissolved_oxygen~pH, data=Elwha_data,xlab="pH", ylab="Dissolved oxygen (mg/L)") #the plot function doesn't use any special packages - it is in what we call base R; for units on Dissolved oxygen, we should check the metadata
abline(lm(Dissolved_oxygen~pH, data=Elwha_data)) #plots our regression line



## ----cor----------------------------------------------------------------------------

P_NO3_cor <- cor.test(Elwha_data$Phosphate_concentration,Elwha_data$Nitrate_Nitrite_concentration,method="pearson",use = "complete.obs") #another way to reference the columns; use argument omits rows with NA values
P_NO3_cor



## ----plot_P_NO3, fig.align='center'-------------------------------------------------

plot(Elwha_data$Phosphate_concentration~Elwha_data$Nitrate_Nitrite_concentration, xlab="Nitrate & Nitrite (micromolar)", ylab="Phosphate (micromolar)")
abline(lm(Elwha_data$Phosphate_concentration~Elwha_data$Nitrate_Nitrite_concentration))


