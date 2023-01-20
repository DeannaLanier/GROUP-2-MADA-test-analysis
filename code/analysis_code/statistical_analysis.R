###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

####################################
#Plot Data#
####################################

#Boxplot of height and hair color 
fig1 = ggplot(data = mydata, aes(x=Haircolor, y=Height)) + geom_boxplot() + ggtitle("Height and Hair Boxplot") +theme(plot.title = element_text(hjust = 0.5))+ xlab(
  "Hair Color") + ylab("Height")
#Sava data
fig1_File = here("results","Height_HairH_Boxplot.png")
ggsave(filename = fig1_File, plot=fig1)

#Scatter plot Weight and ForeheadHeight
fig2 = ggplot(data = mydata, aes(x=Weight, y=ForeheadHeight)) + geom_point() + 
  ggtitle("Weight and Forehead Scatterplot")+theme(plot.title = element_text(hjust = 0.5))+ xlab(
    "Weight") + ylab("Forehead Height")
fig2_File = here("results","Weight_Forehead_Scatter.png")
ggsave(filename = fig2_File, plot=fig2)


######################################
#Data fitting/statistical analysis
######################################

############################
#### First model fit
# fit linear model using height as outcome, weight as predictor

lmfit1 <- lm(Height ~ Weight, mydata)  

# place results from fit into a data frame with the tidy function
lmtable1 <- broom::tidy(lmfit1)

#look at fit results
print(lmtable1)

# save fit results table  
table_file1 = here("results", "resulttable1.rds")
saveRDS(lmtable1, file = table_file1)

############################
#### Second model fit
# fit linear model using height as outcome, weight and sex as predictor

lmfit2 <- lm(Height ~ Weight + Sex, mydata)  

# place results from fit into a data frame with the tidy function
lmtable2 <- broom::tidy(lmfit2)

#look at fit results
print(lmtable2)

# save fit results table  
table_file2 = here("results", "resulttable2.rds")
saveRDS(lmtable2, file = table_file2)

  