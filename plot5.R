# Fine particulate matter data set from the EPA NEI web site from 1999 to 2008
# Calvin Seto
# June 14, 2015

# Question 5
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City? 

# assumes raw data files in the working directory
# Load data set into R objects if NEI or SCC does not exist
if (!exists("NEI")) {
    NEI <- readRDS("summarySCC_PM25.rds")
}
if (!exists("SCC")) {
    SCC <- readRDS("Source_Classification_Code.rds")
}

# use dplyr
library(dplyr)
NEI <- tbl_df(NEI)
SCC <- tbl_df(SCC)

# filter by Baltimore City
baltNEI <- filter(NEI, fips=='24510') # 2096

# filter SCC for motor vehicle sources
onRoadIDs <- filter(SCC, Data.Category=='Onroad') # 1137 sources

# Using Onroad Data Category IDs to filter Baltimore emissions by motor vehicles
# 2096 reduced to 1119
baltOnRoadNEI <- baltNEI[baltNEI$SCC %in% as.character(onRoadIDs$SCC),]
# > baltNEI[baltNEI$SCC %in% as.character(onRoadIDs$SCC),]
# Source: local data frame [1,119 x 6]

# summarize emissions by year
# Compute sum total of Emissions by year for motor vehicle related subset of NEI
totalMotorEmissionsByYear <- tapply(baltOnRoadNEI$Emissions,baltOnRoadNEI$year,sum)
# OUTPUT
# > totalMotorEmissionsByYear
# 1999      2002      2005      2008 
# 346.82000 134.30882 130.43038  88.27546 

# Compute average of Emissions by year for motor vehicle related subset of NEI
averageMotorEmissionsByYear <- tapply(baltOnRoadNEI$Emissions,baltOnRoadNEI$year,mean)
# OUTPUT
# > averageMotorEmissionsByYear
# 1999      2002      2005      2008 
# 1.8063542 0.4184075 0.4025629 0.3130335 

# use colors
library(RColorBrewer)

# create bar plots of total and average emissions side by side size 1280x480
# specify title and customize y axis
# Use Accent and Dark2 colors from RColorBrewer
# set axis on left (2) with labels and orient horizontally
png("plot5.png", height=480, width=1280)
par(mfrow = c(1,2))
barplot(as.table(totalMotorEmissionsByYear), main="Total Motor Vehicle PM2.5 Emissions (Baltimore City) 1999 to 2008",axes=F,col=brewer.pal(4, "Accent"))
axis(side=2, las=1, labels=T)
barplot(as.table(averageMotorEmissionsByYear), main="Average Motor Vehicle PM2.5 Emissions (Baltimore City) 1999 to 2008",axes=F,col=brewer.pal(4, "Dark2"))
axis(side=2, las=1, labels=T)
dev.off()
