# Fine particulate matter data set from the EPA NEI web site from 1999 to 2008
# Calvin Seto
# June 14, 2015

# Question 1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources
# for each of the years 1999, 2002, 2005, and 2008.

# assumes raw data files in the working directory
# Load data set into R objects if NEI or SCC does not exist
if (!exists("NEI")) {
    NEI <- readRDS("summarySCC_PM25.rds")
}
if (!exists("SCC")) {
    SCC <- readRDS("Source_Classification_Code.rds")
}

# Sum Emissions by year
totalEmissionsByYear <- tapply(NEI$Emissions,NEI$year,sum)

# OUTPUT totalEmissionsByYear
# 1999    2002    2005    2008 
# 7332967 5635780 5454703 3464206 

# use colors
library(RColorBrewer)

# Create the a bar plot using base plotting system with size 640x480
# specify title and customize y axis
# Use Accent colors from RColorBrewer
# set axis on left (2) with labels and orient horizontally
png("plot1.png", height=480, width=640)
barplot(as.table(totalEmissionsByYear), main="Total PM2.5 Emissions (U.S.) 1999 to 2008", axes=F, col=brewer.pal(4, "Accent"))
axis(side=2, las=1, labels=T)
dev.off()
