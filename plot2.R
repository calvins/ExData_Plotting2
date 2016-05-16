# Fine particulate matter data set from the EPA NEI web site from 1999 to 2008
# Calvin Seto
# June 14, 2015

# Question 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips == "24510") from 1999 to 2008?
# Use the base plotting system to make a plot answering this question.

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

#  filter by Baltimore City
baltNEI <- filter(NEI, fips=='24510')

# Sum Emissions by year
baltTotalEmissionsByYear <- tapply(baltNEI$Emissions,baltNEI$year,sum)

# OUTPUT baltTotalEmissionsByYear
# 1999     2002     2005     2008 
# 3274.180 2453.916 3091.354 1862.282 

# use colors
library(RColorBrewer)

# Create the a bar plot using base plotting system size 640x480
# specify title and customize y axis
# Use Accent colors from RColorBrewer
# set axis on left (2) with labels and orient horizontally
png("plot2.png", height=480, width=640)
barplot(as.table(baltTotalEmissionsByYear), main="Total PM2.5 Emissions (Baltimore City, Maryland) 1999 to 2008", axes=F, col=brewer.pal(4, "Accent"))
axis(side=2, las=1, labels=T)
dev.off()
