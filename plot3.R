# Fine particulate matter data set from the EPA NEI web site from 1999 to 2008
# Calvin Seto
# June 14, 2015

# Question 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen decreases in emissions from 1999-2008
# for Baltimore City?
# Which have seen increases in emissions from 1999-2008?
# Use the ggplot2 plotting system to make a plot answer this question.

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

#  filter by city
baltNEI <- filter(NEI, fips=='24510')

# Use dplyr to filter baltNEI for each of the 4 types and create 4 subsets
# library(dplyr)
ssBaltPoint <- filter(baltNEI, type=="POINT")
ssBaltNonPoint <- filter(baltNEI, type=="NONPOINT")
ssBaltOnRoad <- filter(baltNEI, type=="ON-ROAD")
ssBaltNonRoad <- filter(baltNEI, type=="NON-ROAD")

# for each type, sum Emissions by year and create data frame and add type column
dfPoint <- summarize(group_by(ssBaltPoint, year),sum(Emissions))
dfPoint$type <- c("POINT")
dfNonPoint <- summarize(group_by(ssBaltNonPoint, year),sum(Emissions))
dfNonPoint$type <- c("NONPOINT")
dfOnRoad <- summarize(group_by(ssBaltOnRoad, year),sum(Emissions))
dfOnRoad$type <- c("ON-ROAD")
dfNonRoad <- summarize(group_by(ssBaltNonRoad, year),sum(Emissions))
dfNonRoad$type <- c("NON-ROAD")
# > dfPoint
# Source: local data frame [4 x 3]
# 
# year sum(Emissions)  type
# 1 1999       296.7950 POINT
# 2 2002       569.2600 POINT
# 3 2005      1202.4900 POINT
# 4 2008       344.9752 POINT
# 
# > dfNonPoint
# Source: local data frame [4 x 3]
# 
# year sum(Emissions)     type
# 1 1999       2107.625 NONPOINT
# 2 2002       1509.500 NONPOINT
# 3 2005       1509.500 NONPOINT
# 4 2008       1373.207 NONPOINT

# > dfOnRoad
# Source: local data frame [4 x 3]
# 
# year sum(Emissions)    type
# 1 1999      346.82000 ON-ROAD
# 2 2002      134.30882 ON-ROAD
# 3 2005      130.43038 ON-ROAD
# 4 2008       88.27546 ON-ROAD
# 
# > dfNonRoad
# Source: local data frame [4 x 3]
# 
# year sum(Emissions)     type
# 1 1999      522.94000 NON-ROAD
# 2 2002      240.84692 NON-ROAD
# 3 2005      248.93369 NON-ROAD
# 4 2008       55.82356 NON-ROAD

# Borrow multiplot function from cookbook-r.com for # Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

# use ggplot2 to create 4 plots for each type to be placed into multiplot
library(ggplot2)
p1 <- ggplot(data=dfNonRoad,
      aes(x=dfNonRoad$year,y=dfNonRoad$"sum(Emissions)",fill=dfNonRoad$year)) +
    geom_bar(stat="identity") +
    labs(title="Total PM2.5 Emissions (Baltimore City, Maryland) 1999 to 2008 Source: NONROAD") +
    labs(x="Year", y="Emissions") +
    scale_x_continuous(breaks=c(1999,2002,2005,2008))

p2 <- ggplot(data=dfOnRoad,
       aes(x=dfOnRoad$year,y=dfOnRoad$"sum(Emissions)",fill=dfOnRoad$year)) +
    geom_bar(stat="identity") +
    labs(title="Total PM2.5 Emissions (Baltimore City, Maryland) 1999 to 2008 Source: ONROAD") +
    labs(x="Year", y="Emissions") +
    scale_x_continuous(breaks=c(1999,2002,2005,2008))

p3 <- ggplot(data=dfNonPoint,
       aes(x=dfNonPoint$year,y=dfNonPoint$"sum(Emissions)",fill=dfNonPoint$year)) +
    geom_bar(stat="identity") +
    labs(title="Total PM2.5 Emissions (Baltimore City, Maryland) 1999 to 2008 Source: NONPOINT") +
    labs(x="Year", y="Emissions") +
    scale_x_continuous(breaks=c(1999,2002,2005,2008))

p4 <- ggplot(data=dfPoint,
       aes(x=dfPoint$year,y=dfPoint$"sum(Emissions)",fill=dfPoint$year)) +
    geom_bar(stat="identity") +
    labs(title="Total PM2.5 Emissions (Baltimore City, Maryland) 1999 to 2008 Source: POINT") +
    labs(x="Year", y="Emissions") +
    scale_x_continuous(breaks=c(1999,2002,2005,2008))

# Create png file with dimensions 1280x480
# Create multiplot with the 4 plots
png("plot3.png", height=480, width=1280)
mp <- multiplot(p1, p2, p3, p4, cols=2)
dev.off()
