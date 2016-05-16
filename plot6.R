# Fine particulate matter data set from the EPA NEI web site from 1999 to 2008
# Calvin Seto
# June 14, 2015

# Question 6
# Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?

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
baltNEI <- filter(NEI, fips=='24510') # 2096
laNEI <- filter(NEI, fips=='06037') # 9320

# filter SCC for motor vehicle sources using Data.Category = Onroad
onRoadIDs <- filter(SCC, Data.Category=='Onroad') # 1137

# Using Onroad Data Category IDs to filter emissions by motor vehicles
# filter Baltimore City emissions by motor vehicles 2096 reduced to 1119
baltOnRoadNEI <- baltNEI[baltNEI$SCC %in% as.character(onRoadIDs$SCC),]

# filter Los Angeles emissions by motor vehicles 9320 reduced to 980
laOnRoadNEI <- laNEI[laNEI$SCC %in% as.character(onRoadIDs$SCC),]

# summarize emissions by year for Baltimore
dfBaltTotalByYear <- summarize(group_by(baltOnRoadNEI, year),sum(Emissions))
names(dfBaltTotalByYear) <- c("year", "totalEmissions")

# Define custom function to compute data frame of three year changes for input data frame
ThreeYrChange <- function(dfIn) {
    df <- data.frame(totalEmissions=0)
    for(i in 2:4) {
        df <- rbind(df, data.frame(totalEmissions=dfIn[i,2]-dfIn[i-1,2]))
    }
    names(df) <- c("ThreeYearChange")
    df
}

# Define custom function to compute data frame of three year percent changes for input data frame
ThreeYrPercentChange <- function(dfIn) {
    df <- data.frame(totalEmissions=0)
    for(i in 2:4) {
        df <- rbind(df, data.frame(totalEmissions=dfIn[i,3]/dfIn[i-1,2]))
    }
    names(df) <- c("ThreeYearPercentChange")
    df
}

# create new variable ThreeYrChange with custom function for Baltimore
dfBaltTotalByYear <- cbind(dfBaltTotalByYear,ThreeYrChange(dfBaltTotalByYear))

# create new variable ThreeYrPercentChange with custom function for Baltimore
dfBaltTotalByYear <- cbind(dfBaltTotalByYear,ThreeYrPercentChange(dfBaltTotalByYear))
# dfBaltTotalByYear OUTPUT
# year totalEmissions ThreeYearChange ThreeYearPercentChange
# 1 1999      346.82000        0.000000             0.00000000
# 2 2002      134.30882     -212.511179            -0.61274200
# 3 2005      130.43038       -3.878441            -0.02887704
# 4 2008       88.27546      -42.154922            -0.32319865

# summarize emissions by year for Los Angeles
dfLaTotalByYear <- summarize(group_by(laOnRoadNEI, year),sum(Emissions))
names(dfLaTotalByYear) <- c("year", "totalEmissions")

# create new variable ThreeYrChange with custom function for Los Angeles
dfLaTotalByYear <- cbind(dfLaTotalByYear, ThreeYrChange(dfLaTotalByYear))

# create new variable ThreeYrPercentChange with custom function for Los Angeles
dfLaTotalByYear <- cbind(dfLaTotalByYear, ThreeYrPercentChange(dfLaTotalByYear))
# dfLaTotalByYear OUTPUT
# year totalEmissions ThreeYearChange ThreeYearPercentChange
# 1 1999       3931.120          0.0000             0.00000000
# 2 2002       4274.030        342.9102             0.08722964
# 3 2005       4601.415        327.3847             0.07659860
# 4 2008       4101.321       -500.0939            -0.10868264

# use colors
library(RColorBrewer)

# create line plots of three year change and three year percent changes side by side size 1280x480
# specify title and customize y axis
# Use Accent and Dark2 colors from RColorBrewer
# set axis on left (2) and below with labels and orient horizontally
png("plot6.png", height=480, width=1280)
par(mfrow=c(1,2))
rng <- range(dfBaltTotalByYear$ThreeYearPercentChange, dfLaTotalByYear$ThreeYearPercentChange)
plot(dfBaltTotalByYear$year, dfBaltTotalByYear$ThreeYearPercentChange, xlab="Year", ylab="3 Year Percent Change", axes=F, main="Three Year % change of Total PM2.5 Emissions (Baltimore) 1999 to 2008", type="l",ylim=rng, col=brewer.pal(4, "Accent"))
axis(side=1, las=1, labels=T)
axis(side=2, las=1, labels=T)
plot(dfLaTotalByYear$year, dfLaTotalByYear$ThreeYearPercentChange, xlab="Year", ylab="3 Year Percent Change", axes=F, main="Three Year % change of Total PM2.5 Emissions (Los Angeles) 1999 to 2008", type="l",ylim=rng, col=brewer.pal(4, "Dark2"))
axis(side=1, las=1, labels=T)
axis(side=2, las=1, labels=T)
dev.off()
