# Fine particulate matter data set from the EPA NEI web site from 1999 to 2008
# Calvin Seto
# June 14, 2015

# Question 4
# Across the United States, how have emissions from coal combustion-related sources
# changed from 1999-2008?

# assumes raw data files in the working directory
# Load data set into R objects if NEI or SCC does not exist
if (!exists("NEI")) {
    NEI <- readRDS("summarySCC_PM25.rds")
}
if (!exists("SCC")) {
    SCC <- readRDS("Source_Classification_Code.rds")
}

# Create logical vector isCoal of NEI rows with a coal related source
# In SCC, Grep Short.Name for the string coal, upper or lower case letters
# Find matching SCC codes in NEI with the %in% operator and matching the Short.Name in SCC
# need to coerce Short.Name to character type
isCoal <- (NEI$SCC %in% as.character(SCC[grep("[Cc][Oo][Aa][Ll]", SCC$Short.Name),1])) #239 sources found

# use logical vector isCoal to create coal related subset of NEI
ssCoalNEI <- NEI[isCoal,]

# some queries to see data values
# str(ssCoalNEI) 53400 obs of 6 variables
# unique(ssCoalNEI$fips) # 3230 counties
# unique(ssCoalNEI$SCC) # 183 source types
# unique(ssCoalNEI$type) # NONPOINT and POINT
# unique(ssCoalNEI$year)  # 1999 2002 2005 2008

# Check Missing Values in Emissions
# sum(is.na(ssCoalNEI$Emissions)) # 0

# summary of Emissions for 1999
# nrow(ssCoalNEI[ssCoalNEI$year==1999,]) # 10074
# summary(ssCoalNEI[ssCoalNEI$year==1999,]$Emissions)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000     0.050     0.331    59.820     2.691 14270.000 

# summary of Emissions for 2002
# nrow(ssCoalNEI[ssCoalNEI$year==2002,]) # 12748
# summary(ssCoalNEI[ssCoalNEI$year==2002,]$Emissions)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000     0.002     0.170    44.320     2.420 10240.000

# summary of Emissions for 2005
# nrow(ssCoalNEI[ssCoalNEI$year==2005,]) # 12829
# summary(ssCoalNEI[ssCoalNEI$year==2005,]$Emissions)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000     0.002     0.180    44.400     2.420 11470.000

# summary of Emissions for 2008
# nrow(ssCoalNEI[ssCoalNEI$year==2008,]) # 17749
# summary(ssCoalNEI[ssCoalNEI$year==2008,]$Emissions)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000    0.000    0.004   20.170    0.415 7220.000 

# Compute sum total of Emissions by year for coal related subset of NEI
totalCoalEmissionsByYear <- tapply(ssCoalNEI$Emissions,ssCoalNEI$year,sum)
# OUTPUT
# > totalCoalEmissionsByYear
# 1999     2002     2005     2008 
# 602624.1 564940.0 569654.7 358083.9 

# Compute average of Emissions by year for coal related subset of NEI
averageCoalEmissionsByYear <- tapply(ssCoalNEI$Emissions,ssCoalNEI$year,mean)
# OUTPUT
# > averageCoalEmissionsByYear
# 1999     2002     2005     2008 
# 59.81974 44.31597 44.40367 20.17487

# use colors
library(RColorBrewer)

# create bar plots of total and average emissions side by side size 1280x480
# specify title and customize y axis
# Use Accent and Dark2 colors from RColorBrewer
# set axis on left (2) with labels and orient horizontally
png("plot4.png", height=480, width=1280)
par(mfrow = c(1,2))
barplot(as.table(totalCoalEmissionsByYear), main="U.S. Total PM2.5 Emissions (Coal related) 1999 to 2008",axes=F,col=brewer.pal(4, "Accent"))
axis(side=2, las=1, labels=T)
barplot(as.table(averageCoalEmissionsByYear), main="U.S. Average PM2.5 Emissions (Coal related) 1999 to 2008",axes=F,col=brewer.pal(4, "Dark2"))
axis(side=2, las=1, labels=T)
dev.off()
