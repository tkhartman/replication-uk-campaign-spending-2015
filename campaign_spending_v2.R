##' ---
##' title: "Constituency Campaign Spending Project"
##' author: "Todd K. Hartman"
##' date: "`r format(Sys.Date())`"
##' output: "github_document"
##' ---

##' Housekeeping
## Load packages via pacman
pacman::p_load(dplyr, haven, interplot, plyr, readxl, scales, tidyr)

## Set display options
options("scipen" = 100, "digits" = 4)

## Set the Working Directory
# setwd("~/Dropbox/_RESEARCH/Pattie_Johnston_2015_UK_Election/Campaign_Spending_Project/Data/")  # For Mac OSX

##' Load the 2015 campaign spending data
## Download (or load if downloaded) from the Electoral Commission website
## http://www.electoralcommission.org.uk/find-information-by-subject/elections-and-referendums/past-elections-and-referendums/uk-general-elections/candidate-election-spending
url.15 <- "https://www.electoralcommission.org.uk/__data/assets/excel_doc/0004/199066/2015-UK-Parliament-spending-data.xlsx"
file.15 <- basename(url.15)  # Extract the filename
if (!file.exists(file.15))   # Only download if not in the working directory
    download.file(url = url.15, destfile = file.15, mode = "wb")
spend.15 <- read_excel(file.15)  # Load the dataset

## Subset the data
names(spend.15)
spend.15.sub <- subset(spend.15, select = c(`ConstituencyId`, `Party Name`, `Spend as % of Short Limit`, `Short Total Spend`, `Spend % of Long Limit`))
spend.15.sub$ons <- spend.15.sub$`ConstituencyId`
spend.15.sub$party <- spend.15.sub$`Party Name`
spend.15.sub$shortpct15 <- spend.15.sub$`Spend as % of Short Limit`
spend.15.sub$`ConstituencyId` <- spend.15.sub$`Party Name` <- spend.15.sub$`Spend as % of Short Limit` <- NULL
write.csv(spend.15.sub, "UK_2015_LONG.csv", row.names = FALSE)


##' Load the 2010 campaign spending data
## Download (or load if downloaded) from the Electoral Commission website
url.10 <- "http://www.electoralcommission.org.uk/__data/assets/excel_doc/0020/150806/2010-UK-Parliament-spending-data-Excel.xls"
file.10 <- basename(url.10)  # Extract the filename
if (!file.exists(file.10))   # Only download if not in the working directory
    download.file(url = url.10, destfile = file.10, mode = "wb")
spend.10 <- suppressWarnings(read_excel(file.10, sheet = 3))  # Load the dataset

## Subset the data
names(spend.10)
spend.10.sub <- subset(spend.10, select = c(`ConstituencyId`, `Party Name`, `Spend as % of Short Limit`))
spend.10.sub$ons <- spend.10.sub$`ConstituencyId`
spend.10.sub$party <- spend.10.sub$`Party Name`
spend.10.sub$shortpct10 <- spend.10.sub$`Spend as % of Short Limit`
spend.10.sub$`ConstituencyId` <- spend.10.sub$`Party Name` <- spend.10.sub$`Spend as % of Short Limit` <- NULL
write.csv(spend.10.sub, "UK_2010_LONG.csv", row.names = FALSE)


##' Load the 2015 British Election Study constituency contextual results data
## Download (or load if downloaded) from the BES website
url.bes <- "http://www.britishelectionstudy.com/custom/uploads/2017/03/BES-2015-General-Election-results-file-v2.2.xlsx"
file.bes <- basename(url.bes)  # Extract the filename
if (!file.exists(file.bes))   # Only download if not in the working directory
    download.file(url = url.bes, destfile = file.bes, mode = "wb")
bes.wide <- read_excel(file.bes)  # Load the dataset

## Subset the data
names(bes.wide)
bes.wide.sub <- subset(bes.wide, select = c("ONSConstID", "Winner15", "Winner10", "Con15", "Lab15", 
                                            "LD15", "SNP15", "PC15", "UKIP15", "Green15", "Other15", 
                                            "Con10", "Lab10", "LD10", "SNP10", "PC10", 
                                            "UKIP10", "Green10", "BNP10"))

## Reshape BES data from wide to long
names(bes.wide.sub)
colnames(bes.wide.sub) <- c("ons", "winner15", "winner10", "Conservative.v2015", "Labour.v2015", 
                            "LibDem.v2015", "SNP.v2015", "PC.v2015", "UKIP.v2015", "Green.v2015", 
                            "Other.v2015", "Conservative.v2010", "Labour.v2010", "LibDem.v2010", 
                            "SNP.v2010", "PC.v2010", "UKIP.v2010", "Green.v2010", "Other.v2010")

bes.long <- 
    bes.wide.sub %>%
    gather(key, pct, -ons, -winner15, -winner10) %>%
    separate(key, into = c("party", "year"), sep = "\\.") %>%
    spread(year, pct)

write.csv(bes.long, "UK_BES_LONG.csv", row.names = FALSE)


##' Load the 2015 seat status indicator (hand-coded)
## Download (or load if downloaded) from Github
url.seat <- "https://raw.githubusercontent.com/tkhartman/replication-ashcroft-polling-campaign-spending/master/seat_status.csv"
file.seat <- basename(url.seat)  # Extract the filename
if (!file.exists(file.seat))   # Only download if not in the working directory
    download.file(url = url.seat, destfile = file.seat, mode = "wb")
seat.wide <- read.csv(file.seat) # Author coded file

seat.long <- 
    seat.wide %>%
    gather(party, seat.status, -ons)

write.csv(bes.long, "SEAT_STATUS_LONG.csv", row.names = FALSE)

##' Merge datasets
## Remove extraneous parties from the 2015 spending data
table(spend.15.sub$party)
party.15.keep <- c("Conservative Party", "Green Party", "Labour Party",
                   "Labour Party / Co-operative Party", "Liberal Democrats", "Plaid Cymru - The Party of Wales",
                   "Scottish National Party (SNP)", "UK Independence Party (UKIP)")

spend.15.sub <- subset(spend.15.sub, party %in% party.15.keep)

## Remove extraneous parties from the 2010 spending data
table(spend.10.sub$party)
party.10.keep <- c("Conservative and Unionist Party", "Green Party", "Labour Party", 
                   "Labour Party/Co-operative Party", "Liberal Democrats", "Plaid Cymru - The Party of Wales",
                   "Scottish National Party (SNP)", "UK Independence Party (UK I P)")

spend.10.sub <- subset(spend.10.sub, party %in% party.10.keep)

## Make party names consistent for merging
party.15.names <- c("Conservative", "Green", "Labour", "Labour", "LibDem", "PC", "SNP", "UKIP")
party.10.names <- c("Conservative", "Green", "Labour", "Labour", "LibDem", "PC", "SNP", "UKIP")

spend.15.sub$party <- mapvalues(spend.15.sub$party, from = party.15.keep, to = party.15.names)
spend.10.sub$party <- mapvalues(spend.10.sub$party, from = party.10.keep, to = party.10.names)

## Merge 2015 and 2010 spending data
f <- left_join(spend.15.sub, spend.10.sub, by = c("ons", "party"))

## Merge spending data with BES election results
ff <- left_join(bes.long, f, by = c("ons", "party"))

## Merge spending, election, and seat status data
df <- left_join(ff, seat.long, by = c("ons", "party"))
write.csv(df, "Short_Spending_Data_Merged_Final_May_2017.csv", row.names = FALSE)


############################################################################
##' Clear the workspace and start anew
rm(list=ls())
df <- read.csv("Short_Spending_Data_Merged_Final_May_2017.csv")

##' Generate 2010 marginality score
## Find the highest vote share in 2010
df <- merge(df, aggregate(v2010 ~ ons, data = df, max),
            by = "ons", suffixes = c("", ".1st"))

## Second highest vote share in 2010
df <- merge(df, aggregate(v2010 ~ ons, data = df, function(x){sort(na.omit(x), decreasing=T)[2]}),
            by = "ons", suffixes = c("", ".2nd"))

## 2010 marginality scores (absolute value)
df$con.margin10 <- ifelse(df$party == "Conservative", 
                          abs(ifelse(df$winner10 == "Conservative", df$v2010 - df$v2010.2nd, df$v2010 - df$v2010.1st)),
                          NA)

df$lab.margin10 <- ifelse(df$party == "Labour", 
                          abs(ifelse(df$winner10 == "Labour", df$v2010 - df$v2010.2nd, df$v2010 - df$v2010.1st)),
                          NA)

df$ld.margin10 <- ifelse(df$party == "LibDem", 
                         abs(ifelse(df$winner10 == "Liberal Democrat", df$v2010 - df$v2010.2nd, df$v2010 - df$v2010.1st)),
                         NA)

summary(df$con.margin10)
summary(df$lab.margin10)
summary(df$ld.margin10)

##' Subset the merged data by party
con <- subset(df, party == "Conservative")
lab <- subset(df, party == "Labour")
ld <- subset(df, party == "LibDem")

##' Descriptive statistics for campaign spending
## Percent of 2015 short campaign constituency spending
summary(con$shortpct15)
summary(lab$shortpct15)
summary(ld$shortpct15)

## Potential 2015 missing data
length(con$shortpct15[con$shortpct15 == "0"]) 
length(lab$shortpct15[lab$shortpct15 == "0"]) 
length(ld$shortpct15[ld$shortpct15 == "0"])

## 2015 Spending data, removing 0s (which may be missing or true 0) and > legal spending limit
con$short15 <- ifelse(con$shortpct15 > 0 & con$shortpct15 < 101, con$shortpct15, NA)
lab$short15 <- ifelse(lab$shortpct15 > 0 & lab$shortpct15 < 101, lab$shortpct15, NA)
ld$short15 <- ifelse(ld$shortpct15 > 0 & ld$shortpct15 < 101, ld$shortpct15, NA)

summary(con$short15)
summary(lab$short15)
summary(ld$short15)

## Seat Status
con$Seat <- factor(con$seat.status, levels = c("Incumbent", "Challenger", "Open"), ordered = FALSE)
con$Seat <- mapvalues(con$Seat,
                      from = c("Incumbent", "Challenger", "Open"), 
                      to = c(": Incumbent", ": Challenger (vs Incumbent)", ": Open (vs Incumbent)"))

lab$Seat <- factor(lab$seat.status, levels = c("Incumbent", "Challenger", "Open"), ordered = FALSE)
lab$Seat <- mapvalues(lab$Seat,
                      from = c("Incumbent", "Challenger", "Open"), 
                      to = c(": Incumbent", ": Challenger (vs Incumbent)", ": Open (vs Incumbent)"))

ld$Seat <- factor(ld$seat.status, levels = c("Incumbent", "Challenger", "Open"), ordered = FALSE)
ld$Seat <- mapvalues(ld$Seat,
                     from = c("Incumbent", "Challenger", "Open"), 
                     to = c(": Incumbent", ": Challenger (vs Incumbent)", ": Open (vs Incumbent)"))


##' Campaign spending models
## 2015 short spend regressed on 2010 marginality, status of the seat, and the interaction
summary(mod.con <- lm(short15 ~ con.margin10*Seat, data = con))
summary(mod.lab <- lm(short15 ~ lab.margin10*Seat, data = lab))
summary(mod.ld <- lm(short15 ~ ld.margin10*Seat, data = ld))


##' Simple Vote Share Models 
summary(mod.con2 <- lm(v2015 ~ v2010 + short15, data = con))
summary(mod.lab2 <- lm(v2015 ~ v2010 + short15, data = lab))
summary(mod.ld2 <- lm(v2015 ~ v2010 + short15, data = ld))


##' Vote Share Models with Interactions
summary(mod.con3 <- lm(v2015 ~ v2010 + short15*Seat, data = con))
summary(mod.lab3 <- lm(v2015 ~ v2010 + short15*Seat, data = lab))
summary(mod.ld3 <- lm(v2015 ~ v2010 + short15*Seat, data = ld))


##' Function to combine plots
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, 
                      labs=list(), labpos=list(c(0.5,0.01), c(0.02,0.5))) {
    require(grid)
    
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
        
        if(!length(labs) == 0){
            grid.text(labs[1], x=labpos[[1]][1], y=labpos[[1]][2], gp=gpar(fontsize=12))
            grid.text(labs[2], x=labpos[[2]][1], y=labpos[[2]][2], rot=90, gp=gpar(fontsize=12))
        }
    }
}


##' Figures: Marginal Effects for Campaign Spending Models
summary(fig.con.1 <- lm(short15 ~ con.margin10*Seat, data = con))
summary(fig.lab.1 <- lm(short15 ~ lab.margin10*Seat, data = lab))
summary(fig.ld.1 <- lm(short15 ~ ld.margin10*Seat, data = ld))

p1 <- interplot(m = fig.con.1, var1 = "Seat", var2 = "con.margin10", 
          point = TRUE, hist = TRUE) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(limits = c(0,66), breaks = seq(0,60,10), oob = squish) +
    scale_y_continuous(limits = c(-150,75), breaks = seq(-100,75,25), oob = squish) +
    theme(plot.title = element_text(face="bold")) +
    labs(x="", y="") +
    ggtitle("Conservatives")

p2 <- interplot(m = fig.lab.1, var1 = "Seat", var2 = "lab.margin10", 
          point = TRUE, hist = TRUE) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(limits = c(0,66), breaks = seq(0,60,10), oob = squish) +
    scale_y_continuous(limits = c(-150,75), breaks = seq(-100,75,25), oob = squish) +
    theme(plot.title = element_text(face="bold")) +
    labs(x="", y="") +
    ggtitle("Labour")

p3 <- interplot(m = fig.ld.1, var1 = "Seat", var2 = "ld.margin10", 
          point = TRUE, hist = TRUE) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(limits = c(0,66), breaks = seq(0,60,10), oob = squish) +
    scale_y_continuous(limits = c(-175,75), breaks = seq(-175,75,25), oob = squish) +
    theme(plot.title = element_text(face="bold")) +
    labs(x="", y="") +
    ggtitle("Liberal Democrats")

multiplot(p1, p2, p3, cols = 1, 
          labs=list("2010 Seat Marginality", "Change in 2015 Short Campaign Spending (as % of limit)"))

##' Figures: Marginal Effects of Vote Share Plots
summary(fig.con.2 <- lm(v2015 ~ v2010 + short15*Seat, data = con))
summary(fig.lab.2 <- lm(v2015 ~ v2010 + short15*Seat, data = lab))
summary(fig.ld.2 <- lm(v2015 ~ v2010 + short15*Seat, data = ld))

pp1 <- interplot(m = fig.con.2, var1 = "Seat", var2 = "short15", 
                point = TRUE, hist = TRUE) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(limits = c(0,100), breaks = seq(0,100,25), oob = squish) +
    scale_y_continuous(limits = c(-30,10), breaks = seq(-30,10,10), oob = squish) +
    theme(plot.title = element_text(face="bold")) +
    #labs(x="", y="") +
    labs(x="2015 Short Campaign Spending (as % of limit)", y="Change in 2015 Vote Share") +
    ggtitle("Conservatives")

pp2 <- interplot(m = fig.lab.2, var1 = "Seat", var2 = "short15", 
                point = TRUE, hist = TRUE) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(limits = c(0,100), breaks = seq(0,100,25), oob = squish) +
    scale_y_continuous(limits = c(-50,10), breaks = seq(-50,10,10), oob = squish) +
    theme(plot.title = element_text(face="bold")) +
    #labs(x="", y="") +
    labs(x="2015 Short Campaign Spending (as % of limit)", y="Change in 2015 Vote Share") +
    ggtitle("Labour")

pp3 <- interplot(m = fig.ld.2, var1 = "Seat", var2 = "short15", 
                point = TRUE, hist = TRUE) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(limits = c(0,100), breaks = seq(0,100,25), oob = squish) +
    scale_y_continuous(limits = c(-80,10), breaks = seq(-80,10,10), oob = squish) +
    theme(plot.title = element_text(face="bold")) +
    #labs(x="", y="") +
    labs(x="2015 Short Campaign Spending (as % of limit)", y="Change in 2015 Vote Share") +
    ggtitle("Liberal Democrats")

pp1 
pp2
pp3

multiplot(pp1, pp2, pp3, cols = 1)

png("vote_share_conservatives.png", height = 16, width =  20, res = 1200, units = "cm")
pp1 
dev.off()

png("vote_share_labour.png", height = 16, width =  20, res = 1200, units = "cm")
pp2
dev.off()

png("vote_share_lib_dems.png", height = 16, width =  20, res = 1200, units = "cm")
pp3
dev.off()

##' Summary stats
pacman::p_load(CrossTable, psych)

count(con$`Spend...of.Long.Limit`)
count(con$shortpct15)
count(lab$`Spend...of.Long.Limit`)
count(lab$shortpct15)
count(ld$`Spend...of.Long.Limit`)
count(ld$shortpct15)

con$nonzero <- ((con$shortpct15 > 0) & (!is.na(con$shortpct15))) * 1
lab$nonzero <- ((lab$shortpct15 > 0) & (!is.na(lab$shortpct15))) * 1
ld$nonzero <- ((ld$shortpct15 > 0) & (!is.na(ld$shortpct15))) * 1

con$party.win <- ((con$winner10 == "Conservative") & (!is.na(con$shortpct15))) * 1
lab$party.win <- ((lab$winner10 == "Labour") & (!is.na(lab$shortpct15))) * 1
ld$party.win <- ((ld$winner10 == "Liberal Democrat") & (!is.na(ld$shortpct15))) * 1

sum(con$`Short Total Spend`[con$shortpct15 > 0 & con$shortpct15 < 101], na.rm = TRUE)
sum(lab$`Short Total Spend`[lab$shortpct15 > 0 & lab$shortpct15 < 101], na.rm = TRUE)
sum(ld$`Short Total Spend`[ld$shortpct15 > 0 & ld$shortpct15 < 101], na.rm = TRUE)

describe(con$short15)
describe(lab$short15)
describe(ld$short15)

con$short.dist <- cut(con$short15, seq(0, 100, 10), right = FALSE, labels = c(1:10))
lab$short.dist <- cut(lab$short15, seq(0, 100, 10), right = FALSE, labels = c(1:10))
ld$short.dist <- cut(ld$short15, seq(0, 100, 10), right = FALSE, labels = c(1:10))

prop.table(table(con$short.dist))*100
prop.table(table(lab$short.dist))*100
prop.table(table(ld$short.dist))*100


