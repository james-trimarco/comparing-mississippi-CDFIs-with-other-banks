#import libraries
library("dplyr")
library("magrittr")
library("ggvis")
library("stringr")

#set working directory
setwd("~/Desktop/Mississippi Data/hmda14")

#Read in all three files for the 2014 Home Mortgage Disclosure Act.
a <- read.csv("hmda14a.csv", header = T)
b <- read.csv("hmda14b.csv", header = T)
c <- read.csv("hmda14c.csv", header = T)

#Merge the three files together to create a complete version of the HMDA data.
hmda <- rbind(a, b, c)

#Make it a data table for dplyr.
hmda <- tbl_df(hmda)

#Set some specific columns to correct data class, and specify the NAs.
hmda$APP_INC <- as.integer(hmda$APP_INC)
hmda$APP_INC[hmda$APP_INC==1] <- NA
hmda$APP_INCCH <- as.integer(hmda$APP_INCCH)
hmda$RESP_ID <- as.character(hmda$RESP_ID)

#Bring in a database with the names of all the banks and their numeric IDs.
lookup <- read.csv('Lookups/rp.csv')
lookup <- tbl_df(lookup)
banknames <- lookup %>% select(RESP_ID, RESP_NAME_PNL)
banknames$RESP_NAME_PNL <- as.character(banknames$RESP_NAME_PNL)
banknames$RESP_ID <- as.character(banknames$RESP_ID)
banknames$RESP_ID <- str_pad(banknames$RESP_ID, 10, "left", "0")

#Merge the two databases using the numeric IDs.
#Now the data frame contains the names of the banks.
hmda <- hmda %>% left_join(banknames, by = "RESP_ID")

#Bring in a database with asset size for all banks.
assets <- lookup %>% select(RESP_ID, ASSETS)
assets$RESP_ID <- str_pad(assets$RESP_ID, 10, "left", "0")

#Merge the databases using the numeric IDs.
#Now the data frame contains the banks' asset sizes.
hmda <- hmda %>% left_join(assets, by = "RESP_ID")

#Limit the results to Mississippi.
MS <- filter(hmda, STATE==28)
MS <- tbl_df(MS)

#Bring in a database with the names of all the counties in Mississippi 
#and their FIPS codes.
FIPS <- read.csv("MS_FIPS.csv")
FIPS$CNTY.NAME <- as.character(FIPS$CNTY.NAME)

#Merge the names of all counties in Mississippi into the 
#HMDA data frame using the FIPS codes.
MS <- MS %>% left_join(FIPS, by = "COUNTY")

#Limit the results to the HMDA columns we're interested in.
MS <- select(MS, RESP_NAME_PNL, AMNT, ACTION, DENIAL1, APP_SEX, 
             APP_RACE1, APP_INCCH, SPREADCH, CNTY.NAME, RESP_ID)

#Rename some columns to make the table easier to read.
names(MS)[names(MS)=="RESP_NAME_PNL"] <- "BANK_NAME"
names(MS)[names(MS)=="SPREADCH"] <- "SPREAD"
names(MS)[names(MS)=="APP_RACE1"] <- "RACE"
names(MS)[names(MS)=="APP_INCCH"] <- "INCOME"
names(MS)[names(MS)=="CNTY.NAME"] <- "COUNTY"

#Create a list of all the Delta counties. Some of these are on the outskirts of the 
#region and are not distressed.
all_delta_counties <- c("BOLIVAR", "CARROLL", "COAHOMA", "DE SOTO", "GRENADA", "HOLMES", "HUMPHRIES", "ISSAQUENA", 
                        "LEFLORE", "PANOLA", "QUITMAN", "SHARKEY", "SUNFLOWER", "TALLAHATCHIE", 
                        "TATE", "TUNICA", "WARREN", "WASHINGTON", "YAZOO")

#Create a list of the 12 delta counties that are on the list of persistently
#poor counties. This is our primary geography of interest. 
delta_counties <- c("BOLIVAR", "COAHOMA", "HOLMES", "HUMPHRIES", "ISSAQUENA", 
                    "LEFLORE", "QUITMAN", "SHARKEY", "SUNFLOWER", "TALLAHATCHIE", 
                    "TUNICA", "WASHINGTON", "YAZOO")

#Subset the Mississippi data so we see only the 18 Delta counties.
all_delta <- MS[MS$COUNTY %in% all_delta_counties, ]

#Subset the Mississippi data so we see only the 12 persistently poor Delta counties.
delta <- MS[MS$COUNTY %in% delta_counties, ]

#merge in the CDFI certification status of all banks that considered applications
#in the poor Delta counties
CDFI <- read.csv("bankNames5.csv", stringsAsFactors = F)
CDFI$RESP_ID <- str_pad(CDFI$RESP_ID, 10, "left", "0")
delta <- delta %>% left_join(CDFI, by = "RESP_ID")

#Group the table by CDFI status of the financial institution,
#and by whether the loan was approved or denied.
by_action <- group_by(delta, ACTION, CDFI)

#Spit out a table with counts of how many loans were approved
#and denied by CDFIs and their mainstream counterparts.
results <- summarise(by_action,
                   count = n())

#Spit out a list of each financial institution and how many loans
#it approved in the poor Delta counties Separate line for CDFIs
#for mainstream institutions.
CDFI_approved <- delta %>% filter(CDFI==1) %>% filter(ACTION==1)
mains_approved <- delta %>% filter(CDFI==0) %>% filter(ACTION==1)
sort(table(CDFI_approved$BANK_NAME),decreasing=TRUE)
sort(table(filter(delta, CDFI==1, ACTION==1)),decreasing=TRUE)

sort(table(mains_approved$BANK_NAME),decreasing=TRUE)

#This plots the probability of approval against the applicant's income, 
#in CDFIs and in mainstream banks. A friend who's a biostatician wrote this code. 
delta %>% filter(ACTION %in% c(1,3)) %>%
        filter(CDFI %in% c(1,2)) %>%
        mutate(approved = (ACTION == 1)*1) %>%
        filter(INCOME < 75) %>%
        ggplot(.,aes(x=INCOME, y=approved)) + geom_point() + geom_smooth(method="loess") + facet_wrap(~CDFI)


#Removes outliers.
nonCDFI_approved_ctrl <- nonCDFI_approved[nonCDFI_approved$AMNT <= 1000, ]
CDFI_approved_ctrl <- CDFI_approved[CDFI_approved$AMNT <= 1000, ]

CDFI_approved_ctrl %>% ggvis(~APP_RACE1) %>% layer_histograms
nonCDFI_approved_ctrl %>% ggvis(~APP_RACE1) %>% layer_histograms

#CDFIs made 618 loans in the poor Delta counties in 2014. NonCDFIs made 1213.
#Median CDFI loan was $25K. Median nonCDFI loan was $64k. 
sort(table(nonCDFI_approved$RESP_NAME_PNL),decreasing=TRUE)
sort(table(CDFI_approved$RESP_NAME_PNL),decreasing=TRUE)

#CDFI borrowers skew somewhat less white. 
CDFI_approved %>% ggvis(~APP_RACE1) %>% layer_histograms()
nonCDFI_approved %>% ggvis(~APP_RACE1) %>% layer_histograms()

# Noah's junk

library(ggplot2)

#This plots the probability of approval against the size of the loan amount, in CDFIs 
#mainstream banks. 
delta %>% filter(ACTION %in% c(1,3)) %>%
        mutate(approved = (ACTION == 1)*1) %>%
        filter(AMNT < 350) %>%
        ggplot(.,aes(x=AMNT, y=approved)) + geom_point() + geom_smooth(method="loess") + facet_wrap(~CDFI)


#This plots the probability of approval against the applicant's income, in CDFIs 
#mainstream banks. 
labels <- c("1" = "CDFIs", "0" = "Mainstream Financial Institutions")
prob_chart <- delta %>% filter(ACTION %in% c(1,3)) %>%
        filter(CDFI %in% c(1,0)) %>%
        mutate(approved = (ACTION == 1)*1) %>%
        filter(INCOME < 75)
n <- ggplot(prob_chart, aes(x=INCOME, y=approved)) + geom_smooth(method="loess") + facet_wrap(~CDFI, labeller = labeller(CDFI = labels))
n <- n + scale_y_continuous(breaks = c(.1, .2, .3, .4, .5, .6 , .7, .8, .9, 1.0), limits=c(0, 1))
n <- n + labs(x = "Applicant Income (in thousands of dollars)", y = "Likelihood of Approval", title = "CDFIs Are Much More Likely to Approve Loans From
              Low-Income Delta Residents")
n


delta %>% filter(ACTION %in% c(1,3)) %>%
        filter(RACE %in% c(3,5)) %>%
        mutate(approved = (ACTION == 1)*1, race = as.factor(RACE)) %>%
        filter(AMNT < 350) %>%
        ggplot(.,aes(x=AMNT, y=approved, colour = race)) + geom_point() + geom_smooth(method="loess") + facet_wrap(~CDFI)

delta %>% filter(ACTION %in% c(1,3)) %>%
        filter(RACE %in% c(3,5)) %>%
        mutate(approved = (ACTION == 1)*1) %>%
        filter(INCOME < 75) %>%
        ggplot(.,aes(x=INCOME, y=approved, colour = as.factor(RACE))) + geom_point() + geom_smooth(method="loess") + facet_wrap(~CDFI)

hmda %>% filter(ACTION %in% c(1,3)) %>%
        filter(APP_RACE1 %in% c(3,5)) %>%
        mutate(approved = (ACTION == 1)*1, race = as.factor(APP_RACE1)) %>%
        filter(AMNT < 350) %>%
        ggplot(.,aes(x=AMNT, y=approved, colour = race)) + 
        geom_point() + geom_smooth(method="loess") #+ facet_wrap(~CDFI)


