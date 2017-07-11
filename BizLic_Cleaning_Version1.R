
### From BARI Google Drive ###
# Author: BARI
# 
#
# Install these packages if you don't have them.
#   
#    install_packages("lubridate")
#    ...
#


######################################################### Business Licenses Data Set ###############################################################

#Loading the packages

# suppressPackageStartupMessages(install.packages("stats"))
suppressPackageStartupMessages(library(stats))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(sp))
# suppressPackageStartupMessages(install.packages("ggmap"))
suppressPackageStartupMessages(library(ggmap))
suppressPackageStartupMessages(library(rgeos))
# suppressPackageStartupMessages(install.packages("maptools"))
suppressPackageStartupMessages(library(maptools))
# suppressPackageStartupMessages(install.packages("gridExtra"))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(dplyr))
# suppressPackageStartupMessages(install.packages("plyr"))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(stringr))
# suppressPackageStartupMessages(install.packages("ggplot2"))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(stringi))
suppressPackageStartupMessages(library(stringr))

# Loading the data
 ### Download the csv file from BARI's Google Drive
setwd("E:/BARI/Big Cities, Big Data/Business Licenses")
biz.raw <- read.csv("BL - Original dataset - Short version.csv", 
                    header = T, stringsAsFactors=F, na.strings = c("", " ", "NULL", "NA"))


# ---------------------------------------------
#              1. Helper Functions;l;
# ---------------------------------------------

## This function cleans the state names and compares them with the 
con.state <- function(n) {
    n <- toupper(gsub("[\\.\ ]", "", n))
    sn.full <- toupper(state.name)  # state.name is a built-in data source
    sn.abbr <- state.abb
    # add special cases ("MAq" looks like a typo)
    sn.full <- append(sn.full, c("MASS", "NEWYORK", "NULL", "MAQ"))
    sn.abbr <- append(sn.abbr, c("MA", "NY", "", "MA"))
    sapply(n, function(x) {
        ifelse(x %in% sn.full, sn.abbr[which(sn.full == x)], x)
    })
}

## This function only cleans the names.
con.name <- function(n) {
    n <- stri_trans_totitle(sub("/", "", n))
    n <- gsub("\\b(L L C|Llc)\\b", "LLC", n)
    n <- gsub("^NULL$", "", n)  # clear up NULL values
}

## This function cleans the zipcodes and unifies them
con.zip <- function(zip) {
    # consolidate zip codes
    #  - remove brackets ()
    #  - remove end dash
    #  - remove "NULL", replace with empty string
    zip <- gsub("[ \\)\\)]+", "", zip)
    zip <- gsub("-$", "", zip)
    zip <- gsub("NULL", "", zip)
}


# ---------------------------------------------
#              2. Cleaning the data
# ---------------------------------------------

# Renaming and rearranging the columns in the data frame
bizz.2 <- with(biz.raw, data.frame(
    BUSINESSNAME = con.name(BUSINESSNAME),
    DBANAME = con.name(DBANAME),
    NAMELAST = con.name(NAMELAST),
    NAMEFIRST = con.name(NAMEFIRST),
    LICENSENO = LICENSENO,
    ISSDTTM = as.Date(ISSDTTM, format = "%m/%d/%Y"),
    EXPDTTM = as.Date(EXPDTTM, format = "%m/%d/%Y"),
    LICSTATUS = LICSTATUS,
    LICENSECAT = LICENSECAT,
    FEEDESC = FEEDESC,
    DESCRIPT = DESCRIPT,
    AMT = AMT,
    TYPEOFBUS = TYPEOFBUS, 
    APKEY = APKEY,
    CNTCTKEY = CNTCTKEY,
    ADDRTYPE = ADDRTYPE,
    LEGALOWNER = LEGALOWNER,
    B.STNO = STNO,
    B.STNAME = STNAME,
    B.SUFFIX = SUFFIX,
    B.ADDRESS = paste(STNO, " ", STNAME, " ",SUFFIX), # Creating a new variable called address
    B.ZIP = con.zip(ZIP),
    B.CITY = con.name(CITY),
    B.STATE = con.state(STATE),
    B.ADDRKEY = ADDRKEY, 
    CO.NAME = con.name(CONAME),
    CO.CITY = con.name(Contact_City),    
    CO.DAYPHN = DAYPHN,      # renaming for consistency
    CO.ADDRESS = ADDR1,      # renaming for consistency
    CO.STATE = con.state(Contact_State),       # renaming for consistency
    CO.ZIP = con.zip(Contact_Zip),      # renaming for consistency
    CO.CONTACTTYPE = CONTACTTYPE
    # Categories not included
    # NAMEMID
    # PRI
    # PREDIR
    # STAT
    
))

### Cleaning LICENSENO of bad license numbers
bizz.2 <- bizz.2[grep("^[a-zA-Z0-9\\-]+$", bizz.2$LICENSENO), ]

### Cleaning LICSTATUS
bizz.2$LICSTATUS <- gsub("inactive", "Inactive", bizz.2$LICSTATUS, ignore.case = T)


# ---------------------------------------------
#              3. Adding metrics
# ---------------------------------------------

# Unique ID (ORIGINAL)
bizz.2$UNIQUE.ID = paste(bizz.2$B.ADDRKEY, bizz.2$NAMELAST)
# bizz.2$UNIQUE.ID = paste(bizz.2$B.Property_ID, bizz.2$NAMELAST) 
# bizz.2$UNIQUE.ID = paste(bizz.2$B.Property_ID, bizz.2$BUSINESSNAME)
# bizz.2$UNIQUE.ID = paste(bizz.2$B.ADDRKEY, bizz.2$NAMELAST, bizz.2$B.STNO, bizz.2$B.STNAME)


#Georefence
bizz.mike <- read.csv("Licenses.Records.2015.csv")
bizz.mike.2 <- bizz.mike[,c(32:42)]
bizz.mike.2 <- bizz.mike.2[match(unique(bizz.mike.2$ADDRKEY), bizz.mike.2$ADDRKEY), ]
colnames(bizz.mike.2) <- c("B.ADDRKEY", "B.Property_ID", "B.X", "B.Y", "B.LocationID", "B.TLID", "B.Blk_ID_10",
                           "B.BG_ID_10", "B.CT_ID_10", "B.NSA_NAME", "B.BRA_PD")
bizz.2 <- bizz.2 %>% left_join(bizz.mike.2, by = "B.ADDRKEY")

# Years
bizz.2$START.YEAR <- year(bizz.2$ISSDTTM)
bizz.2$END.YEAR <- year(bizz.2$EXPDTTM)

# Adding three rows for the Categories based on the LICENSECAT (CAT.BY.LICENSECAT) and another row to 
# distingunon-business, business and ML licenses
bizz.2 <- bizz.2 %>% left_join(lic.cat.licensecat, by = "LICENSECAT")

# Adding three rows for the Categories based on the LICENSECAT (CAT.BY.LICENSECAT) and another row to 
# distingunon-business, business and ML licenses
bizz.2 <- bizz.2 %>% left_join(lic.cat.desc, by = "FEEDESC")

# Renewals or not?
bizz.2$RENEW <- ifelse(grepl("renew", bizz.2$FEEDESC, ignore.case = T),1,NA)

# Late fees
bizz.2$LATE <- ifelse(grepl("late", bizz.2$FEEDESC, ignore.case = T),1,NA)

# New variable to findout if the business zip code is the same as the contact zip code of the owner
boston.zip.codes <- c(
    02108, 02109, 02110, 02111, 02113, 02114, 02115, 02116,
    02118, 02119, 02120, 02121, 02122, 02124, 02125, 02126,
    02127, 02128, 02129, 02130, 02131, 02132, 02134, 02135,
    02136, 02150, 02151, 02152, 02210, 02215, 02445, 02446,
    02447, 02467)

boston.zip.codes <- as.factor(boston.zip.codes)

bizz.2$LOCAL <- ifelse(bizz.2$CO.ZIP %in% boston.zip.codes, "Local","Non-local")


# Valid date or not? (1 = valid, 0 = invalid)
bizz.2$DTTM_VALID <- ifelse(bizz.2$ISSDTTM <= bizz.2$EXPDTTM, 1, NA)

# Variables for activity in a certain year
bizz.2$y.2007 <- ifelse(bizz.2$START.YEAR <= 2007 & bizz.2$END.YEAR >= 2007,1,NA)
bizz.2$y.2008 <- ifelse(bizz.2$START.YEAR <= 2008 & bizz.2$END.YEAR >= 2008,1,NA)
bizz.2$y.2009 <- ifelse(bizz.2$START.YEAR <= 2009 & bizz.2$END.YEAR >= 2009,1,NA)
bizz.2$y.2010 <- ifelse(bizz.2$START.YEAR <= 2010 & bizz.2$END.YEAR >= 2010,1,NA)
bizz.2$y.2011 <- ifelse(bizz.2$START.YEAR <= 2011 & bizz.2$END.YEAR >= 2011,1,NA)
bizz.2$y.2012 <- ifelse(bizz.2$START.YEAR <= 2012 & bizz.2$END.YEAR >= 2012,1,NA)
bizz.2$y.2013 <- ifelse(bizz.2$START.YEAR <= 2013 & bizz.2$END.YEAR >= 2013,1,NA)
bizz.2$y.2014 <- ifelse(bizz.2$START.YEAR <= 2014 & bizz.2$END.YEAR >= 2014,1,NA)
bizz.2$y.2015 <- ifelse(bizz.2$START.YEAR <= 2015 & bizz.2$END.YEAR >= 2015,1,NA)
bizz.2$y.2016 <- ifelse(bizz.2$START.YEAR <= 2016 & bizz.2$END.YEAR >= 2016,1,NA)
bizz.2$y.2017 <- ifelse(bizz.2$START.YEAR <= 2017 & bizz.2$END.YEAR >= 2017,1,NA)

# Dummy column with only ones for aggregation purposes
bizz.2$ONES <- 1

# Changing values by hand
bizz.2$START.YEAR[bizz.2$START.YEAR == "2105"] <-"2015"
bizz.2$END.YEAR[bizz.2$END.YEAR == "2108"] <-"2018"
bizz.2$END.YEAR[bizz.2$END.YEAR == "2109"] <-"2019"
bizz.2$END.YEAR[bizz.2$END.YEAR == "3015"] <-"2015"
bizz.2$END.YEAR[bizz.2$END.YEAR == "3017"] <-"2017"


# ---------------------------------------------
#              4. Business Licenses only
# ---------------------------------------------

###### Selection only businesses #####
bizz.bus <- bizz_2[(bizz_2$Bus.NotBus == "Business"), ]
bizz.bus <- bizz.bus[!is.na(bizz.bus$Bus.NotBus == "Business"), ]


### Business licenses active
bizz.bus.active <- bizz.bus[(bizz.bus$LICSTATUS == "Active"), ]

## Business licenses active uniques 
bizz.bus.active.unique <- bizz.bus.active[match(unique(bizz.bus.active$UNIQUE.ID), bizz.bus.active$UNIQUE.ID), ]


# -----------------------------------------------------------------
#               4.0.1 Writing CSV File for Business-Licenses Only
# -----------------------------------------------------------------

write.csv(bizz.2, file = "E:/BARI/Big Cities, Big Data/Business Licenses/bizz.2.csv", row.names=F)
bizz.2 <- read.csv("bizz.2.csv", header = T, stringsAsFactors=F, na.strings = c("", " ", "NULL", "NA"))

bizz.2 <- read.csv("bizz_2.csv", header = T, stringsAsFactors=F, na.strings = c("", " ", "NULL", "NA"))
bizz_2$UNIQUE.ID = paste(bizz_2$B.ADDRKEY, bizz_2$NAMELAST)
bizz_2$UNIQUE.ID = paste(bizz_2$B.Property_ID, bizz_2$NAMELAST) 
bizz_2$UNIQUE.ID = paste(bizz_2$B.Property_ID, bizz_2$BUSINESSNAME)
bizz.2$UNIQUE.ID = paste(bizz_2$B.ADDRKEY, bizz_2$NAMELAST, bizz_2$B.STNO, bizz_2$B.STNAME)

# ------------------------------------------------------
#              4.1. UNIQUES Business Licenses only
# ------------------------------------------------------

#### Business Licenses Uniques (in total)
bizz.bus.unique <- bizz.bus[match(unique(bizz.bus$UNIQUE.ID), bizz.bus$UNIQUE.ID), ]

# Aggregating the amount of licenses by UNIQUE.ID
lic.by.unique <- aggregate(bizz.bus$ONES ~ bizz.bus$UNIQUE.ID, data = bizz.bus, sum)
colnames(lic.by.unique) <- c("UNIQUE.ID", "Total.Licenses")

# Join with bizz.bus.unique
bizz.bus.unique <- bizz.bus.unique %>% left_join(lic.by.unique, by = "UNIQUE.ID")


# ---------------------------------------------
#              4.2. Business Licenses only (not temporary) 
# ---------------------------------------------

# Removing "Mobile" categories
bizz.ntemp <- bizz.bus
bizz.ntemp$CAT.BY.LICENSECAT[bizz.ntemp$CAT.BY.LICENSECAT == "Mobile Alcohol"] <- NA
bizz.ntemp$CAT.BY.LICENSECAT[bizz.ntemp$CAT.BY.LICENSECAT == "Mobile Food"] <- NA
bizz.ntemp$CAT.BY.LICENSECAT[bizz.ntemp$CAT.BY.LICENSECAT == "Mobile Entertainment"] <- NA
bizz.ntemp$CAT.BY.LICENSECAT[bizz.ntemp$CAT.BY.LICENSECAT == "Mobile Business"] <- NA

bizz.ntemp <- bizz.ntemp[!is.na(bizz.ntemp$CAT.BY.LICENSECAT), ]





# ---------------------------------------------
#             6. Dataset for Mike 
# ---------------------------------------------

# This dataset is a replica of the previous dataset called "bizz.2" but has the names that Mike used for his
# variables
bizz.new.mike <- bizz.bus


### Cleaning done to match Mike's original dataset
# Removing NA in UNIQUE.ID
bizz.new.mike <- bizz.new.mike[!is.na(bizz.new.mike$UNIQUE.ID), ] 

# Removing NA in ADDRESS
bizz.new.mike$B.ADDRESS[bizz.new.mike$B.ADDRESS == "         "] <- NA
bizz.new.mike <- bizz.new.mike[!is.na(bizz.new.mike$B.ADDRESS), ]











# ---------------------------------------------
#             10. Exporting datasets
# ---------------------------------------------

#Data set for the visualization project
bizz.vis <- bizz.ntemp
write.csv(bizz.vis, "Business_Visualizacion.csv", row.names = FALSE)


#Data set for the visualization project
# bizz.ML <- bizz.2[(bizz.2$ML == "ML"), ]
# bizz.ML <- bizz.ML [!is.na(bizz.ML$ML),]
write.csv(bizz.bus, "Business_ML.csv", row.names = FALSE)


# ---------------------------------------------
#              99. Code that worked before but I stop using
# ---------------------------------------------

### License Status
#Aggregation
results.licstatus <- aggregate(bizz.business$ONES ~ bizz.business$LICSTATUS, 
                               data = bizz.business, sum)
names(results.licstatus) <- c("Status", "Amount")
results.licstatus$Percentage <- round(results.licstatus$Amount/nrow(bizz.business),4)*100
results.licstatus <- results.licstatus[order(results.licstatus$Amount, decreasing=TRUE),]


### License Categories
#Aggregation
results.licensecat <- aggregate(bizz.business$ONES ~ bizz.business$DESCRIPT, 
                                data = bizz.business, sum)
names(results.licensecat) <- c("License Description", "Amount")
results.licensecat$Percentage <- round(results.licensecat$Amount/nrow(bizz.business),4)*100
results.licensecat <- results.licensecat[order(results.licensecat$Amount, decreasing=TRUE),]
results.licensecat.10 <- results.licensecat[c(1:10),]


#### Dates
#Aggregation
issued.license.year <- aggregate(bizz.business$ONES ~ bizz.business$START.YEAR, 
                                 data = bizz.business, sum)
names(issued.license.year) <- c("Year", "Issued licenses")
expiration.license.year <- aggregate(bizz.business$Business ~ bizz.business$END.YEAR, 
                                     data = bizz.business, sum)
names(expiration.license.year) <- c("Year", "Issued licenses")



###############

Contact GitHub API Training Shop Blog About
? 2017 GitHub, Inc. Terms Privacy Security Status Help