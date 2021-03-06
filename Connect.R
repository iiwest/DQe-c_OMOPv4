#####################################################################################################################################
####### This script connects your R to your OMOP postgreSQL database and saves the preliminary data needed for further analysis.  ##


# read username and password
source("keys.R")

# set up connection
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv,
                 dbname = "TYPE YOUR DATABASE NAME HERE",
                 host = "TYPE YOUR HOST NAME HERE",
                 port = ????,
                 user = usrnm,
                 password = pss_OMOP
                 )
rm(pss_OMOP)

#######
######## If you don't know your data base name, host, or port, contact your OMOP server administrator.
#######

# create a vector of OMOP V4 tables -- to switch to OMOP v5, this needs to be updated
OMOPV4 <- c("care_site", "cohort", "condition_era", "condition_occurrence", "death", "drug_cost", "drug_era", "drug_exposure", 
            "location", "observation", "observation_period", "payer_plan_period", "person", "procedure_cost", "procedure_occurrence", 
            "provider", "visit_occurrence", "organization")  

# create a list of tables in the PostgreSQL database
list <- dbListTables(con)

# pick OMOP tables from all tables provided
OMOPtbls <- subset(list, list %in% OMOPV4)
OMOPtbls <- unique(OMOPtbls)

# create a version of the list to save as a .csv table
OMOPtbls2 <- data.frame(OMOPtbls)
colnames(OMOPtbls2)[1] <- "OMOP_Tables"
## write list of provided omop tables for the record
write.csv(OMOPtbls2, file = paste("reports/tablelist_",usrnm,"_",as.character(format(Sys.Date(),"%d-%m-%Y")),".csv", sep=""))


