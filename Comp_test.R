source("dmtest.R")

###########################################
############ COMPLETENESS ANALYSIS ########
###################++++++##################
##This scripts counts and stores frequency of missing values


#############################################################################
##a loop to go through all columns in all tables and count rows with a NULL/NA value 
## and store in DQTBL table as a new column, called MS1_FRQ, for each row
#############################################################################

for (j in 1: length(unique(DQTBL$TabNam))) 
  ##DQTBL$TabNam has all table names
{
  NAM <-  unique(DQTBL$TabNam)[j]
  ##extracted name of table j
  id.NAM <- which(DQTBL$TabNam == NAM)
  ##extracting the row numbers
  NAMTB <- DQTBL[id.NAM,]
  ##subsetting the DQTBL to only the rows from table j
  TB <- as.character(unique(DQTBL$TabNam)[j])
  ##saving the name of table j as characters
  TBL <- get(TB)
  ##creating a new table equal to table j
  
  for (i in 1:length(get(TB)))
    ##now going through the columns of table j
  {
    col <- names(get(TB))[i]
    Sub <- nrow(TBL[(is.null(TBL[,col]) | is.na(TBL[,col])), ])
    ##calculated number of rows with NULL/NAs
    DQTBL$MS1_FRQ <- ifelse(DQTBL$ColNam == col & DQTBL$TabNam == NAM, Sub, DQTBL$MS1_FRQ )
    ##stored frequency of DQ issues (missingness here) in the culumn MS1_FRQ
  }
}




#############################################################################
##a loop to go through all columns in all tables and count rows with a "" flag, 
# meaning that there is nothing in the cell, but also not marked as NULL/NA 
## and store in DQTBL table as a new column, called MS2_FRQ, for each row
#############################################################################

for (j in 1: length(unique(DQTBL$TabNam))) 
  ##DQTBL$TabNam has all table names
{
  NAM <-  unique(DQTBL$TabNam)[j]
  ##extracted name of table j
  id.NAM <- which(DQTBL$TabNam == NAM)
  ##extracting the row numbers
  NAMTB <- DQTBL[id.NAM,]
  ##subsetting the DQTBL to only the rows from table j
  TB <- as.character(unique(DQTBL$TabNam)[j])
  ##saving the name of table j as characters
  TBL <- get(TB)
  ##creating a new table equal to table j
  
  for (i in 1:length(get(TB)))
    ##now going through the columns of table j
  {
    col <- names(get(TB))[i]
    Sub2 <- nrow(TBL[(nzchar(TBL[,col]) == "FALSE" & !(is.null(TBL[,col]) | is.na(TBL[,col]))), ])
    ##calculated number of rows with empty string (missingness) 
    DQTBL$MS2_FRQ <- ifelse(DQTBL$ColNam == col & DQTBL$TabNam == NAM, Sub2, DQTBL$MS2_FRQ )
    ##stored frequency of DQ issues (missingness here) in the culumn MS2_FRQ
  }
}





##calculating percent missing compared to the entire rows in each column/table
DQTBL$MSs_PERC <- round((DQTBL$MS1_FRQ+DQTBL$MS2_FRQ)/DQTBL$FRQ,2)
##saving the master DQ table
write.csv(DQTBL, file = paste("reports/mstabs/DQ_Master_Table_",usrnm,"_",as.character(format(Sys.Date(),"%d-%m-%Y")),".csv", sep=""))






##### Creating FRQ_comp table to compare frequencies from MSDQ table over time.
path = "reports/mstabs"
msnames <- list.files(path)
n <- length(msnames)

##reading and storing master DQ tables
compr <- list()
N <- length(msnames)
for (n in 1:N) {
  compr[[n]] = data.frame(read.csv(paste0(path,"/",msnames[n],sep="")))
}

#binding the tables together to create a masters table

FRQ_comp <- subset(rbindlist(compr), (ColNam == "person_id" & TabNam == "person") |
                     (ColNam == "provider_id" & TabNam == "provider") |
                     (ColNam == "care_site_id" & TabNam == "care_site") |
                     (ColNam == "organization_id" & TabNam == "organization") |
                     (ColNam == "drug_exposure_id" & TabNam == "drug_exposure") |
                     (ColNam == "observation_id" & TabNam == "observation") |
                     (ColNam == "procedure_occurrence_id" & TabNam == "procedure_occurrence") |
                     (ColNam == "person_id" & TabNam == "death") |
                     (ColNam == "drug_era_id" & TabNam == "drug_era") |
                     (ColNam == "drug_cost_id" & TabNam == "drug_cost") |
                     (ColNam == "death_idvisit_occurrence_id" & TabNam == "visit_occurrence") |
                     (ColNam == "observation_period_id" & TabNam == "observation_period") |
                     (ColNam == "payer_plan_period_id" & TabNam == "payer_plan_period") |
                     (ColNam == "condition_occurrence_id" & TabNam == "condition_occurrence") |
                     (ColNam == "procedure_cost_id" & TabNam == "procedure_cost") |
                     (ColNam == "cohort_id" & TabNam == "cohort") |
                     (ColNam == "condition_era_id" & TabNam == "condition_era") |
                     (ColNam == "location_id" & TabNam == "location")
                   
)


write.csv(FRQ_comp, file = paste("reports/FRQ_comp_",usrnm,"_",as.character(format(Sys.Date(),"%d-%m-%Y")),".csv", sep=""))


