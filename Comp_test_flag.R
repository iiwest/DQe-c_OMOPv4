source("dmtest.R")

###########################################
############ COMPLETENESS ANALYSIS ########
###################++++++##################
##This scripts is for flag generation


################################################################
####This loop goes through all tables and columns in each tables and generates a code for the NULL or NA values
###############################################################
##Results will be stored in the original table
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
    ##extracting column names
    DQIMPLVL <- filter(DQTBL, DQTBL$ColNam == col & DQTBL$TabNam == NAM)
    ##subsetted DQTBL for each column 
    LVL <- DQIMPLVL[,3]
    ##extracted the importance value from column 3 of the subsetted row
    abbr <- DQIMPLVL[,4]
    #extracted the abbreviated table name from column 4 of the subsetted row
    dt <- DQIMPLVL[,5]
    #extracted the testing date from column 5 of the sunsetted row
    
    TBL[,col] <- ifelse(is.null(TBL[,col]) | is.na(TBL[,col]), 
                        paste(LVL,"_",abbr,"_","MS1","_",TBL[,1],"_",which(colnames(TBL)== col),"_",dt,
                              sep=""),TBL[,col])
    ##calculating flag values to replace NAs and NULLs 
  }
  assign(TB, TBL)
}


#############################################################################
##a loop to go through all columns in all tables and count rows with a NULL/NA flag 
## and store in DQTBL table as a new column, called MSFRQ, for each row
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
    Sub <- nrow(TBL[grep("_MS1_", TBL[,col]), ])
    ##calculated number of rows with MS (missingness) as DQ Issue lable
    DQTBL$MS1_FRQ <- ifelse(DQTBL$ColNam == col & DQTBL$TabNam == NAM, Sub, DQTBL$MS1_FRQ )
    ##stored frequency of DQ issues (missingness here) in the culumn MS_FRQ
  }
}








################################################################
####This loop goes through all tables and columns in each tables and generates a code for the "" (nothing) values
###############################################################
##Results will be stored in the original table
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
    ##extracting column names
    DQIMPLVL <- filter(DQTBL, DQTBL$ColNam == col & DQTBL$TabNam == NAM)
    ##subsetted DQTBL for each column 
    LVL <- DQIMPLVL[,3]
    ##extracted the importance value from column 3 of the subsetted row
    abbr <- DQIMPLVL[,4]
    #extracted the abbreviated table name from column 4 of the subsetted row
    dt <- DQIMPLVL[,5]
    #extracted the testing date from column 5 of the sunsetted row
    
    TBL[,col] <- ifelse(TBL[,col] == "", 
                        paste(LVL,"_",abbr,"_","MS2","_",TBL[,1],"_",which(colnames(TBL)== col),"_",dt,
                              sep=""),TBL[,col])
    ##calculating flag values to replace NAs and NULLs 
  }
  assign(TB, TBL)
}



#############################################################################
##a loop to go through all columns in all tables and count rows with a "" flag, 
# meaning that there is nothing in the cell, but also not marked as NULL/NA 
## and store in DQTBL table as a new column, called MSFRQ, for each row
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
    Sub <- nrow(TBL[grep("_MS2_", TBL[,col]), ])
    ##calculated number of rows with MS (missingness) as DQ Issue lable
    DQTBL$MS2_FRQ <- ifelse(DQTBL$ColNam == col & DQTBL$TabNam == NAM, Sub, DQTBL$MS2_FRQ )
    ##stored frequency of DQ issues (missingness here) in the culumn MS_FRQ
  }
}




################################################################################
##extracting DQ Issues from the table and storing them into a long table called RESULTS
################################################################################


##calculating percent missing compared to the entire rows in each column/table
DQTBL$MSs_PERC <- round((DQTBL$MS1_FRQ+DQTBL$MS2_FRQ)/DQTBL$FRQ,2)
##saving the master DQ table
write.csv(DQTBL, file = paste("reports/mstabs/DQ_Master_Table_",usrnm,"_",as.character(format(Sys.Date(),"%d-%m-%Y")),".csv", sep=""))

##creating a list of issue tables for each available table
OMOP <- as.data.frame(unique(DQTBL$TabNam))
OMOP$ISSUES <- "NULL"
names(OMOP) <- c("TabNAM", "ISSUENAM")
OMOP$ISSUENAM <- paste("ISSUES_",OMOP$TabNAM, sep = "")

issue_list <- list()
for (i in 1:length(OMOP$ISSUENAM)) {
  iss <- issue_list[i]
  iss <- assign(OMOP$ISSUENAM[i], "NULL")
}

for (i in 1: length(unique(OMOP$ISSUENAM))) 
  ##DQTBL$TabNam has all table names
{
  NAM <-  OMOP$ISSUENAM[i]
  NAM2 <-  OMOP$TabNAM[i]
  
  TB <- as.character(NAM)
  TB2 <- as.character(NAM2)
  ##saving the name of table j as characters
  TBL <- get(TB)
  TBL2 <- get(TB2)
  
  TBL <- sapply(1:length(TBL2), function(j) {c(TBL2[grep("_MS", TBL2[,j]), j])})
  TBL <- unlist(TBL)
  TBL <- data.frame(TBL)
  names(TBL) <- c("DQ_ISSUE_CODE")
  write.csv(TBL, file = paste("reports/flagtabs/flagged_cells_",NAM,"_",usrnm,"_",as.character(format(Sys.Date(),"%d-%m-%Y")),".csv", sep=""))
  assign(TB, TBL)
  rm(TBL, TBL2)
  
}

DQ_ISSUES <- list()
N <- length(unique(OMOP$ISSUENAM))
for (i in 1:N) 
  ##DQTBL$TabNam has all table names
{
  NAM2 <-  OMOP$TabNAM[i]
  TB2 <- as.character(NAM2)
  ##saving the name of table j as characters
  TBL2 <- get(TB2)
  DQ_ISSUES[[i]] <- sapply(1:length(TBL2), function(j) {c(TBL2[grep("_MS", TBL2[,j]), j])})
}

DQ_ISSUES <- unlist(DQ_ISSUES)
DQ_ISSUES <- data.frame(DQ_ISSUES)
names(DQ_ISSUES) <- c("DQ_ISSUE_CODE")
##saving all error cells is one table, DQ_ISSUES
write.csv(DQ_ISSUES, file = paste("reports/flagged_cells_",usrnm,"_",as.character(format(Sys.Date(),"%d-%m-%Y")),".csv", sep=""))


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
