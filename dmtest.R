source("Comp_prep.R")

source("Without.R")
#############################################################################
##A loop to go through all columns in all tables and count number of rows 
##Results will be stored in column "FRQ" of the DQTBL table
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
    FRQ <- length(TBL[,col])
    ##calculated length (number of total rows) of each column from each table
    DQTBL$FRQ <- ifelse(DQTBL$ColNam == col & DQTBL$TabNam == NAM, FRQ, DQTBL$FRQ )
    ##stored frequency in the culumn FRQ
  }
}



#############################################################################
##a loop to go through all columns in all tables and count number of UNIQUE rows 
##Results will be stored in column "UNIQFRQ" of the DQTBL table
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
    UNIQ <- length(unique(TBL[,col]))
    ##calculated length unique values in each column (number of unique rows) from each table
    DQTBL$UNIQFRQ <- ifelse(DQTBL$ColNam == col & DQTBL$TabNam == NAM, UNIQ, DQTBL$UNIQFRQ)
    ##stored frequency of unique values in the culumn UNIQFRQ
  }
}


## create data frame to store only columns and tables that have related key and/or primary keys for data model test 
DQTBL_KEYS <- select(subset(DQTBL, ColNam %in% c("person_id","care_site_id","visit_occurrence_id","location_id","organization_id")),TabNam, ColNam, UNIQFRQ)


################################################################################################################################
################################################################################################################################
################################################################################################################################
########## THIS SCRIPT CHECKS SOME DATA MODEL RELATED STUFF
################################################################################################################################
################################################################################################################################


###### this test is working based on DQTBL_KEYS which was created in the previous script

## creating an index for plotting: Count In means number rof unique frequencies that exist in the reference table
DQTBL_KEYS$Index <- "Count_In"
# Reference means that the column value is reference for all other tables that have the same column
DQTBL_KEYS$Index <- ifelse(((DQTBL_KEYS$TabNam == "person" & DQTBL_KEYS$ColNam == "person_id") |
                            (DQTBL_KEYS$TabNam == "visit_occurrence" & DQTBL_KEYS$ColNam == "visit_occurrence_id") |
                            (DQTBL_KEYS$TabNam == "organization" & DQTBL_KEYS$ColNam == "organization_id") |
                            (DQTBL_KEYS$TabNam == "care_site" & DQTBL_KEYS$ColNam == "care_site_id") |
                            (DQTBL_KEYS$TabNam == "location" & DQTBL_KEYS$ColNam == "location_id")),
                           "Reference",
                           DQTBL_KEYS$Index)



#Copy the data frame to store not counted ids (the ones that are not available in the reference coulumn)
DQTBL_KEYS2 <- subset(DQTBL_KEYS, DQTBL_KEYS$Index != "Reference")
DQTBL_KEYS2$Index <- "Count_Out"
DQTBL_KEYS2$UNIQFRQ <- 0

DQTBL_KEYS <- rbind(DQTBL_KEYS,DQTBL_KEYS2)

### Now let's count the number of unique ids that do not exist in the reference column and assigne related values to them
## and then subtracting the number of counted outs from the number of counted ins


#person id
DQTBL_KEYS[(DQTBL_KEYS$TabNam == "observation" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] <- 
  nrow(unique(subset(select(subset(observation, !(is.na(observation$person_id))),person_id), !(person_id %in% person$person_id))))

DQTBL_KEYS[(DQTBL_KEYS$TabNam == "observation" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] <- 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "observation" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] - 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "observation" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] 
  

DQTBL_KEYS[(DQTBL_KEYS$TabNam == "visit_occurrence" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] <- 
  nrow(unique(subset(select(subset(visit_occurrence, !(is.na(visit_occurrence$person_id))),person_id), !(person_id %in% person$person_id) )))

DQTBL_KEYS[(DQTBL_KEYS$TabNam == "visit_occurrence" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] <- 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "visit_occurrence" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] - 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "visit_occurrence" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] 


DQTBL_KEYS[(DQTBL_KEYS$TabNam == "drug_exposure" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] <- 
  nrow(unique(subset(select(subset(drug_exposure, !(is.na(drug_exposure$person_id))),person_id), !(person_id %in% person$person_id) )))

DQTBL_KEYS[(DQTBL_KEYS$TabNam == "drug_exposure" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] <- 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "drug_exposure" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] - 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "drug_exposure" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] 


DQTBL_KEYS[(DQTBL_KEYS$TabNam == "procedure_occurrence" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] <- 
  nrow(unique(subset(select(subset(procedure_occurrence, !(is.na(procedure_occurrence$person_id))),person_id), !(person_id %in% person$person_id) )))

DQTBL_KEYS[(DQTBL_KEYS$TabNam == "procedure_occurrence" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] <- 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "procedure_occurrence" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] - 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "procedure_occurrence" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] 


DQTBL_KEYS[(DQTBL_KEYS$TabNam == "condition_occurrence" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] <- 
  nrow(unique(subset(select(subset(condition_occurrence, !(is.na(condition_occurrence$person_id))),person_id), !(person_id %in% person$person_id) )))

DQTBL_KEYS[(DQTBL_KEYS$TabNam == "condition_occurrence" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] <- 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "condition_occurrence" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] - 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "condition_occurrence" & DQTBL_KEYS$ColNam == "person_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] 


#care site id
DQTBL_KEYS[(DQTBL_KEYS$TabNam == "person" & DQTBL_KEYS$ColNam == "care_site_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] <- 
  nrow(unique(subset(select(subset(person, !(is.na(person$care_site_id))),care_site_id), !(care_site_id %in% care_site$care_site_id))))

DQTBL_KEYS[(DQTBL_KEYS$TabNam == "person" & DQTBL_KEYS$ColNam == "care_site_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] <- 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "person" & DQTBL_KEYS$ColNam == "care_site_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] - 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "person" & DQTBL_KEYS$ColNam == "care_site_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] 


DQTBL_KEYS[(DQTBL_KEYS$TabNam == "provider" & DQTBL_KEYS$ColNam == "care_site_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] <- 
  nrow(unique(subset(select(subset(provider, !(is.na(provider$care_site_id))),care_site_id), !(care_site_id %in% care_site$care_site_id))))

DQTBL_KEYS[(DQTBL_KEYS$TabNam == "provider" & DQTBL_KEYS$ColNam == "care_site_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] <- 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "provider" & DQTBL_KEYS$ColNam == "care_site_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] - 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "provider" & DQTBL_KEYS$ColNam == "care_site_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] 


DQTBL_KEYS[(DQTBL_KEYS$TabNam == "visit_occurrence" & DQTBL_KEYS$ColNam == "care_site_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] <- 
  nrow(unique(subset(select(subset(visit_occurrence, !(is.na(visit_occurrence$care_site_id))),care_site_id), !(care_site_id %in% care_site$care_site_id))))

DQTBL_KEYS[(DQTBL_KEYS$TabNam == "visit_occurrence" & DQTBL_KEYS$ColNam == "care_site_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] <- 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "visit_occurrence" & DQTBL_KEYS$ColNam == "care_site_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] - 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "visit_occurrence" & DQTBL_KEYS$ColNam == "care_site_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] 


#visit_occurrence_id
DQTBL_KEYS[(DQTBL_KEYS$TabNam == "condition_occurrence" & DQTBL_KEYS$ColNam == "visit_occurrence_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] <- 
  nrow(unique(subset(select(subset(condition_occurrence, !(is.na(condition_occurrence$visit_occurrence_id))),visit_occurrence_id), 
            !(visit_occurrence_id %in% visit_occurrence$visit_occurrence_id))))

DQTBL_KEYS[(DQTBL_KEYS$TabNam == "condition_occurrence" & DQTBL_KEYS$ColNam == "visit_occurrence_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] <- 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "condition_occurrence" & DQTBL_KEYS$ColNam == "visit_occurrence_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] - 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "condition_occurrence" & DQTBL_KEYS$ColNam == "visit_occurrence_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] 


DQTBL_KEYS[(DQTBL_KEYS$TabNam == "drug_exposure" & DQTBL_KEYS$ColNam == "visit_occurrence_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] <- 
  nrow(unique(subset(select(subset(drug_exposure, !(is.na(drug_exposure$visit_occurrence_id))),visit_occurrence_id), 
            !(visit_occurrence_id %in% visit_occurrence$visit_occurrence_id))))

DQTBL_KEYS[(DQTBL_KEYS$TabNam == "drug_exposure" & DQTBL_KEYS$ColNam == "visit_occurrence_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] <- 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "drug_exposure" & DQTBL_KEYS$ColNam == "visit_occurrence_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] - 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "drug_exposure" & DQTBL_KEYS$ColNam == "visit_occurrence_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] 


DQTBL_KEYS[(DQTBL_KEYS$TabNam == "observation" & DQTBL_KEYS$ColNam == "visit_occurrence_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] <- 
  nrow(unique(subset(select(subset(observation, !(is.na(observation$visit_occurrence_id))),visit_occurrence_id), 
            !(visit_occurrence_id %in% visit_occurrence$visit_occurrence_id))))

DQTBL_KEYS[(DQTBL_KEYS$TabNam == "observation" & DQTBL_KEYS$ColNam == "visit_occurrence_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] <- 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "observation" & DQTBL_KEYS$ColNam == "visit_occurrence_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] - 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "observation" & DQTBL_KEYS$ColNam == "visit_occurrence_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] 


DQTBL_KEYS[(DQTBL_KEYS$TabNam == "procedure_occurrence" & DQTBL_KEYS$ColNam == "visit_occurrence_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] <- 
  nrow(unique(subset(select(subset(procedure_occurrence, !(is.na(procedure_occurrence$visit_occurrence_id))),visit_occurrence_id), 
            !(visit_occurrence_id %in% visit_occurrence$visit_occurrence_id))))

DQTBL_KEYS[(DQTBL_KEYS$TabNam == "procedure_occurrence" & DQTBL_KEYS$ColNam == "visit_occurrence_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] <- 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "procedure_occurrence" & DQTBL_KEYS$ColNam == "visit_occurrence_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] - 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "procedure_occurrence" & DQTBL_KEYS$ColNam == "visit_occurrence_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] 




# location_id
DQTBL_KEYS[(DQTBL_KEYS$TabNam == "person" & DQTBL_KEYS$ColNam == "location_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] <- 
  nrow(unique(subset(select(subset(person, !(is.na(person$location_id))),location_id), 
            !(location_id %in% location$location_id))))

DQTBL_KEYS[(DQTBL_KEYS$TabNam == "person" & DQTBL_KEYS$ColNam == "location_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] <- 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "person" & DQTBL_KEYS$ColNam == "location_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] - 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "person" & DQTBL_KEYS$ColNam == "location_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] 


DQTBL_KEYS[(DQTBL_KEYS$TabNam == "care_site" & DQTBL_KEYS$ColNam == "location_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] <- 
  nrow(unique(subset(select(subset(care_site, !(is.na(care_site$location_id))),location_id), 
            !(location_id %in% location$location_id))))

DQTBL_KEYS[(DQTBL_KEYS$TabNam == "care_site" & DQTBL_KEYS$ColNam == "location_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] <- 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "care_site" & DQTBL_KEYS$ColNam == "location_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] - 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "care_site" & DQTBL_KEYS$ColNam == "location_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] 


DQTBL_KEYS[(DQTBL_KEYS$TabNam == "organization" & DQTBL_KEYS$ColNam == "location_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] <- 
  nrow(unique(subset(select(subset(organization, !(is.na(organization$location_id))),location_id), 
            !(location_id %in% location$location_id))))

DQTBL_KEYS[(DQTBL_KEYS$TabNam == "organization" & DQTBL_KEYS$ColNam == "location_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] <- 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "organization" & DQTBL_KEYS$ColNam == "location_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] - 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "organization" & DQTBL_KEYS$ColNam == "location_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] 

# organization_id
DQTBL_KEYS[(DQTBL_KEYS$TabNam == "care_site" & DQTBL_KEYS$ColNam == "organization_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] <- 
  nrow(unique(subset(select(subset(care_site, !(is.na(care_site$organization_id))),organization_id), 
            !(organization_id %in% organization$organization_id))))

DQTBL_KEYS[(DQTBL_KEYS$TabNam == "care_site" & DQTBL_KEYS$ColNam == "organization_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] <- 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "care_site" & DQTBL_KEYS$ColNam == "organization_id" & DQTBL_KEYS$Index == "Count_In"),"UNIQFRQ"] - 
  DQTBL_KEYS[(DQTBL_KEYS$TabNam == "care_site" & DQTBL_KEYS$ColNam == "organization_id" & DQTBL_KEYS$Index == "Count_Out"),"UNIQFRQ"] 


write.csv(DQTBL_KEYS, file = paste("reports/DM_",usrnm,"_",as.character(format(Sys.Date(),"%d-%m-%Y")),".csv", sep=""))


















