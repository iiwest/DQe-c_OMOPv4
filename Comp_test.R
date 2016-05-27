source("dmtest.R")

###########################################
############ COMPLETENESS ANALYSIS ########
###################++++++##################
##reading the Comp_prep script


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

