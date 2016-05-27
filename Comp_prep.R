####################################
###preparations to run the analysis
####################################

source("Con.R")
##reading and storing OMOP tables
for (i in 1:length(OMOPtbls)) assign(OMOPtbls[i], dbGetQuery(con,paste("select * from omop.",OMOPtbls[i],"",sep="")))

if(length(person)!= 0) person <- person[,1:14]  #removing extra columns
if(length(provider)!= 0) provider <- provider[,1:7]  #removing extra columns
if(length(care_site)!= 0) care_site <- care_site[,1:6]  #removing extra columns
if(length(organization)!= 0) organization <- organization[,1:5]  #removing extra columns
if(length(location)!= 0) location <- location[,1:8]  #removing extra columns
if(length(drug_era)!= 0) drug_era <- drug_era[,1:7]  #removing extra columns
if(length(death)!= 0) death <- death[,1:5]  #removing extra columns
if(length(drug_exposure)!= 0) drug_exposure <- drug_exposure[,1:15]  #removing extra columns
if(length(visit_occurrence)!= 0) visit_occurrence <- visit_occurrence[,1:7]  #removing extra columns
if(length(observation)!= 0) observation <- observation[,1:17]  #removing extra columns
if(length(observation_period)!= 0) observation_period <- observation_period[,1:4]  #removing extra columns
if(length(payer_plan_period)!= 0) payer_plan_period <- payer_plan_period[,1:7]  #removing extra columns
if(length(condition_occurrence)!= 0) condition_occurrence <- condition_occurrence[,1:10]  #removing extra columns
if(length(procedure_occurrence)!= 0) procedure_occurrence <- procedure_occurrence[,1:9]  #removing extra columns
if(length(procedure_cost)!= 0) procedure_cost <- procedure_cost[,1:14]  #removing extra columns
if(length(cohort)!= 0) cohort <- cohort[,1:6]  #removing extra columns
if(length(condition_era)!= 0) condition_era <- condition_era[,1:7]  #removing extra columns

##store test date in mm-YYYY format
test_date <- as.character(format(Sys.Date(),"%m-%Y"))

# read the .csv file
DQTBL <- read.csv(file="DQTBL_v4.csv",head=TRUE,sep=",")



            # this piece of code that I modified from internet 
            # creates a data frame of all data frames in the 
            # global environment and calculates their 
            # size and number of columns and rows
            .ls.objects <- function (pos = 1, pattern, order.by,
                                     decreasing=FALSE, head=FALSE, n=5, row.names = 1) {
              napply <- function(names, fn) sapply(names, function(x)
                fn(get(x, pos = pos)))
              names <- ls(pos = pos, pattern = pattern)
              obj.class <- napply(names, function(x) as.character(class(x))[1])
              obj.mode <- napply(names, mode)
              obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
              obj.prettysize <- napply(names, function(x) {
                capture.output(format(utils::object.size(x), units = "auto")) })
              obj.size <- napply(names, object.size)
              obj.dim <- t(napply(names, function(x)
                as.numeric(dim(x))[1:2]))
              vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
              obj.dim[vec, 1] <- napply(names, length)[vec]
              out <- data.frame(names, obj.type, obj.size, obj.prettysize, obj.dim)
              names(out) <- c("names","Type", "Size", "PrettySize", "Rows", "Columns")
              if (!missing(order.by))
                out <- out[order(out[[order.by]], decreasing=decreasing), ]
              if (head)
                out <- head(out, n)
              out
            }
            
            # shorthand
            lsos <- function(..., n=1000) {
              .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
            }
            # the output
            lsos()
            
# selecting empty tables provided as part of OMOP data.            
empt_tables <- subset(lsos(), lsos()$Rows == 0 & lsos()$Columns == 0 & lsos()$Type == "data.frame")
empt_tables <- data.frame(empt_tables$names)
names(empt_tables) <- "Empty_Tables"


write.csv(empt_tables, file = paste("reports/empty_tablelist_",usrnm,"_",as.character(format(Sys.Date(),"%d-%m-%Y")),".csv", sep=""))

## subsetting the empty tables from DQTBL to avoid problems running loops in the analysis phase
DQTBL <- subset(DQTBL, !(DQTBL$TabNam %in% empt_tables$Empty_Tables))

# copy DQTBL for date testing
dateTBL <-select(DQTBL[grep("_date", DQTBL[,"ColNam"]), ],TabNam, ColNam)

# add columns needed for completeness analysis
DQTBL$test_date <- as.factor(test_date)
DQTBL$FRQ <- 0
DQTBL$UNIQFRQ <- 0
DQTBL$MS1_FRQ <- 0 # for NULL/NAs
DQTBL$MS2_FRQ <- 0 # for ""s
DQTBL$MSs_PERC


### creating and savinf an source table that records details of tables lodead from the list of OMOP V4 tables 
OMOPtbls2$availability <- "Yes"
OMOPtbls2$index <- 1
OMOPtbls2$availability <- ifelse(OMOPtbls2$OMOP_Tables %in% empt_tables$Empty_Tables, "No",OMOPtbls2$availability)

bob <- data.frame(select(subset(lsos(), lsos()$names %in% OMOPtbls2$OMOP_Tables), names, Size, Rows, Columns))
rownames(bob) <- NULL
colnames(bob)[1] <- "OMOP_Tables"
OMOPtbls2 <- select(merge(OMOPtbls2, bob, by = "OMOP_Tables"), OMOP_Tables, availability, index, Size, Rows, Columns)
rm(bob)
write.csv(OMOPtbls2, file = paste("reports/load_details_",usrnm,"_",as.character(format(Sys.Date(),"%d-%m-%Y")),".csv", sep=""))


