### finding out how many patients don't have specific health records.

### set the denominator
#total patients for which date of birth is available
people <- subset(person, !(is.na(person$year_of_birth) | is.null(person$year_of_birth) | person$year_of_birth == ""))
people_count <- length(unique(people$person_id))
people_id <- data.frame(ids= unique(people$person_id))


# a function to count patients without a given parameter
withoutit <- function(data,col1,col2,list,denominator) {
  df.name <- deparse(substitute(data))
  list.name <- deparse(substitute(list))
  
  dat0 <- data.frame(person_id=unique(subset(people_id$ids, !(people_id$ids %in% data[,col2]))))
  d00 <- length(dat0$person_id)
  d1 <- data.frame(person_id=unique(subset(data, !(data[,col2] %in% people_id$ids))))
  d10 <- length(d1$person_id)
  
  dat01 <- subset(data, data[,col2] %in% people_id$ids)
  
  NO <- subset(dat01[,col2],!(dat01[,col1] %in% list))
  NO <- data.frame(person_id=unique(NO))
  YES <- subset(dat01[,col2],dat01[,col1] %in% list)
  YES <- data.frame(person_id=unique(YES))
  NO2 <- subset(NO, !(NO[,1] %in% YES[,1]) )
  dat02 <- rbind(dat0,NO)
  dat02 <- subset(person, person$person_id %in% dat02$person_id)

  d00 <- round((d00/denominator)*100,4)
  d2 <- round((nrow(NO2)/denominator)*100,4)
  d20 <- round((nrow(NO)/denominator)*100,4)
  
  
  message(d2+d00, "% of patients are missing ", list.name," information.",appendLF=T)
  if (d10 > 0) message(d1, " unique patient ids not available in the source table.",appendLF=T)
  message(d20, "% of patients have at least 1 missing ", list.name," mapped concept id in ",df.name, " table.",appendLF=T)
  if (d2 != d20) message("of the ",d2+d00,"%, ", d2, "% miss ", list.name," mapped concept ids in ",df.name, " table.",appendLF=T)
  message("of the ",d2+d00,"%, ",d00, "% don't have a ", list.name," record in ",df.name, " table.",appendLF=T)
  output <- data.frame(group=list.name, missing = d2+d00)
  return(output)
  rm(NO,NO2,YES,dat0,dat01,d1,dat02)
}

## a function to count patients that are not available in the list of certain condition/drug/lab/...
withoutthem <- function(data,col1,col2,list,denominator) {
  df.name <- deparse(substitute(data))
  list.name <- deparse(substitute(list))
  # colx = paste(df.name,"_",col1,sep = "")
  
  dat <- subset(data, data[,col1] %in% list)
  
  dat0 <- data.frame(person_id=unique(subset(people_id$ids, !(people_id$ids %in% dat[,col2]))))
  d00 <- length(dat0$person_id)
  d00 <- round((d00/denominator)*100,4)
  d1 <- ifelse(dim(dat)[1] == "0","0",length(unique(subset(dat[,col2], !(dat[,col2] %in% people_id$ids)))))
  
  message(d00, "% of unique patients don't have a '", list.name,"' record in ",df.name, " table.",appendLF=T)
  if (d1 > 0) message(d1, " unique patient ids not available in the source table.",appendLF=T)
  output <- data.frame(group=list.name, missing = d00)
  return(output)
  rm(dat,dat0)
}


  
  ##gender
  table(person$gender_concept_id)

  #define the only wanted values
  gender <- c(unique(subset(person$gender_concept_id,!(person$gender_concept_id == 0))))
  
  without_gender <- withoutit(data = person,col1 = "gender_concept_id",col2 = "person_id", list = gender,denominator = people_count)

  
  ##race -- make sure we understand what values are in accepted list!
  table(person$race_concept_id)
  race <- c(unique(subset(person$race_concept_id, !(person$race_concept_id == 0))))
  
  without_race <- withoutit(data = person,col1 = "race_concept_id",col2 = "person_id", list = race, denominator = people_count)
  

  #ethnicity
  table(person$ethnicity_concept_id)
  ethnicity <- c(unique(subset(person$ethnicity_concept_id, !(person$ethnicity_concept_id == 0))))
  
  without_ethnicity <- withoutit(data = person,col1 = "ethnicity_concept_id",col2 = "person_id", list = ethnicity,denominator = people_count)
  

  # medication
  drug_exposure$with <- "YES"
  drug_exposure$with <- ifelse((drug_exposure$drug_concept_id == 0), "NO", drug_exposure$with)

  #define the only wanted values
  medication <- c("YES")
  # 

  without_medication <- withoutit(data = drug_exposure,col1 = "with",col2 = "person_id", list = medication, denominator = people_count)
  drug_exposure$with <- NULL
  
  
  #Dx -------------
  condition_occurrence$with <- "YES"
  condition_occurrence$with <- ifelse(condition_occurrence$condition_concept_id == 0, "NO", condition_occurrence$with)
  diagnosis <- c("YES")
  
  without_diagnosis <- withoutit(data = condition_occurrence,col1 = "with",col2 = "person_id", list = diagnosis, denominator = people_count)
  condition_occurrence$with <- NULL
  
  
  #Encounter -------------
  visit_occurrence$with <- "YES"
  visit_occurrence$with <- ifelse((visit_occurrence$care_site_id == "" | (is.na(visit_occurrence$care_site_id))), "NO", visit_occurrence$with)
  encounter <- c("YES")

  without_encounter <- withoutit(data = visit_occurrence,col1 = "with",col2 = "person_id", list = encounter, denominator = people_count)
  visit_occurrence$with <- NULL

  
  
  
  #############
  ##########
  #############
  #select the list you want
  weight <- c("29463-7","3025315")
  without_weight <- withoutthem(data = observation,col1 = "observation_concept_id","person_id",list=weight, denominator=people_count)
  
  height <- c("8302-2","3036277","146")
  without_height <- withoutthem(data = observation,col1 = "observation_concept_id","person_id",list=height, denominator=people_count)
  

  BP <- c("8462-4","FND_BPS","3012888","FND_BPD","275944005","3004249","8480-6")
  without_BP <- withoutthem(data = observation,col1 = "observation_concept_id","person_id",list=BP, denominator=people_count)
  
#   smoking <- c(
#     "40664586","2108525","SOC_Smoker","8392000","77176002","393644001","4298794","40664492","40756893","4222303","42740578","2617852",
#     "2617806","8517006","2514535","2721536","2514539","SOC_TobCess","44805624","2108526","4293153","2617449","40298679","440012000",
#     "44808272","225323000","SOC_NonSmoker","138001004","4254340","4254477","4310250","40664474","44808270","4257068","40572072","4248981",
#     "393646004","40572070","40664513","2617959","4206526","44808273","2721262","2721265","SOC_TobCess","SOC_PastSmoker","2721530","2617450","4021160",
#     "2514534","2514538","PROC1_TobCess","4237483")
#   without_smoking <- withoutthem(data = observation,col1 = "observation_concept_id","person_id",list=smoking, denominator=people_count)
#   
#   
#   alcohol <- c(
#     "F10.180","F10.27", "F10.959","F10.988","2108674","SOC_ALCOHOL HEAVY USE","2617464","F10.282","4265900",
#     "F10.280","F10.19", "SOC_ALCOHOL COUNSEL","F10.981","2007785","F10.21", "F10.250","4275257","F10.221",
#     "413473000","2618108","F10.229","F10.239","F10.230","2108871","303.92", "F10.121","F10.232","F10.281","303.91", 
#     "2514536","F10.220","F10.120","303",    "303.9",  "F10.188","F10.94","F10.920","2721634","4261832","F10.151",
#     "SOC_ALCOHOL LIGHT USE","F10.99","2007787","4164527","F10.950","F10.259","4064043","F10.982","F10.288","F10.20",
#     "F10.980","303.02", "F10.129","F10.159","2514537","40664725","F10.96", "F10.24", "F10.14", "F10.182","F10.929",
#     "F10.251","2007786","4211325","F10.97", "F10.921","F10.231","303.93", "F10.29", "F10.10", "2617465","303.01","F10.150" 
#   )
#   without_alcohol <- withoutthem(data = observation,col1 = "observation_concept_id","person_id",list=alcohol, denominator=people_count)
  
  
  withouts <- rbind(without_encounter,without_diagnosis,without_medication,without_ethnicity,without_race,without_gender,without_weight,
                    without_height,without_BP)
  
  
  #a function to print out percentages for the text labels
  percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(x, format = format, digits = digits, ...), "%")
  }
  withouts$perc <- percent(withouts$missing)
  
  write.csv(withouts, file = paste("reports/withouts_",usrnm,"_",as.character(format(Sys.Date(),"%d-%m-%Y")),".csv", sep=""))
  
  
  

    
  



  
  
    
  