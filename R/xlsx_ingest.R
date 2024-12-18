# title:    ARD_Code_Generator_xlsx_V1
# purpose:  Single function to produce ARD code per output, based on CDISC Pilot Study metadata
# Author:   Malan Bosman
# Date:     14OCT2024

#ARD_code_generator_xlsx <- function(ARS_xlsx){
ARS_xlsx <- "inst/extdata/Common Safety Displays.xlsx"

# load libraries ----------------------------------------------------------

func_libraries <- function(){
  template <- "

# load libraries ----
library(tidyverse)
library(readxl)

  "
  code <- template
  return(code)
}

code_libraries <- func_libraries()
cat(code_libraries)
eval(parse(text=code_libraries))

# Read in ARS xlsx content ----------------------------------------------------
ListOfPlannedAnalyses <- read_excel(ARS_xlsx,
                                    sheet = 'MainListOfContents')
ListOfPlannedOutputs <- read_excel(ARS_xlsx,
                                   sheet = 'OtherListsOfContents')
DataSubsets <- read_excel(ARS_xlsx,
                          sheet = 'DataSubsets')
AnalysisSets <- read_excel(ARS_xlsx,
                           sheet = 'AnalysisSets')
AnalysisGroupings <- read_excel(ARS_xlsx,
                                sheet = 'AnalysisGroupings')
Analyses <- read_excel(ARS_xlsx,
                       sheet = 'Analyses')
AnalysisMethods <- read_excel(ARS_xlsx,
                              sheet = 'AnalysisMethods')


# load ADaM ---------------------------------------------------------------

func_ADaM <- function(){
  template <- "
# load ADaM ----
ADSL <- read_csv('inst/extdata/ADSL.csv')
ADAE <- read_csv('inst/extdata/ADAE.csv') %>%
  rename(TRT01A = TRTA)
# ADVS <- read_csv('inst/extdata/ADVS.csv') %>%
#   rename(TRT01A = TRTA)
  "
  code <- template
  return(code)
}

code_ADaM <- func_ADaM()

# Prework and loops ----------------------------------------------------

#Init work
Lopo <- ListOfPlannedOutputs # list of all planned outputs to loop
Lopa <- ListOfPlannedAnalyses %>%  # list of all planned analyses to loop
  fill(listItem_outputId) %>%
  filter(!is.na(listItem_analysisId)) %>%
  select(listItem_analysisId, listItem_outputId)


# for (i in 1:1) {            # loop through outputs
max_i = nrow(Lopo)
for (i in 1:max_i) {
  Output = Lopo[i,]$listItem_outputId
  OutputName = Lopo[i,]$listItem_name

  Anas <- Lopa %>%    # get all analyses for current output
    filter(listItem_outputId == Output)

  run_code <- ""    # variable to contain generated code
  combine_analysis_code <- "" # variable containing code to combine analyses

  # Programme header ----
  timenow <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  func_header <- function(OutputId, OutputName, date){
    template <- "
# Programme:    Generate code to produce ARD for outputidhere
# Output:       outputnamehere
# Date created: datehere

  "
    code <- gsub('outputidhere', OutputId, template)
    code <- gsub('outputnamehere', OutputName, code)
    code <- gsub('datehere', date, code)
    return(code)
  }

  code_header <- func_header(Output, OutputName, timenow)

  # loop through individual analyses
  max_j = nrow(Anas)
  for (j in 1:max_j) {
    # for (j in 1:nrow(Anas)) {

    #Analysis
    Anas_j <- Anas[j, ]$listItem_analysisId  # AnalysisID from PA to filter AN
    Anas_s <- Analyses %>% # row from AN to get other IDs
      filter(id == Anas_j)

    ana_adam <- Anas_s$dataset # ADaM used for this analysis (esp. to be used in ChiSq)

    # Analysis Set
    ana_setId <- Anas_s$analysisSetId # AS ID (to be used in AS)
    ana_var <- Anas_s$variable #AS variable (to be used in MT)

    # Analysis Grouping
    groupid1 <- Anas_s$groupingId1    #group ID (to be used in AG)
    resultsByGroup1 <- Anas_s$resultsByGroup1 # Y/N group by
    groupid2 <- Anas_s$groupingId2
    resultsByGroup2 <- Anas_s$resultsByGroup2
    groupid3 <- Anas_s$groupingId3
    resultsByGroup3 <- Anas_s$resultsByGroup3

    # Data Subset
    subsetid <- Anas_s$dataSubsetId # data subset ID (to be used in DS

    # Method
    methodid <- Anas_s$method_id # data subset ID (to be used in DS

    # Apply Analysis Set -----
    temp_AnSet <- AnalysisSets %>%  # get analysis set for this iteration
      filter(id == ana_setId)

    cond_adam <- temp_AnSet %>% # ADaM for this analysis set
      select(condition_dataset) %>%
      as.character()

    cond_var <- temp_AnSet %>% # condition variable for this analysis set
      select(condition_variable) %>%
      as.character()

    cond_oper <- temp_AnSet %>% # condition operator for this analysis set
      select(condition_comparator) %>%
      as.character()

    if(!is.na(temp_AnSet %>% # if condition value is not blank
              select(condition_value))){
      cond_val <- temp_AnSet %>% # condition value for this analysis set
        select(condition_value)%>%
        as.character()
    } else { #if condition value is blank
      cond_val <- ""
    }


    anSetName <- temp_AnSet %>% # analysis set name for this analysis set
      select(name)%>%
      as.character()

    if(cond_oper == "EQ") { # convert to R code
      oper <-  '=='
    } else if(cond_oper == "NE"){
      oper <- '!='
    }

    if(cond_adam == ana_adam){    # if Analysis Set ADaM and Analysis ADaM are same

      func_AnalysisSet <- function(dataset, variable, oper, val, ASID, anSetName) {
        template <- "
# Apply Analysis Set ---
# Analysis set :  Analysissetnamehere
df_analysisidhere <- filter(ADaM,
            var operator 'value')

"
        code <- gsub('ADaM', dataset, template)
        code <- gsub('var', variable, code)
        code <- gsub('operator', oper, code)
        code <- gsub('value', val, code)
        code <- gsub('analysisidhere', ASID, code)
        code <- gsub('Analysissetnamehere', anSetName, code)

        return(code)
      }

      assign(paste0("code_AnalysisSet_",Anas_j), func_AnalysisSet(cond_adam,
                                                                  cond_var,
                                                                  oper,
                                                                  cond_val,
                                                                  Anas_j,
                                                                  anSetName))

    }
    else {

      func_AnalysisSet <- function(dataset, variable, oper, val, ASID, anaADaM, anSetName) {
        template <- "
# Apply Analysis Set ---
# Analysis set :  Analysissetnamehere
df_analysisidhere <- filter(ADaM,
            var operator 'value') %>%
            select(USUBJID) %>%
            merge(analysisADAMhere,
                  by = 'USUBJID')

"
        code <- gsub('ADaM', dataset, template)
        code <- gsub('var', variable, code)
        code <- gsub('operator', oper, code)
        code <- gsub('value', val, code)
        code <- gsub('analysisidhere', ASID, code)
        code <- gsub('analysisADAMhere', anaADaM, code)
        code <- gsub('Analysissetnamehere', anSetName, code)

        return(code)
      }

      assign(paste0("code_AnalysisSet_",Anas_j), func_AnalysisSet(cond_adam,
                                                                  cond_var,
                                                                  oper,
                                                                  cond_val,
                                                                  Anas_j,
                                                                  ana_adam,
                                                                  anSetName))
    }


    # Apply Grouping ----------------------------------------------------------------

    AG_temp1 <- AnalysisGroupings %>%
      filter(id == groupid1)

    AG_var1 <- AG_temp1 %>%
      select(groupingVariable) %>%
      unique() %>%
      as.character()

    AG_temp2 <- AnalysisGroupings %>%
      filter(id == groupid2)

    AG_var2 <- AG_temp2 %>%
      select(groupingVariable) %>%
      unique() %>%
      as.character()

    AG_temp3 <- AnalysisGroupings %>%
      filter(id == groupid3)

    AG_var3 <- AG_temp3 %>%
      select(groupingVariable) %>%
      unique() %>%
      as.character()

    #get the number of group_by to perform in Grouping Apply
    if(resultsByGroup1 == TRUE && !is.na(resultsByGroup1)){
      num_grp <- 1
      if(resultsByGroup2 == TRUE && !is.na(resultsByGroup2)) {
        num_grp <- 2
        if(resultsByGroup3 == TRUE && !is.na(resultsByGroup3)) {
          num_grp <- 3
        }
      }
    } else num_grp = 0

    if(num_grp == 1){
      func_AnalysisGrouping <- function(var1, ASID) {

        template <- "

#Apply Analysis Grouping ---
df1_analysisidhere <- df_analysisidhere %>%
          group_by(var)

"
        code <- gsub('var', var1, template)
        code <- gsub('analysisidhere', ASID, code)
      }

      code_AnalysisGrouping_0 <- func_AnalysisGrouping(AG_var1, Anas_j)

    } else if(num_grp == 2){
      func_AnalysisGrouping <- function(var1, var2, ASID) {

        template <- "

#Apply Analysis Grouping ---
df1_analysisidhere <- df_analysisidhere %>%
          group_by(var1, var2)

"

        code <- gsub('var1', var1, template)
        code <- gsub('var2', var2, code)
        code <- gsub('analysisidhere', ASID, code)

        return(code)
      }

      code_AnalysisGrouping_0 <- func_AnalysisGrouping(AG_var1,
                                                       AG_var2,
                                                       Anas_j)
    } else if(num_grp == 3){
      func_AnalysisGrouping <- function(var1, var2, var3, ASID) {

        template <- "

#Apply Analysis Grouping ---
df1_analysisidhere <- df_analysisidhere %>%
          group_by(var1, var2, var3)

"
        code <- gsub('var1', var1, template)
        code <- gsub('var2', var2, code)
        code <- gsub('var3', var3, code)
        code <- gsub('analysisidhere', ASID, code)
        return(code)
      }

      code_AnalysisGrouping_0 <- func_AnalysisGrouping(AG_var1,
                                                       AG_var2,
                                                       AG_var3,
                                                       Anas_j)
    } else {

      func_AnalysisGrouping <- function(ASID) {

        template <- "

#Apply Analysis Grouping ---

# (No grouping applicable for this analysis)
df1_analysisidhere <- df_analysisidhere
"

        code <- gsub('analysisidhere', ASID, template)
        return(code)
      }

      code_AnalysisGrouping_0 <- func_AnalysisGrouping(Anas_j)
    }

    assign(paste0("code_AnalysisGrouping_",
                  Anas_j),
           code_AnalysisGrouping_0)
    # cat(code_AnalysisGrouping_0)

    # Apply DataSubset -------------------------------------------------------------

    if(!is.na(subsetid)){ # if there is a data subset for this analysis
      subsetrule <- DataSubsets %>%
        filter(id == subsetid)

      DSname <- subsetrule %>%
        select(name) %>%
        unique() %>%
        as.character()

      if(nrow(subsetrule) == 1){      # if there's only one row

        #dset?
        var = subsetrule$condition_variable

        if(!is.na(subsetrule$condition_value)){ # check if it's empty / NA
          # val1 = str_trim(subsetrule$condition_value)
          val1 = subsetrule$condition_value
        } else{
          val1 = ""
        }

        vac = subsetrule$condition_comparator

        # R code
        if(vac == "EQ") rvac = '=='
        if(vac == "NE") rvac = '!='
        if(vac == "GT") rvac = '>'
        if(vac == "GE") rvac = '>='
        if(vac == "LT") rvac = '<'
        if(vac == "LE") rvac = '<='
        rFilt_final <- paste0(var," ", rvac," ", "'",val1,"'")

      } else  {                       # if there are more than one rows

        for (m in 1:(max(subsetrule$level) - 1)){   #loop through levels
          # get logical operators

          log_oper = subsetrule %>%  # identify all rows for this level
            filter(level == m,
                   !is.na(compoundExpression_logicalOperator)) %>%
            select(compoundExpression_logicalOperator) %>%
            as.character()

          if(log_oper == "character(0)" ) log_oper = NA
          assign(paste('log_oper',m, sep=''), log_oper) # assign logical operator value
          #R code
          if(!is.na(log_oper)){
            if(log_oper == "AND") rlog_oper = '&'
            else if(log_oper == "OR") rlog_oper = '|'
            else rlog_oper = NA
          }


          lev = subsetrule %>%  # subset containing only first set of equations
            filter(level == m+1,
                   is.na(compoundExpression_logicalOperator))

          rcode <- ""

          for (n in 1:nrow(lev)) {

            ord1_ <- lev[n, ] # one row at a time

            # assign the variables
            var = ord1_$condition_variable
            vac = ord1_$condition_comparator
            if (grepl('|', ord1_$condition_value, fixed = TRUE)) {     # split the condition_value
              # ord1 <- cSplit(ord1_, 'condition_value', sep = "|")
              ord1 <- ord1_ %>%
                separate_wider_delim(condition_value, delim = "|", names_sep = "_")
              # val1 = str_trim(ord1$condition_value_1)
              val1 = ord1$condition_value_1
              # val2 = str_trim(ord1$condition_value_2)
              val2 = ord1$condition_value_2
            } else {
              # val1 = str_trim(ord1_$condition_value)
              val1 = ord1_$condition_value
            }

            # put together in R code
            if(vac == "IN"){
              f_vac = "%in%" # define operator in R code

              # concatenate expression
              assign(paste("fexp", m,n, sep = "_"), paste0(var," ", f_vac," ", "c('",val1, "', '",val2, "')"))

              if(n>1) assign('rcode', paste0(rcode, " LOGOP ",var," ", f_vac," ", "c('",val1, "', '",val2, "')"))
              else assign('rcode', paste0(var," ", f_vac," ", "c('",val1, "', '",val2, "')"))
            } else { # vac is EQ or NE
              if(vac == "EQ") f_vac = "==" # define operator in R code
              else if(vac == "NE") f_vac = "!="

              # concatenate expression
              assign(paste("fexp", m,n, sep = "_"), paste0(var," ", f_vac," ", "'",val1,"'"))

              if(n>1) assign('rcode', paste0(rcode, " LOGOP ",var," ", f_vac," ", "'",val1,"'"))
              else assign('rcode', paste0(var," ", f_vac," ", "'",val1,"'"))

            }



          } # end loop through rows
          # combine total filter


          assign(paste("rFilt", m, sep = "_"),
                 gsub("LOGOP", rlog_oper, rcode))
        } # end loop through levels

        # combine all filter values:
        if(exists('rFilt_2')){

          rFilt_final <- paste(rFilt_1, rFilt_2, sep = ", ")
          rm(rFilt_2) #clear it so it doesn't exist for future
        } else rFilt_final <- rFilt_1
      } # end case where there are more than one rows

      func_DataSubset <- function(filterVal, ASID, DSNAME) {
        template <- "

# Apply Data Subset ---
# Data subset: dsnamehere
df2_analysisidhere <- df1_analysisidhere %>%
        filter(filtertext1)

"
        code <- gsub('filtertext1', filterVal, template)
        code <- gsub('analysisidhere', ASID, code)
        code <- gsub('dsnamehere', DSNAME, code)

        return(code)
      }

      # code_DataSubset <- func_DataSubset(rFilt_final, Anas_j)
      assign(paste0("code_DataSubset_",Anas_j),
             func_DataSubset(rFilt_final,
                             Anas_j,
                             DSname)
      )
      # cat(code_DataSubset)
      # eval(parse(text=code_DataSubset))

    } else { # there is no data subsetting for this analysis

      func_DataSubset <- function(ASID) {
        template <- "

#Apply Data Subset ---
df2_analysisidhere <- df1_analysisidhere

"
        code <- gsub('analysisidhere', ASID, template)
        return(code)
      } # end function

      # code_DataSubset <- func_DataSubset(rFilt_final, Anas_j)
      assign(paste0("code_DataSubset_",Anas_j),
             func_DataSubset(Anas_j)
      )
    } # end case where no data subsetting

    # Apply AnalysisMethod -------------------------------------------------------------
    method <- AnalysisMethods %>%
      filter(id == methodid)

    code_Operation_0 = ""  # initialise code (to be appended)
    code_combine = "" #initialise code to combine datasets

    for(k in 1:nrow(method)){
      # for(k in 1:1){

      operation = method[k,] # one operation at a time
      oper_id <- operation$operation_id #current operation ID
      oper_order <- operation$operation_order #current operation order
      # oper_name <- operation$operation_name #current operation name
      oper_name <- operation$operation_label #current operation name
      oper_desc <- operation$description #current operation description
      oper_pattern <- operation$operation_resultPattern # pattern


      # Mth01_CatVar_Count_ByGrp_1_n ------------------------

      #if(oper_id == "Mth01_CatVar_Count_ByGrp_1_n"){
      if(oper_name == "n_distinct"){

        func_OperationTmp <- function(operid,
                                      operorder,
                                      opername,
                                      operdesc,
                                      analysisid,
                                      methodid,
                                      outputid,
                                      pattern) {
          template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        summarise(res = n()) %>%
        mutate(AnalsysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
          code <- gsub('operationidhere', operid, template)
          code <- gsub('operationnamehere', opername, code)
          code <- gsub('operationdeschere', operdesc, code)
          code <- gsub('operorderhere', operorder, code)
          code <- gsub('analysisidhere', analysisid, code)
          code <- gsub('methodidhere', methodid, code)
          code <- gsub('outputidhere', outputid, code)
          code <- gsub('patternhere', pattern, code)

          return(code)
        }

        code_Operation_tmp = func_OperationTmp(oper_id,
                                               oper_order,
                                               oper_name,
                                               oper_desc,
                                               Anas_j,
                                               methodid,
                                               Output,
                                               oper_pattern)

        #cat(code_Operation_tmp)
        # Mth02_ContVar_Summ_ByGrp_1_n ------------------
        #} else if(operation$operation_id == "Mth02_ContVar_Summ_ByGrp_1_n"){
      } else if(oper_name == "n"){

        func_OperationTmp <- function(operid,
                                      operorder,
                                      opername,
                                      operdesc,
                                      analysisid,
                                      methodid,
                                      outputid,
                                      pattern) {
          template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        summarise(res = n()) %>%
        mutate(AnalsysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
          code <- gsub('operationidhere', operid, template)
          code <- gsub('operationnamehere', opername, code)
          code <- gsub('operationdeschere', operdesc, code)
          code <- gsub('operorderhere', operorder, code)
          code <- gsub('analysisidhere', analysisid, code)
          code <- gsub('methodidhere', methodid, code)
          code <- gsub('outputidhere', outputid, code)
          code <- gsub('patternhere', pattern, code)

          return(code)
        }

        code_Operation_tmp = func_OperationTmp(oper_id,
                                               oper_order,
                                               oper_name,
                                               oper_desc,
                                               Anas_j,
                                               methodid,
                                               Output,
                                               oper_pattern)

        #cat(code_Operation_tmp)
        # Mth02_ContVar_Summ_ByGrp_2_Mean ------------------
        #} else if(operation$operation_id == "Mth02_ContVar_Summ_ByGrp_2_Mean"){
      } else if(oper_name == "Mean"){


        func_OperationTmp <- function(operid,
                                      operorder,
                                      opername,
                                      operdesc,
                                      analysisid,
                                      methodid,
                                      outputid,
                                      analysisvar,
                                      pattern) {
          template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        summarise(res = mean(ana_varhere, na.rm = TRUE)) %>%
        mutate(AnalsysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
          code <- gsub('operationidhere', operid, template)
          code <- gsub('operationnamehere', opername, code)
          code <- gsub('operationdeschere', operdesc, code)
          code <- gsub('operorderhere', operorder, code)
          code <- gsub('analysisidhere', analysisid, code)
          code <- gsub('methodidhere', methodid, code)
          code <- gsub('outputidhere', outputid, code)
          code <- gsub('ana_varhere', analysisvar, code)
          code <- gsub('patternhere', pattern, code)


          return(code)
        }

        code_Operation_tmp = func_OperationTmp(oper_id,
                                               oper_order,
                                               oper_name,
                                               oper_desc,
                                               Anas_j,
                                               methodid,
                                               Output,
                                               ana_var,
                                               oper_pattern)

        #cat(code_Operation_tmp)
        # Mth02_ContVar_Summ_ByGrp_3_SD ------------------
      } else if(oper_name == "SD"){

        func_OperationTmp <- function(operid,
                                      operorder,
                                      opername,
                                      operdesc,
                                      analysisid,
                                      methodid,
                                      outputid,
                                      analysisvar,
                                      pattern) {
          template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        summarise(res = sd(ana_varhere, na.rm = TRUE)) %>%
        mutate(AnalsysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
          code <- gsub('operationidhere', operid, template)
          code <- gsub('operationnamehere', opername, code)
          code <- gsub('operationdeschere', operdesc, code)
          code <- gsub('operorderhere', operorder, code)
          code <- gsub('analysisidhere', analysisid, code)
          code <- gsub('methodidhere', methodid, code)
          code <- gsub('outputidhere', outputid, code)
          code <- gsub('ana_varhere', analysisvar, code)
          code <- gsub('patternhere', pattern, code)

          return(code)
        }

        code_Operation_tmp = func_OperationTmp(oper_id,
                                               oper_order,
                                               oper_name,
                                               oper_desc,
                                               Anas_j,
                                               methodid,
                                               Output,
                                               ana_var,
                                               oper_pattern)

        #cat(code_Operation_tmp)
        # Mth02_ContVar_Summ_ByGrp_4_Median ------------------
      } else if(oper_name == "Median"){

        func_OperationTmp <- function(operid,
                                      operorder,
                                      opername,
                                      operdesc,
                                      analysisid,
                                      methodid,
                                      outputid,
                                      analysisvar,
                                      pattern) {
          template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        summarise(res = median(ana_varhere, na.rm = TRUE)) %>%
        mutate(AnalsysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
          code <- gsub('operationidhere', operid, template)
          code <- gsub('operationnamehere', opername, code)
          code <- gsub('operationdeschere', operdesc, code)
          code <- gsub('operorderhere', operorder, code)
          code <- gsub('analysisidhere', analysisid, code)
          code <- gsub('methodidhere', methodid, code)
          code <- gsub('outputidhere', outputid, code)
          code <- gsub('ana_varhere', analysisvar, code)
          code <- gsub('patternhere', pattern, code)

          return(code)
        }

        code_Operation_tmp = func_OperationTmp(oper_id,
                                               oper_order,
                                               oper_name,
                                               oper_desc,
                                               Anas_j,
                                               methodid,
                                               Output,
                                               ana_var,
                                               oper_pattern)
        #cat(code_Operation_tmp)
        # Mth02_ContVar_Summ_ByGrp_5_Q1 ------------------
      } else if(oper_name == "Q1"){
        func_OperationTmp <- function(operid,
                                      operorder,
                                      opername,
                                      operdesc,
                                      analysisid,
                                      methodid,
                                      outputid,
                                      analysisvar,
                                      pattern) {
          template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        summarise(res = quantile(ana_varhere, c(.25), na.rm = TRUE)) %>%
        mutate(AnalsysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
          code <- gsub('operationidhere', operid, template)
          code <- gsub('operationnamehere', opername, code)
          code <- gsub('operationdeschere', operdesc, code)
          code <- gsub('operorderhere', operorder, code)
          code <- gsub('analysisidhere', analysisid, code)
          code <- gsub('methodidhere', methodid, code)
          code <- gsub('outputidhere', outputid, code)
          code <- gsub('ana_varhere', analysisvar, code)
          code <- gsub('patternhere', pattern, code)

          return(code)
        }

        code_Operation_tmp = func_OperationTmp(oper_id,
                                               oper_order,
                                               oper_name,
                                               oper_desc,
                                               Anas_j,
                                               methodid,
                                               Output,
                                               ana_var,
                                               oper_pattern)
        #cat(code_Operation_tmp)
        # Mth02_ContVar_Summ_ByGrp_6_Q3 ------------------
      } else if(oper_name == "Q3"){
        func_OperationTmp <- function(operid,
                                      operorder,
                                      opername,
                                      operdesc,
                                      analysisid,
                                      methodid,
                                      outputid,
                                      analysisvar,
                                      pattern) {
          template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        summarise(res = quantile(ana_varhere, c(.75), na.rm = TRUE)) %>%
        mutate(AnalsysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
          code <- gsub('operationidhere', operid, template)
          code <- gsub('operationnamehere', opername, code)
          code <- gsub('operationdeschere', operdesc, code)
          code <- gsub('operorderhere', operorder, code)
          code <- gsub('analysisidhere', analysisid, code)
          code <- gsub('methodidhere', methodid, code)
          code <- gsub('outputidhere', outputid, code)
          code <- gsub('ana_varhere', analysisvar, code)
          code <- gsub('patternhere', pattern, code)

          return(code)
        }

        code_Operation_tmp = func_OperationTmp(oper_id,
                                               oper_order,
                                               oper_name,
                                               oper_desc,
                                               Anas_j,
                                               methodid,
                                               Output,
                                               ana_var,
                                               oper_pattern)
        #cat(code_Operation_tmp)
        # Mth02_ContVar_Summ_ByGrp_7_Min ------------------
      } else if(oper_name == "Min"){
        func_OperationTmp <- function(operid,
                                      operorder,
                                      opername,
                                      operdesc,
                                      analysisid,
                                      methodid,
                                      outputid,
                                      analysisvar,
                                      pattern) {
          template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        summarise(res = min(ana_varhere, na.rm = TRUE)) %>%
        mutate(AnalsysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
          code <- gsub('operationidhere', operid, template)
          code <- gsub('operationnamehere', opername, code)
          code <- gsub('operationdeschere', operdesc, code)
          code <- gsub('operorderhere', operorder, code)
          code <- gsub('analysisidhere', analysisid, code)
          code <- gsub('methodidhere', methodid, code)
          code <- gsub('outputidhere', outputid, code)
          code <- gsub('ana_varhere', analysisvar, code)
          code <- gsub('patternhere', pattern, code)


          return(code)
        }

        code_Operation_tmp = func_OperationTmp(oper_id,
                                               oper_order,
                                               oper_name,
                                               oper_desc,
                                               Anas_j,
                                               methodid,
                                               Output,
                                               ana_var,
                                               oper_pattern)
        # #cat(code_Operation_tmp)
        # Mth02_ContVar_Summ_ByGrp_8_Max ------------------

      } else if(oper_name == "Max"){
        func_OperationTmp <- function(operid,
                                      operorder,
                                      opername,
                                      operdesc,
                                      analysisid,
                                      methodid,
                                      outputid,
                                      analysisvar,
                                      pattern) {
          template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        summarise(res = max(ana_varhere, na.rm = TRUE)) %>%
        mutate(AnalsysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
          code <- gsub('operationidhere', operid, template)
          code <- gsub('operationnamehere', opername, code)
          code <- gsub('operationdeschere', operdesc, code)
          code <- gsub('operorderhere', operorder, code)
          code <- gsub('analysisidhere', analysisid, code)
          code <- gsub('methodidhere', methodid, code)
          code <- gsub('outputidhere', outputid, code)
          code <- gsub('ana_varhere', analysisvar, code)
          code <- gsub('patternhere', pattern, code)


          return(code)
        }

        code_Operation_tmp = func_OperationTmp(oper_id,
                                               oper_order,
                                               oper_name,
                                               oper_desc,
                                               Anas_j,
                                               methodid,
                                               Output,
                                               ana_var,
                                               oper_pattern)
        # #cat(code_Operation_tmp)
        # Mth01_CatVar_Summ_ByGrp_1_n ------------------

      } else if(operation$operation_id == "Mth01_CatVar_Summ_ByGrp_1_n"){
        func_OperationTmp <- function(operid,
                                      operorder,
                                      opername,
                                      operdesc,
                                      analysisid,
                                      methodid,
                                      outputid,
                                      analysisvar,
                                      pattern) {
          template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        summarise(res = n_distinct(ana_varhere)) %>%
        mutate(AnalsysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
          code <- gsub('operationidhere', operid, template)
          code <- gsub('operationnamehere', opername, code)
          code <- gsub('operationdeschere', operdesc, code)
          code <- gsub('operorderhere', operorder, code)
          code <- gsub('analysisidhere', analysisid, code)
          code <- gsub('methodidhere', methodid, code)
          code <- gsub('outputidhere', outputid, code)
          code <- gsub('ana_varhere', analysisvar, code)
          code <- gsub('patternhere', pattern, code)

          return(code)
        }

        code_Operation_tmp = func_OperationTmp(oper_id,
                                               oper_order,
                                               oper_name,
                                               oper_desc,
                                               Anas_j,
                                               methodid,
                                               Output,
                                               ana_var,
                                               oper_pattern)
        # Mth01_CatVar_Summ_ByGrp_2_pct ------------------

      } else if(oper_name == "%"){

        # change operation ID from % to pct
        oper_id <- gsub("%", "pct", oper_id)


        NUM_analysisid = Anas_s$referencedAnalysisOperations_analysisId1
        DEN_analysisid = Anas_s$referencedAnalysisOperations_analysisId2

        #mergevar
        merge_grp <- Analyses %>%
          filter(id == DEN_analysisid) %>%
          select(groupingId1) %>%
          as.character()

        if(is.na(merge_grp)){
          merge_var = ""
        } else{
          merge_var = AnalysisGroupings %>%
            filter(id == merge_grp) %>%
            select(groupingVariable) %>%
            unique() %>%
            as.character()
        }

        NUM_operationid = operation$operation_referencedResultRelationships1_operationId
        DEN_operationid = operation$operation_referencedResultRelationships2_operationId

        if(merge_var != ""){ # case where merge_var != ""

          func_OperationTmp <- function(operid,
                                        operorder,
                                        opername,
                                        operdesc,
                                        analysisid,
                                        methodid,
                                        outputid,
                                        analysisvar,
                                        num_analysisid,
                                        den_analysisid,
                                        num_operationid,
                                        den_operationid,
                                        pattern,
                                        #groupvar1
                                        mergevar
          ) {
            template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere_num <- df3_num_analysisIDhere_num_operationIDhere %>%
          rename(NUM = res)

df3_analysisidhere_operationidhere_den <- df3_den_analysisIDhere_den_operationIDhere %>%
          rename(DEN = res)

df3_analysisidhere_operationidhere <- merge(df3_analysisidhere_operationidhere_num,
                                            df3_analysisidhere_operationidhere_den %>%
                                                  select(mergevarhere, DEN),
                                            by = c('mergevarhere')) %>%
                                            mutate(res = NUM / DEN * 100,
                                                   OperationId = 'operationidhere',
                                                   pattern = 'patternhere') %>%
                                            select(-NUM, -DEN)%>%
                                            mutate(across(starts_with('Group'), as.character))

"
            code <- gsub('operationidhere', operid, template)
            code <- gsub('operationnamehere', opername, code)
            code <- gsub('operationdeschere', operdesc, code)
            code <- gsub('operorderhere', operorder, code)
            code <- gsub('analysisidhere', analysisid, code)
            code <- gsub('methodidhere', methodid, code)
            code <- gsub('outputidhere', outputid, code)
            code <- gsub('num_analysisIDhere', num_analysisid, code)
            code <- gsub('den_analysisIDhere', den_analysisid, code)
            code <- gsub('num_operationIDhere', num_operationid, code)
            code <- gsub('den_operationIDhere', den_operationid, code)
            code <- gsub('patternhere', pattern, code)
            code <- gsub('mergevarhere', mergevar, code)

            return(code)
          }
          code_Operation_tmp = func_OperationTmp(oper_id,
                                                 oper_order,
                                                 oper_name,
                                                 oper_desc,
                                                 Anas_j,
                                                 methodid,
                                                 Output,
                                                 ana_var,
                                                 NUM_analysisid,
                                                 DEN_analysisid,
                                                 NUM_operationid,
                                                 DEN_operationid,
                                                 oper_pattern,
                                                 merge_var)
        } #end case where merge_var != ""

        else { # case where merge_var == ""

          func_OperationTmp <- function(operid,
                                        operorder,
                                        opername,
                                        operdesc,
                                        analysisid,
                                        methodid,
                                        outputid,
                                        analysisvar,
                                        num_analysisid,
                                        den_analysisid,
                                        num_operationid,
                                        den_operationid,
                                        pattern
          ) {
            template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere_num <- df3_num_analysisIDhere_num_operationIDhere %>%
          rename(NUM = res)

df3_analysisidhere_operationidhere_den <- df3_den_analysisIDhere_den_operationIDhere %>%
          rename(DEN = res)

df3_analysisidhere_operationidhere <- cbind(df3_analysisidhere_operationidhere_num,
                                            df3_analysisidhere_operationidhere_den %>%
                                                   select(DEN)) %>%
                                            mutate(res = NUM / DEN * 100,
                                                   OperationId = 'operationidhere',
                                                   pattern = 'patternhere') %>%
                                            select(-NUM, -DEN)%>%
                                            mutate(across(starts_with('Group'), as.character))

"
            code <- gsub('operationidhere', operid, template)
            code <- gsub('operationnamehere', opername, code)
            code <- gsub('operationdeschere', operdesc, code)
            code <- gsub('operorderhere', operorder, code)
            code <- gsub('analysisidhere', analysisid, code)
            code <- gsub('methodidhere', methodid, code)
            code <- gsub('outputidhere', outputid, code)
            code <- gsub('num_analysisIDhere', num_analysisid, code)
            code <- gsub('den_analysisIDhere', den_analysisid, code)
            code <- gsub('num_operationIDhere', num_operationid, code)
            code <- gsub('den_operationIDhere', den_operationid, code)
            code <- gsub('patternhere', pattern, code)

            return(code)
          }
          code_Operation_tmp = func_OperationTmp(oper_id,
                                                 oper_order,
                                                 oper_name,
                                                 oper_desc,
                                                 Anas_j,
                                                 methodid,
                                                 Output,
                                                 ana_var,
                                                 NUM_analysisid,
                                                 DEN_analysisid,
                                                 NUM_operationid,
                                                 DEN_operationid,
                                                 oper_pattern)




        } # end case where merge_var == ""

        # Method: RISK DIFFERENCE ----
      } else if(oper_name == "Risk Difference %"){


        tab <- table(ana_adam[, c('ana_groupvar1here', 'ana_groupvar2here')])


        #         func_OperationTmp <- function(operid,
        #                                       operorder,
        #                                       opername,
        #                                       operdesc,
        #                                       analysisid,
        #                                       methodid,
        #                                       outputid,
        #                                       analysisvar,
        #                                       AGvar,
        #                                       pattern) {
        #           template <- "
        # # Operation ID:           operationidhere
        # # Operation name:         operationnamehere
        # # Operation description:  operationdeschere
        #
        # fm <- as.formula(paste('ana_varhere', '~', 'ana_groupvarhere'))
        #   model <- lm(fm, data = df2_analysisidhere
        #   )
        #
        # if (class(model) != 'lm') stop('Not an object of class lm ')
        # f <- summary(model)$fstatistic
        # p <- pf(f[1],f[2],f[3],lower.tail=F)
        # attributes(p) <- NULL
        #
        # df3_analysisidhere_operationidhere <- data.frame(res = p,
        #                   AnalsysisId = 'analysisidhere',
        #                   MethodId = 'methodidhere',
        #                   OperationId = 'operationidhere',
        #                   OutputId = 'outputidhere',
        #                   pattern = 'patternhere')%>%
        #         mutate(across(starts_with('Group'), as.character))
        # "
        #           code <- gsub('operationidhere', operid, template)
        #           code <- gsub('operationnamehere', opername, code)
        #           code <- gsub('operationdeschere', operdesc, code)
        #           code <- gsub('operorderhere', operorder, code)
        #           code <- gsub('analysisidhere', analysisid, code)
        #           code <- gsub('methodidhere', methodid, code)
        #           code <- gsub('outputidhere', outputid, code)
        #           code <- gsub('ana_varhere', analysisvar, code)
        #           code <- gsub('ana_groupvarhere', AGvar, code)
        #           code <- gsub('patternhere', pattern, code)
        #
        #           return(code)
        #         }
        #
        #         code_Operation_tmp = func_OperationTmp(oper_id,
        #                                                oper_order,
        #                                                oper_name,
        #                                                oper_desc,
        #                                                Anas_j,
        #                                                methodid,
        #                                                Output,
        #                                                ana_var,
        #                                                AG_var1,
        #                                                oper_pattern)


        # Mth04_ContVar_Comp_Anova_1_pval ----
      } else if(operation$operation_id == "Mth04_ContVar_Comp_Anova_1_pval"){

        func_OperationTmp <- function(operid,
                                      operorder,
                                      opername,
                                      operdesc,
                                      analysisid,
                                      methodid,
                                      outputid,
                                      analysisvar,
                                      AGvar,
                                      pattern) {
          template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere


fm <- as.formula(paste('ana_varhere', '~', 'ana_groupvarhere'))
  model <- lm(fm, data = df2_analysisidhere
  )

if (class(model) != 'lm') stop('Not an object of class lm ')
f <- summary(model)$fstatistic
p <- pf(f[1],f[2],f[3],lower.tail=F)
attributes(p) <- NULL

df3_analysisidhere_operationidhere <- data.frame(res = p,
                  AnalsysisId = 'analysisidhere',
                  MethodId = 'methodidhere',
                  OperationId = 'operationidhere',
                  OutputId = 'outputidhere',
                  pattern = 'patternhere')%>%
        mutate(across(starts_with('Group'), as.character))
"
          code <- gsub('operationidhere', operid, template)
          code <- gsub('operationnamehere', opername, code)
          code <- gsub('operationdeschere', operdesc, code)
          code <- gsub('operorderhere', operorder, code)
          code <- gsub('analysisidhere', analysisid, code)
          code <- gsub('methodidhere', methodid, code)
          code <- gsub('outputidhere', outputid, code)
          code <- gsub('ana_varhere', analysisvar, code)
          code <- gsub('ana_groupvarhere', AGvar, code)
          code <- gsub('patternhere', pattern, code)

          return(code)
        }

        code_Operation_tmp = func_OperationTmp(oper_id,
                                               oper_order,
                                               oper_name,
                                               oper_desc,
                                               Anas_j,
                                               methodid,
                                               Output,
                                               ana_var,
                                               AG_var1,
                                               oper_pattern)
        # Mth03_CatVar_Comp_PChiSq_1_pval ----
      } else if(operation$operation_id == "Mth03_CatVar_Comp_PChiSq_1_pval"){

        func_OperationTmp <- function(operid,
                                      operorder,
                                      opername,
                                      operdesc,
                                      analysisid,
                                      methodid,
                                      outputid,
                                      analysisvar,
                                      AGvar1,
                                      AGvar2,
                                      ana_adam,
                                      pattern) {
          template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

tab <- table(adamhere[, c('ana_groupvar1here', 'ana_groupvar2here')])
p <- chisq.test(tab)$p.value

df3_analysisidhere_operationidhere <- data.frame(res = p,
                  AnalsysisId = 'analysisidhere',
                  MethodId = 'methodidhere',
                  OperationId = 'operationidhere',
                  OutputId = 'outputidhere',
                  pattern = 'patternhere')%>%
        mutate(across(starts_with('Group'), as.character))

"
          code <- gsub('operationidhere', operid, template)
          code <- gsub('operationnamehere', opername, code)
          code <- gsub('operationdeschere', operdesc, code)
          code <- gsub('operorderhere', operorder, code)
          code <- gsub('analysisidhere', analysisid, code)
          code <- gsub('methodidhere', methodid, code)
          code <- gsub('outputidhere', outputid, code)
          code <- gsub('ana_varhere', analysisvar, code)
          code <- gsub('ana_groupvar1here', AGvar1, code)
          code <- gsub('ana_groupvar2here', AGvar2, code)
          code <- gsub('adamhere', ana_adam, code)
          code <- gsub('patternhere', pattern, code)

          return(code)
        }

        code_Operation_tmp = func_OperationTmp(oper_id,
                                               oper_order,
                                               oper_name,
                                               oper_desc,
                                               Anas_j,
                                               methodid,
                                               Output,
                                               ana_var,
                                               AG_var1,
                                               AG_var2,
                                               ana_adam,
                                               oper_pattern)

      } # operation loop ends

      code_Operation_0 = paste(code_Operation_0, # code contai
                               code_Operation_tmp)

      if(k<nrow(method)) {
        code_combine = paste0(code_combine,
                              "df3_",Anas_j, "_",oper_id, ", \n")
      } else {
        code_combine = paste0(code_combine,
                              "df3_",Anas_j, "_",oper_id)
      }
    } # all operations end

    # code to combine it all --------------------------------------------------
    # rename groups to append
    if(num_grp == 1){ # if 1 analysis grouping
      func_rename1 <- function(groupvar1) {
        template <- " %>%
      rename(Group1 = groupvar1here) %>%
  mutate(Group1 = as.character(Group1)) %>%
  mutate(across(starts_with('Group'), as.character))
"
        code <- gsub('groupvar1here', groupvar1, template)

        return(code)
      }

      code_rename = func_rename1(AG_var1)

    }
    else if(num_grp == 2){ # if 2 analysis groupings
      func_rename2 <- function(groupvar1,
                               groupvar2) {
        template <- " %>%
      rename(Group1 = groupvar1here,
             Group2 = groupvar2here) %>%
  mutate(Group1 = as.character(Group1)) %>%
  mutate(across(starts_with('Group'), as.character))
"
        code <- gsub('groupvar1here', groupvar1, template)
        code <- gsub('groupvar2here', groupvar2, code)

        return(code)
      }
      code_rename = func_rename2(AG_var1,
                                 AG_var2)

    }
    else if(num_grp == 3){ # if 3 analysis groupings
      func_rename3 <- function(groupvar1,
                               groupvar2,
                               groupvar3) {
        template <- " %>%
      rename(Group1 = groupvar1here,
             Group2 = groupvar2here,
             Group3 = groupvar3here) %>%
  mutate(Group1 = as.character(Group1)) %>%
  mutate(across(starts_with('Group'), as.character))
"
        code <- gsub('groupvar1here', groupvar1, template)
        code <- gsub('groupvar2here', groupvar2, code)
        code <- gsub('groupvar3here', groupvar3, code)

        return(code)
      }

      code_rename = func_rename3(AG_var1,
                                 AG_var2,
                                 AG_var3)
    } else code_rename = "" # if no analysis grouping


    assign(paste0("code_Operation_", Anas_j),
           paste0("#Apply Operations --- \n",
                  code_Operation_0,
                  "#Combine operation datasets: \n",
                  "df3_",Anas_j," <- bind_rows(",
                  code_combine,
                  ")",
                  code_rename))

    # Generate code for analysis ----------------------------------------------

    assign(paste0("code_",Anas_j),
           paste0("\n\n# Analysis ", Anas_j,"----",
                  get(paste0("code_AnalysisSet_",Anas_j)),
                  get(paste0("code_AnalysisGrouping_",Anas_j)),
                  get(paste0("code_DataSubset_",Anas_j)),
                  get(paste0("code_Operation_",Anas_j))))

    run_code <- paste0(run_code,
                       get(paste0("code_",Anas_j)))

    if(j<max_j) {
      combine_analysis_code = paste0(combine_analysis_code,
                                     "df3_",Anas_j, ", \n")
    } else {
      combine_analysis_code = paste0(combine_analysis_code,
                                     "df3_",Anas_j)
    }

  } # end of analysis

  # add pattern formatting


  code_pattern <- paste0('ARD_',
                         gsub('-', '_', Output),
                         "<- df4 %>%
      mutate(dec = ifelse(grepl('X.X',
                                df4$pattern, ),
                          str_count(substr(df4$pattern,
                                          str_locate(df4$pattern,
                                                    'X.X')[, 1]+2,
                                          nchar(df4$pattern)), 'X'),
                          0)) %>%
      rowwise() %>%
      mutate(rnd = round(res, dec)) %>%
      as_tibble() %>%
      mutate(disp = ifelse(grepl('\\\\(N=', df4$pattern),
                           paste0('(N=', rnd, ')'),
                           ifelse(grepl('\\\\(', df4$pattern),
                                  paste0('(', rnd, ')'),
                                  as.character(rnd)))) %>%
                         select(-rnd, -dec)")


  # add all code, combine analyses ARDs and apply pattern
  assign(paste0("code_",Output),
         paste0(code_header,
                code_libraries,
                code_ADaM,
                run_code,
                "\n\n# combine analyses to create ARD ----\n",
                "df4 <- bind_rows(",
                combine_analysis_code,
                ")\n\n #Apply pattern format:\n",
                code_pattern)
  )


  writeLines(get(paste0("code_",Output)),
             paste0("inst","/ARD_",Output,".R"))


  # writeLines(get(paste0("code_",Output)),
  #            paste0("ARD_",Output,".R"))
  #
  # file.edit(paste0("ARD_",Output,".R"))

} # end of outputs

#} # end of function

