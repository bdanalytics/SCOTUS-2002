# SCOTUS-Stevens: Decision.fctr classification: caret Random Forest
bdanalytics  

**  **    
**Date: (Sun) Apr 05, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/c4x/MITx/15.071x_2/asset/stevens.csv  
    New:        <newdt_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/mydsutils.R")
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
# Gather all package requirements here
#suppressPackageStartupMessages(require())

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_is_separate_newent_dataset <- FALSE    # or TRUE
glb_split_entity_newent_datasets <- TRUE   # or FALSE
glb_split_newdata_method <- "sample"          # "condition" or "sample"
glb_split_newdata_condition <- "<col_name> <condition_operator> <value>"    # or NULL
glb_split_newdata_size_ratio <- 0.3               # > 0 & < 1
glb_split_sample.seed <- 3000               # or any integer 

glb_is_regression <- FALSE; glb_is_classification <- TRUE

glb_rsp_var_raw <- "Reverse"

# for classification, the response variable has to be a factor
#   especially for random forests (method="rf")
glb_rsp_var <- "Decision.fctr"

# if the response factor is based on numbers e.g (0/1 vs. "A"/"B"), 
#   caret predict(..., type="prob") crashes
glb_map_rsp_raw_to_var <- function(raw) 
    relevel(factor(ifelse(raw == 1, "R", "A")), as.factor(c("R", "A")), ref="A")

glb_map_rsp_var_to_raw <- function(var) 
    as.numeric(var) - 1

if ((glb_rsp_var != glb_rsp_var_raw) & is.null(glb_map_rsp_raw_to_var))
    stop("glb_map_rsp_raw_to_var function expected")

glb_rsp_var_out <- paste0(glb_rsp_var, ".prediction")
glb_id_vars <- c("Docket")                # or NULL

glb_exclude_vars_as_features <- union(glb_id_vars, ".rnorm")     # or NULL                      
# List transformed vars  
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                        c("Reverse", "Reverse.fctr"))
# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(union(glb_exclude_vars_as_features, 
                        paste(glb_rsp_var_out, c("", ".proba"), sep="")),
                                    c("Docket.fctr", "Term")) # or "<col_name>"

glb_impute_na_data <- FALSE            # or TRUE
glb_mice_complete.seed <- 144               # or any integer

glb_sel_mdl <- glb_dmy_mdl <- NULL;
glb_models_method_vctr <- c("glm", "rpart", "rf")
glb_models_lst <- list(); glb_models_df <- data.frame()
glb_tune_models_df <- rbind(
    data.frame(parameter="cp", min=0.01, max=0.5, by=0.01), 
    data.frame(parameter="mtry", min=2, max=4, by=1)) # or NULL

glb_clf_proba_threshold <- 0.5 # NULL

# Depict process
glb_analytics_pn <- petrinet(name="glb_analytics_pn",
                        trans_df=data.frame(id=1:6,
    name=c("data.training.all","data.new",
           "model.selected","model.final",
           "data.training.all.prediction","data.new.prediction"),
    x=c(   -5,-5,-15,-25,-25,-35),
    y=c(   -5, 5,  0,  0, -5,  5)
                        ),
                        places_df=data.frame(id=1:4,
    name=c("bgn","fit.data.training.all","predict.data.new","end"),
    x=c(   -0,   -20,                    -30,               -40),
    y=c(    0,     0,                      0,                 0),
    M0=c(   3,     0,                      0,                 0)
                        ),
                        arcs_df=data.frame(
    begin=c("bgn","bgn","bgn",        
            "data.training.all","model.selected","fit.data.training.all",
            "fit.data.training.all","model.final",    
            "data.new","predict.data.new",
            "data.training.all.prediction","data.new.prediction"),
    end  =c("data.training.all","data.new","model.selected",
            "fit.data.training.all","fit.data.training.all","model.final",
            "data.training.all.prediction","predict.data.new",
            "predict.data.new","data.new.prediction",
            "end","end")
                        ))
#print(ggplot.petrinet(glb_analytics_pn))
print(ggplot.petrinet(glb_analytics_pn) + coord_flip())
```

```
## Loading required package: grid
```

![](stevens_caret_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_script_tm <- proc.time()
glb_script_df <- data.frame(chunk_label="import_data", 
                            chunk_step_major=1, chunk_step_minor=0,
                            elapsed=(proc.time() - glb_script_tm)["elapsed"])
print(tail(glb_script_df, 2))
```

```
##         chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed import_data                1                0   0.002
```

## Step `1`: import data

```r
glb_entity_df <- myimport_data(
    url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/stevens.csv", 
    comment="glb_entity_df", force_header=TRUE,
    print_diagn=(glb_is_separate_newent_dataset | 
                !glb_split_entity_newent_datasets))
```

```
## [1] "Reading file ./data/stevens.csv..."
## [1] "dimensions of data in ./data/stevens.csv: 566 rows x 9 cols"
```

```r
if (glb_is_separate_newent_dataset) {
    glb_newent_df <- myimport_data(
        url="<newdt_url>", 
        comment="glb_newent_df", force_header=TRUE, print_diagn=TRUE)
} else {
    if (!glb_split_entity_newent_datasets) {
        stop("Not implemented yet") 
        glb_newent_df <- glb_entity_df[sample(1:nrow(glb_entity_df),
                                          max(2, nrow(glb_entity_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newent_df <- do.call("subset", 
                list(glb_entity_df, parse(text=glb_split_newdata_condition)))
            glb_entity_df <- do.call("subset", 
                list(glb_entity_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_entity_df[, "Reverse"], 
                                      SplitRatio=(1-glb_split_newdata_size_ratio))
                glb_newent_df <- glb_entity_df[!split, ] 
                glb_entity_df <- glb_entity_df[split ,]
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample')")   

    comment(glb_newent_df) <- "glb_newent_df"
    myprint_df(glb_newent_df)
    str(glb_newent_df)

    if (glb_split_entity_newent_datasets) {
        myprint_df(glb_entity_df)
        str(glb_entity_df)        
    }
}         
```

```
## Loading required package: caTools
```

```
##     Docket Term Circuit            Issue     Petitioner Respondent
## 1  93-1408 1994     2nd EconomicActivity       BUSINESS   BUSINESS
## 3  93-1612 1994     5th EconomicActivity       BUSINESS   BUSINESS
## 4   94-623 1994     1st EconomicActivity       BUSINESS   BUSINESS
## 6   95-129 1995     9th EconomicActivity       BUSINESS   BUSINESS
## 8  96-1768 1997     9th EconomicActivity       BUSINESS   BUSINESS
## 21 97-1704 1998     5th    JudicialPower INJURED.PERSON   BUSINESS
##    LowerCourt Unconst Reverse
## 1     liberal       0       1
## 3     liberal       0       1
## 4      conser       0       1
## 6      conser       1       0
## 8      conser       1       1
## 21    liberal       1       0
##      Docket Term Circuit             Issue         Petitioner Respondent
## 187  99-536 1999     5th       CivilRights           EMPLOYEE   EMPLOYER
## 188 00-6029 2001     8th            Unions           EMPLOYEE   EMPLOYER
## 286 99-1030 2000     7th CriminalProcedure               CITY      OTHER
## 359  97-371 1997     9th    FirstAmendment              OTHER      OTHER
## 452 95-6510 1995     4th        DueProcess CRIMINAL.DEFENDENT      STATE
## 484 99-9136 2000     9th CriminalProcedure CRIMINAL.DEFENDENT      STATE
##     LowerCourt Unconst Reverse
## 187     conser       0       1
## 188    liberal       0       0
## 286    liberal       0       0
## 359    liberal       0       1
## 452    liberal       1       0
## 484     conser       1       1
##      Docket Term Circuit             Issue          Petitioner Respondent
## 545  00-507 2001    10th       CivilRights     AMERICAN.INDIAN         US
## 546 95-1478 1996     9th CriminalProcedure GOVERNMENT.OFFICIAL         US
## 551  00-157 2000     4th   FederalTaxation GOVERNMENT.OFFICIAL         US
## 552 00-1567 2001     1st  EconomicActivity GOVERNMENT.OFFICIAL         US
## 556  95-173 1995     9th  EconomicActivity               OTHER         US
## 558 96-6839 1997     5th CriminalProcedure               OTHER         US
##     LowerCourt Unconst Reverse
## 545     conser       0       0
## 546    liberal       1       0
## 551    liberal       0       0
## 552    liberal       0       0
## 556     conser       1       1
## 558     conser       1       1
## 'data.frame':	170 obs. of  9 variables:
##  $ Docket    : chr  "93-1408" "93-1612" "94-623" "95-129" ...
##  $ Term      : int  1994 1994 1994 1995 1997 1998 1994 1995 1996 1996 ...
##  $ Circuit   : chr  "2nd" "5th" "1st" "9th" ...
##  $ Issue     : chr  "EconomicActivity" "EconomicActivity" "EconomicActivity" "EconomicActivity" ...
##  $ Petitioner: chr  "BUSINESS" "BUSINESS" "BUSINESS" "BUSINESS" ...
##  $ Respondent: chr  "BUSINESS" "BUSINESS" "BUSINESS" "BUSINESS" ...
##  $ LowerCourt: chr  "liberal" "liberal" "conser" "conser" ...
##  $ Unconst   : int  0 0 0 1 1 1 0 0 1 0 ...
##  $ Reverse   : int  1 1 1 0 1 0 0 0 0 1 ...
##  - attr(*, "comment")= chr "glb_newent_df"
##     Docket Term Circuit             Issue Petitioner Respondent LowerCourt
## 2  93-1577 1994     9th  EconomicActivity   BUSINESS   BUSINESS    liberal
## 5  94-1175 1995     7th     JudicialPower   BUSINESS   BUSINESS     conser
## 7   95-728 1996     FED  EconomicActivity   BUSINESS   BUSINESS     conser
## 9   96-843 1997      DC  EconomicActivity   BUSINESS   BUSINESS     conser
## 10 98-1480 1999    11th CriminalProcedure   BUSINESS   BUSINESS     conser
## 11  99-150 1999     2nd  EconomicActivity   BUSINESS   BUSINESS     conser
##    Unconst Reverse
## 2        0       1
## 5        0       1
## 7        0       1
## 9        0       1
## 10       0       1
## 11       0       1
##      Docket Term Circuit             Issue      Petitioner
## 20  95-1872 1996     8th       CivilRights AMERICAN.INDIAN
## 49  96-1971 1997     5th CriminalProcedure           OTHER
## 52   97-704 1997      DC     JudicialPower           OTHER
## 73  94-1893 1995     4th    FirstAmendment              US
## 232  94-500 1994     5th   FederalTaxation           OTHER
## 401  95-489 1995    10th    FirstAmendment      POLITICIAN
##              Respondent LowerCourt Unconst Reverse
## 20             BUSINESS     conser       0       0
## 49             BUSINESS    liberal       1       1
## 52             BUSINESS     conser       0       0
## 73             BUSINESS    liberal       0       1
## 232 GOVERNMENT.OFFICIAL     conser       0       1
## 401               OTHER     conser       1       0
##      Docket Term Circuit             Issue Petitioner Respondent
## 561 98-1828 1999     2nd CriminalProcedure      OTHER         US
## 562 99-5153 1999     6th CriminalProcedure      OTHER         US
## 563  99-804 2000     5th CriminalProcedure      OTHER         US
## 564 99-8508 2000     9th CriminalProcedure      OTHER         US
## 565   97-29 1997      DC       CivilRights      STATE         US
## 566  00-189 2000     9th       CivilRights      STATE         US
##     LowerCourt Unconst Reverse
## 561     conser       1       1
## 562     conser       0       0
## 563    liberal       0       1
## 564     conser       1       0
## 565     conser       0       0
## 566    liberal       0       0
## 'data.frame':	396 obs. of  9 variables:
##  $ Docket    : chr  "93-1577" "94-1175" "95-728" "96-843" ...
##  $ Term      : int  1994 1995 1996 1997 1999 1999 2000 2000 1994 1999 ...
##  $ Circuit   : chr  "9th" "7th" "FED" "DC" ...
##  $ Issue     : chr  "EconomicActivity" "JudicialPower" "EconomicActivity" "EconomicActivity" ...
##  $ Petitioner: chr  "BUSINESS" "BUSINESS" "BUSINESS" "BUSINESS" ...
##  $ Respondent: chr  "BUSINESS" "BUSINESS" "BUSINESS" "BUSINESS" ...
##  $ LowerCourt: chr  "liberal" "conser" "conser" "conser" ...
##  $ Unconst   : int  0 0 0 0 0 0 1 1 0 0 ...
##  $ Reverse   : int  1 1 1 1 1 1 1 1 0 0 ...
##  - attr(*, "comment")= chr "glb_entity_df"
```

```r
glb_script_df <- rbind(glb_script_df,
                   data.frame(chunk_label="cleanse_data", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##           chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed   import_data                1                0   0.002
## elapsed1 cleanse_data                2                0   0.675
```

## Step `2`: cleanse data

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="inspectORexplore.data", 
                              chunk_step_major=max(glb_script_df$chunk_step_major), 
                              chunk_step_minor=1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                    chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed1          cleanse_data                2                0   0.675
## elapsed2 inspectORexplore.data                2                1   0.706
```

### Step `2`.`1`: inspect/explore data

```r
#print(str(glb_entity_df))
#View(glb_entity_df)

# List info gathered for various columns
# <col_name>:   <description>; <notes>

# Create new features that help diagnostics
#   Create factors of string variables
str_vars <- sapply(1:length(names(glb_entity_df)), 
    function(col) ifelse(class(glb_entity_df[, names(glb_entity_df)[col]]) == "character",
                         names(glb_entity_df)[col], ""))
str_vars <- setdiff(str_vars[str_vars != ""], glb_exclude_vars_as_features)
warning("Creating factors of string variables:", paste0(str_vars, collapse=", "))
```

```
## Warning: Creating factors of string variables:Circuit, Issue, Petitioner,
## Respondent, LowerCourt
```

```r
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, str_vars)
for (var in str_vars) {
    glb_entity_df[, paste0(var, ".fctr")] <- factor(glb_entity_df[, var], 
                    as.factor(union(glb_entity_df[, var], glb_newent_df[, var])))
    glb_newent_df[, paste0(var, ".fctr")] <- factor(glb_newent_df[, var], 
                    as.factor(union(glb_entity_df[, var], glb_newent_df[, var])))
}

#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

add_new_diag_feats <- function(obs_df, obs_twin_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<ref_val>"), 
#         <col2_name>.fctr=relevel(factor(ifelse(<col1_name> == <val>, "<oth_val>", "<ref_val>")), 
#                               as.factor(c("R", "<ref_val>")),
#                               ref="<ref_val>"),

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>.log=log(<col.name>),        
#         <col_name>=<table>[as.character(<col2_name>)],
#         <col_name>=as.numeric(<col2_name>),

        .rnorm=rnorm(n=nrow(obs_df))
                        )

    # If levels of a factor are different across obs_df & glb_newent_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    print(summary(obs_df))
    print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}

glb_entity_df <- add_new_diag_feats(glb_entity_df, glb_newent_df)
```

```
## Loading required package: plyr
```

```
##     Docket               Term        Circuit             Issue          
##  Length:396         Min.   :1994   Length:396         Length:396        
##  Class :character   1st Qu.:1995   Class :character   Class :character  
##  Mode  :character   Median :1997   Mode  :character   Mode  :character  
##                     Mean   :1997                                        
##                     3rd Qu.:1999                                        
##                     Max.   :2001                                        
##                                                                         
##   Petitioner         Respondent         LowerCourt           Unconst      
##  Length:396         Length:396         Length:396         Min.   :0.0000  
##  Class :character   Class :character   Class :character   1st Qu.:0.0000  
##  Mode  :character   Mode  :character   Mode  :character   Median :0.0000  
##                                                           Mean   :0.2247  
##                                                           3rd Qu.:0.0000  
##                                                           Max.   :1.0000  
##                                                                           
##     Reverse        Circuit.fctr             Issue.fctr
##  Min.   :0.0000   9th    : 79   CriminalProcedure:92  
##  1st Qu.:0.0000   11th   : 41   JudicialPower    :78  
##  Median :1.0000   5th    : 40   EconomicActivity :58  
##  Mean   :0.5455   4th    : 33   CivilRights      :55  
##  3rd Qu.:1.0000   8th    : 32   DueProcess       :36  
##  Max.   :1.0000   7th    : 31   FirstAmendment   :25  
##                   (Other):140   (Other)          :52  
##             Petitioner.fctr           Respondent.fctr LowerCourt.fctr
##  OTHER              :123    OTHER             :120    liberal:185    
##  CRIMINAL.DEFENDENT : 58    BUSINESS          : 58    conser :211    
##  BUSINESS           : 53    US                : 52                   
##  STATE              : 38    CRIMINAL.DEFENDENT: 45                   
##  US                 : 34    STATE             : 35                   
##  GOVERNMENT.OFFICIAL: 25    EMPLOYEE          : 18                   
##  (Other)            : 65    (Other)           : 68                   
##      .rnorm        
##  Min.   :-2.74448  
##  1st Qu.:-0.58477  
##  Median :-0.04916  
##  Mean   : 0.07455  
##  3rd Qu.: 0.80318  
##  Max.   : 3.06873  
##                    
##          Docket            Term         Circuit           Issue 
##               0               0               0               0 
##      Petitioner      Respondent      LowerCourt         Unconst 
##               0               0               0               0 
##         Reverse    Circuit.fctr      Issue.fctr Petitioner.fctr 
##               0               0               0               0 
## Respondent.fctr LowerCourt.fctr          .rnorm 
##               0               0               0
```

```r
glb_newent_df <- add_new_diag_feats(glb_newent_df, glb_entity_df)
```

```
##     Docket               Term        Circuit             Issue          
##  Length:170         Min.   :1994   Length:170         Length:170        
##  Class :character   1st Qu.:1995   Class :character   Class :character  
##  Mode  :character   Median :1997   Mode  :character   Mode  :character  
##                     Mean   :1997                                        
##                     3rd Qu.:1999                                        
##                     Max.   :2001                                        
##                                                                         
##   Petitioner         Respondent         LowerCourt           Unconst   
##  Length:170         Length:170         Length:170         Min.   :0.0  
##  Class :character   Class :character   Class :character   1st Qu.:0.0  
##  Mode  :character   Mode  :character   Mode  :character   Median :0.0  
##                                                           Mean   :0.3  
##                                                           3rd Qu.:1.0  
##                                                           Max.   :1.0  
##                                                                        
##     Reverse        Circuit.fctr                            Issue.fctr
##  Min.   :0.0000   9th    :43    EconomicActivity                :40  
##  1st Qu.:0.0000   7th    :16    CriminalProcedure               :40  
##  Median :1.0000   6th    :14    JudicialPower                   :24  
##  Mean   :0.5471   5th    :13    CivilRights                     :19  
##  3rd Qu.:1.0000   4th    :13    FirstAmendment                  :14  
##  Max.   :1.0000   3rd    :12    FederalismAndInterstateRelations:10  
##                   (Other):59    (Other)                         :23  
##             Petitioner.fctr           Respondent.fctr LowerCourt.fctr
##  OTHER              :52     OTHER             :57     liberal:88     
##  CRIMINAL.DEFENDENT :31     BUSINESS          :22     conser :82     
##  BUSINESS           :26     STATE             :21                    
##  US                 :14     US                :17                    
##  GOVERNMENT.OFFICIAL:13     CRIMINAL.DEFENDENT:13                    
##  STATE              :10     EMPLOYEE          :10                    
##  (Other)            :24     (Other)           :30                    
##      .rnorm       
##  Min.   :-3.0077  
##  1st Qu.:-0.6076  
##  Median : 0.1553  
##  Mean   : 0.1304  
##  3rd Qu.: 0.8110  
##  Max.   : 3.5577  
##                   
##          Docket            Term         Circuit           Issue 
##               0               0               0               0 
##      Petitioner      Respondent      LowerCourt         Unconst 
##               0               0               0               0 
##         Reverse    Circuit.fctr      Issue.fctr Petitioner.fctr 
##               0               0               0               0 
## Respondent.fctr LowerCourt.fctr          .rnorm 
##               0               0               0
```

```r
# Histogram of predictor in glb_entity_df & glb_newent_df
print(myplot_histogram(glb_entity_df, glb_rsp_var_raw))
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](stevens_caret_files/figure-html/inspectORexplore_data-1.png) 

```r
# Check for duplicates in glb_id_vars
id_vars_dups_df <- subset(id_vars_df <- mycreate_tbl_df(
    rbind(glb_entity_df[, glb_id_vars, FALSE], glb_newent_df[, glb_id_vars, FALSE]),
        glb_id_vars), .freq > 1)
if (nrow(id_vars_dups_df) > 0) {
    warning("Duplicates found in glb_id_vars data:", nrow(id_vars_dups_df))
    myprint_df(id_vars_dups_df)
} else {
    # glb_id_vars are unique across obs in both glb_<>_df
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_id_vars)
}

#pairs(subset(glb_entity_df, select=-c(col_symbol)))
# Check for glb_newent_df & glb_entity_df features range mismatches

# Other diagnostics:
# print(subset(glb_entity_df, <col1_name> == max(glb_entity_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_entity_df$<col1_name>, na.rm=TRUE)))

# print(glb_entity_df[which.max(glb_entity_df$<col_name>),])

# print(<col_name>_freq_glb_entity_df <- mycreate_tbl_df(glb_entity_df, "<col_name>"))
# print(which.min(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>)[, 2]))
# print(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>))
# print(table(is.na(glb_entity_df$<col1_name>), glb_entity_df$<col2_name>))
# print(table(sign(glb_entity_df$<col1_name>), glb_entity_df$<col2_name>))
# print(mycreate_xtab(glb_entity_df, <col1_name>))
# print(mycreate_xtab(glb_entity_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mycreate_xtab(glb_entity_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_entity_df[is.na(<col1_name>_<col2_name>_xtab_glb_entity_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_entity_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>.NA, glb_entity_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_entity_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>"))

glb_script_df <- rbind(glb_script_df, 
    data.frame(chunk_label="manage_missing_data", 
        chunk_step_major=max(glb_script_df$chunk_step_major), 
        chunk_step_minor=glb_script_df[nrow(glb_script_df), "chunk_step_minor"]+1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                    chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed2 inspectORexplore.data                2                1   0.706
## elapsed3   manage_missing_data                2                2   1.721
```

### Step `2`.`2`: manage missing data

```r
# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))
# glb_entity_df <- na.omit(glb_entity_df)
# glb_newent_df <- na.omit(glb_newent_df)
# df[is.na(df)] <- 0

# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function(entity_df, newent_df) {
    if (!glb_is_separate_newent_dataset) {
        # Combine entity & newent
        union_df <- rbind(mutate(entity_df, .src = "entity"),
                          mutate(newent_df, .src = "newent"))
        union_imputed_df <- union_df[, setdiff(setdiff(names(entity_df), 
                                                       glb_rsp_var), 
                                               glb_exclude_vars_as_features)]
        print(summary(union_imputed_df))
    
        require(mice)
        set.seed(glb_mice_complete.seed)
        union_imputed_df <- complete(mice(union_imputed_df))
        print(summary(union_imputed_df))
    
        union_df[, names(union_imputed_df)] <- union_imputed_df[, names(union_imputed_df)]
        print(summary(union_df))
#         union_df$.rownames <- rownames(union_df)
#         union_df <- orderBy(~.rownames, union_df)
#         
#         imp_entity_df <- myimport_data(
#             url="<imputed_trnng_url>", 
#             comment="imp_entity_df", force_header=TRUE, print_diagn=TRUE)
#         print(all.equal(subset(union_df, select=-c(.src, .rownames, .rnorm)), 
#                         imp_entity_df))
        
        # Partition again
        glb_entity_df <<- subset(union_df, .src == "entity", select=-c(.src, .rownames))
        comment(glb_entity_df) <- "entity_df"
        glb_newent_df <<- subset(union_df, .src == "newent", select=-c(.src, .rownames))
        comment(glb_newent_df) <- "newent_df"
        
        # Generate summaries
        print(summary(entity_df))
        print(sapply(names(entity_df), function(col) sum(is.na(entity_df[, col]))))
        print(summary(newent_df))
        print(sapply(names(newent_df), function(col) sum(is.na(newent_df[, col]))))
    
    } else stop("Not implemented yet")
}

if (glb_impute_na_data) {
    if ((sum(sapply(names(glb_entity_df), 
                    function(col) sum(is.na(glb_entity_df[, col])))) > 0) | 
        (sum(sapply(names(glb_newent_df), 
                    function(col) sum(is.na(glb_newent_df[, col])))) > 0))
        glb_impute_missing_data(glb_entity_df, glb_newent_df)
}    

glb_script_df <- rbind(glb_script_df, 
    data.frame(chunk_label="encode_retype_data", 
        chunk_step_major=max(glb_script_df$chunk_step_major), 
        chunk_step_minor=glb_script_df[nrow(glb_script_df), "chunk_step_minor"]+1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                  chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed3 manage_missing_data                2                2   1.721
## elapsed4  encode_retype_data                2                3   2.001
```

### Step `2`.`3`: encode/retype data

```r
# map_<col_name>_df <- myimport_data(
#     url="<map_url>", 
#     comment="map_<col_name>_df", print_diagn=TRUE)
# map_<col_name>_df <- read.csv(paste0(getwd(), "/data/<file_name>.csv"), strip.white=TRUE)

# glb_entity_df <- mymap_codes(glb_entity_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
# glb_newent_df <- mymap_codes(glb_newent_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
# glb_entity_df$<col_name>.fctr <- factor(glb_entity_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_newent_df$<col_name>)))
# glb_newent_df$<col_name>.fctr <- factor(glb_newent_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_newent_df$<col_name>)))

if (!is.null(glb_map_rsp_raw_to_var)) {
    glb_entity_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_entity_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_entity_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    glb_newent_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_newent_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_newent_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)    
}
```

```
## Loading required package: sqldf
## Loading required package: gsubfn
## Loading required package: proto
## Loading required package: RSQLite
## Loading required package: DBI
## Loading required package: tcltk
```

```
##   Reverse Decision.fctr  .n
## 1       1             R 216
## 2       0             A 180
```

![](stevens_caret_files/figure-html/encode_retype_data_1-1.png) 

```
##   Reverse Decision.fctr .n
## 1       1             R 93
## 2       0             A 77
```

![](stevens_caret_files/figure-html/encode_retype_data_1-2.png) 

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                 chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed4 encode_retype_data                2                3   2.001
## elapsed5   extract_features                3                0   4.869
```

## Step `3`: extract features

```r
# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_entity_df$<col_name>), -2, na.pad=TRUE)
# glb_entity_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newent_df$<col_name>), -2, na.pad=TRUE)
# glb_newent_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newent_df[1, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df) - 1, 
#                                                    "<col_name>"]
# glb_newent_df[2, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df), 
#                                                    "<col_name>"]
                                                   
# glb_entity_df <- mutate(glb_entity_df,
#     <new_col_name>=
#                     )

# glb_newent_df <- mutate(glb_newent_df,
#     <new_col_name>=
#                     )

# print(summary(glb_entity_df))
# print(summary(glb_newent_df))

# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))

# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all","data.new")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0
```

![](stevens_caret_files/figure-html/extract_features-1.png) 

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="select_features", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##               chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed5 extract_features                3                0   4.869
## elapsed6  select_features                4                0   6.122
```

## Step `4`: select features

```r
print(glb_feats_df <- 
    myselect_features( entity_df=glb_entity_df, 
                       exclude_vars_as_features=glb_exclude_vars_as_features, 
                       rsp_var=glb_rsp_var))
```

```
##        id      cor.y  cor.y.abs
## 1 Unconst 0.06627144 0.06627144
```

```r
glb_script_df <- rbind(glb_script_df, 
    data.frame(chunk_label="remove_correlated_features", 
        chunk_step_major=max(glb_script_df$chunk_step_major),
        chunk_step_minor=glb_script_df[nrow(glb_script_df), "chunk_step_minor"]+1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))        
print(tail(glb_script_df, 2))
```

```
##                         chunk_label chunk_step_major chunk_step_minor
## elapsed6            select_features                4                0
## elapsed7 remove_correlated_features                4                1
##          elapsed
## elapsed6   6.122
## elapsed7   6.320
```

### Step `4`.`1`: remove correlated features

```r
print(glb_feats_df <- orderBy(~-cor.y, merge(glb_feats_df, 
          mydelete_cor_features(glb_feats_df, glb_entity_df, glb_rsp_var, 
                                glb_exclude_vars_as_features), 
          all.x=TRUE)))
```

```
## Loading required package: reshape2
```

```
##        id      cor.y  cor.y.abs cor.low
## 1 Unconst 0.06627144 0.06627144       1
```

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="run.models", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                         chunk_label chunk_step_major chunk_step_minor
## elapsed7 remove_correlated_features                4                1
## elapsed8                 run.models                5                0
##          elapsed
## elapsed7   6.320
## elapsed8   6.412
```

## Step `5`: run models

```r
max_cor_y_x_var <- subset(glb_feats_df, cor.low == 1)[1, "id"]

#   Regression:
if (glb_is_regression) {
    #   Linear:
    myrun_mdl_fn <- myrun_mdl_lm
}    

#   Classification:
if (glb_is_classification) myrun_mdl_fn <- myrun_mdl_classification 
    
# Add dummy model - random variable
#   Potential Enhancements:
#       For classifiers, it shd generate proba/outcomes that mimics the freq
#           distribution of glb_rsp_var values; Right now it always generates
#           0 (most frequent ?)
ret_lst <- myrun_mdl_fn(indep_vars_vctr=".rnorm",
                         rsp_var=glb_rsp_var, 
                         rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df,
                        method="glm")
```

```
## Loading required package: ROCR
## Loading required package: gplots
## 
## Attaching package: 'gplots'
## 
## The following object is masked from 'package:stats':
## 
##     lowess
## 
## Loading required package: caret
## Loading required package: lattice
## 
## Attaching package: 'caret'
## 
## The following object is masked from 'package:survival':
## 
##     cluster
```

![](stevens_caret_files/figure-html/run.models-1.png) ![](stevens_caret_files/figure-html/run.models-2.png) ![](stevens_caret_files/figure-html/run.models-3.png) ![](stevens_caret_files/figure-html/run.models-4.png) 

```
## 
## Call:  NULL
## 
## Coefficients:
## (Intercept)       .rnorm  
##     0.18489     -0.03377  
## 
## Degrees of Freedom: 395 Total (i.e. Null);  394 Residual
## Null Deviance:	    545.7 
## Residual Deviance: 545.6 	AIC: 549.6
```

```r
glb_dmy_mdl <- ret_lst[["model"]]

# Highest cor.y
ret_lst <- myrun_mdl_fn(indep_vars_vctr=max_cor_y_x_var,
                         rsp_var=glb_rsp_var, 
                         rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df,
                        method="glm")
```

![](stevens_caret_files/figure-html/run.models-5.png) ![](stevens_caret_files/figure-html/run.models-6.png) ![](stevens_caret_files/figure-html/run.models-7.png) ![](stevens_caret_files/figure-html/run.models-8.png) 

```
## 
## Call:  NULL
## 
## Coefficients:
## (Intercept)      Unconst  
##      0.1109       0.3228  
## 
## Degrees of Freedom: 395 Total (i.e. Null);  394 Residual
## Null Deviance:	    545.7 
## Residual Deviance: 543.9 	AIC: 547.9
```

```r
# Enhance Highest cor.y model with additions of interaction terms that were 
#   dropped due to high correlations
if (nrow(subset(glb_feats_df, is.na(cor.low))) > 0)
    ret_lst <- myrun_mdl_fn(indep_vars_vctr=c(max_cor_y_x_var, 
        paste(max_cor_y_x_var, 
              subset(glb_feats_df, is.na(cor.low))[, "id"], sep=":")),
                        glb_rsp_var, glb_rsp_var_out,
                            fit_df=glb_entity_df, OOB_df=glb_newent_df,
                        method="glm")    

# Low correlated X
ret_lst <- myrun_mdl_fn(indep_vars_vctr=subset(glb_feats_df, 
                                               cor.low == 1)[, "id"],
                        glb_rsp_var, glb_rsp_var_out,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df,
                        method="glm")
```

![](stevens_caret_files/figure-html/run.models-9.png) ![](stevens_caret_files/figure-html/run.models-10.png) ![](stevens_caret_files/figure-html/run.models-11.png) ![](stevens_caret_files/figure-html/run.models-12.png) 

```
## 
## Call:  NULL
## 
## Coefficients:
## (Intercept)      Unconst  
##      0.1109       0.3228  
## 
## Degrees of Freedom: 395 Total (i.e. Null);  394 Residual
## Null Deviance:	    545.7 
## Residual Deviance: 543.9 	AIC: 547.9
```

```r
# User specified
for (method in glb_models_method_vctr) {
    #print(sprintf("iterating over method:%s", method))
    
    # All X that is not user excluded
    indep_vars_vctr <- setdiff(names(glb_entity_df), 
        union(glb_rsp_var, glb_exclude_vars_as_features))
    
    # easier to exclude features
#     indep_vars_vctr <- setdiff(names(glb_entity_df), 
#         union(union(glb_rsp_var, glb_exclude_vars_as_features), 
#               c("<feat1_name>", "<feat2_name>")))
    
    # easier to include features
#     indep_vars_vctr <- c("<feat1_name>", "<feat2_name>")

    # User specified bivariate models
#     indep_vars_vctr_lst <- list()
#     for (feat in setdiff(names(glb_entity_df), 
#                          union(glb_rsp_var, glb_exclude_vars_as_features)))
#         indep_vars_vctr_lst[["feat"]] <- feat

    # User specified combinatorial models
#     indep_vars_vctr_lst <- list()
#     combn_mtrx <- combn(c("<feat1_name>", "<feat2_name>", "<featn_name>"), 
#                           <num_feats_to_choose>)
#     for (combn_ix in 1:ncol(combn_mtrx))
#         #print(combn_mtrx[, combn_ix])
#         indep_vars_vctr_lst[[combn_ix]] <- combn_mtrx[, combn_ix]

    set.seed(200)
    ret_lst <- myrun_mdl_fn(indep_vars_vctr=indep_vars_vctr,
                             rsp_var=glb_rsp_var, 
                             rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_entity_df, 
                            OOB_df=glb_newent_df,
                            method=method, 
                             tune_models_df=glb_tune_models_df)
#     glb_<id>_mdl <- ret_lst[["model"]]
}
```

![](stevens_caret_files/figure-html/run.models-13.png) ![](stevens_caret_files/figure-html/run.models-14.png) ![](stevens_caret_files/figure-html/run.models-15.png) 

```
## 
## Call:  NULL
## 
## Coefficients:
##                                (Intercept)  
##                                    0.66858  
##                                    Unconst  
##                                   -0.18029  
##                            Circuit.fctr7th  
##                                   -0.64115  
##                            Circuit.fctrFED  
##                                   -0.70973  
##                             Circuit.fctrDC  
##                                   -0.76663  
##                           Circuit.fctr11th  
##                                   -0.22491  
##                            Circuit.fctr2nd  
##                                    0.97359  
##                            Circuit.fctr6th  
##                                    0.14258  
##                            Circuit.fctr3rd  
##                                   -0.88092  
##                           Circuit.fctr10th  
##                                   -1.34112  
##                            Circuit.fctr8th  
##                                   -0.55667  
##                            Circuit.fctr5th  
##                                    0.69556  
##                            Circuit.fctr4th  
##                                    0.61347  
##                            Circuit.fctr1st  
##                                   -0.88244  
##                    Issue.fctrJudicialPower  
##                                   -0.32201  
##                Issue.fctrCriminalProcedure  
##                                   -0.29264  
##                       Issue.fctrDueProcess  
##                                    0.27264  
## Issue.fctrFederalismAndInterstateRelations  
##                                    0.06834  
##                      Issue.fctrCivilRights  
##                                   -0.05890  
##                   Issue.fctrFirstAmendment  
##                                   -0.71501  
##                  Issue.fctrFederalTaxation  
##                                   -1.98079  
##                           Issue.fctrUnions  
##                                   -0.99795  
##                          Issue.fctrPrivacy  
##                                    2.18674  
##                        Issue.fctrAttorneys  
##                                   -0.21192  
##                        Petitioner.fctrCITY  
##                                   -1.55222  
##                    Petitioner.fctrEMPLOYEE  
##                                    0.73154  
##             Petitioner.fctrAMERICAN.INDIAN  
##                                   -1.36136  
##              Petitioner.fctrINJURED.PERSON  
##                                    0.10632  
##         Petitioner.fctrGOVERNMENT.OFFICIAL  
##                                   -1.00998  
##                       Petitioner.fctrOTHER  
##                                    0.04596  
##                       Petitioner.fctrSTATE  
##                                   -0.54067  
##                          Petitioner.fctrUS  
##                                    0.09438  
##          Petitioner.fctrCRIMINAL.DEFENDENT  
##                                    0.65127  
##                    Petitioner.fctrEMPLOYER  
##                                   -0.67638  
##                  Petitioner.fctrPOLITICIAN  
##                                   -0.58921  
##                        Respondent.fctrCITY  
##                                   -0.89541  
##          Respondent.fctrCRIMINAL.DEFENDENT  
##                                   -1.27049  
##                    Respondent.fctrEMPLOYEE  
##                                   -0.58380  
##                    Respondent.fctrEMPLOYER  
##                                    0.81539  
##             Respondent.fctrAMERICAN.INDIAN  
##                                    1.73754  
##              Respondent.fctrINJURED.PERSON  
##                                   -1.72998  
##         Respondent.fctrGOVERNMENT.OFFICIAL  
##                                   -0.64414  
##                       Respondent.fctrOTHER  
##                                   -0.51043  
##                  Respondent.fctrPOLITICIAN  
##                                    0.06309  
##                       Respondent.fctrSTATE  
##                                    0.36823  
##                          Respondent.fctrUS  
##                                   -1.29002  
##                      LowerCourt.fctrconser  
##                                    0.95835  
## 
## Degrees of Freedom: 395 Total (i.e. Null);  349 Residual
## Null Deviance:	    545.7 
## Residual Deviance: 428.7 	AIC: 522.7
```

```
## Loading required package: rpart
```

![](stevens_caret_files/figure-html/run.models-16.png) 

```
## Loading required package: rpart.plot
```

![](stevens_caret_files/figure-html/run.models-17.png) 

```
## n= 396 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 396 180 R (0.4545455 0.5454545)  
##    2) LowerCourt.fctrconser< 0.5 185  73 A (0.6054054 0.3945946)  
##      4) Respondent.fctrAMERICAN.INDIAN< 0.5 175  65 A (0.6285714 0.3714286)  
##        8) Petitioner.fctrOTHER< 0.5 125  38 A (0.6960000 0.3040000)  
##         16) Circuit.fctr2nd< 0.5 115  31 A (0.7304348 0.2695652) *
##         17) Circuit.fctr2nd>=0.5 10   3 R (0.3000000 0.7000000) *
##        9) Petitioner.fctrOTHER>=0.5 50  23 R (0.4600000 0.5400000)  
##         18) Unconst>=0.5 7   2 A (0.7142857 0.2857143) *
##         19) Unconst< 0.5 43  18 R (0.4186047 0.5813953) *
##      5) Respondent.fctrAMERICAN.INDIAN>=0.5 10   2 R (0.2000000 0.8000000) *
##    3) LowerCourt.fctrconser>=0.5 211  68 R (0.3222749 0.6777251) *
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

![](stevens_caret_files/figure-html/run.models-18.png) ![](stevens_caret_files/figure-html/run.models-19.png) ![](stevens_caret_files/figure-html/run.models-20.png) 

```
## 
## Call:
##  randomForest(x = x, y = y, mtry = param$mtry) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 2
## 
##         OOB estimate of  error rate: 35.35%
## Confusion matrix:
##    A   R class.error
## A 83  97   0.5388889
## R 43 173   0.1990741
```

```r
# Simplify a model
# fit_df <- glb_entity_df; glb_mdl <- step(<complex>_mdl)

plot_models_df <- mutate(glb_models_df, feats.label=substr(feats, 1, 30))
if (glb_is_regression) {
    print(orderBy(~ -R.sq.OOB -Adj.R.sq.fit, glb_models_df))
    stop("glb_sel_mdl not selected")
    print(myplot_scatter(plot_models_df, "Adj.R.sq.fit", "R.sq.OOB") + 
          geom_text(aes(label=feats.label), data=plot_models_df, color="NavyBlue", 
                    size=3.5, angle=45))
}    

if (glb_is_classification) {
    # Lower AIC is better
    print(tmp_models_df <- orderBy(~ -auc.OOB -accuracy.fit +accuracySD.fit, glb_models_df))
    print("Selected model:glb_sel_mdl:")
    glb_sel_mdl <- glb_models_lst[[as.numeric(rownames(tmp_models_df)[1])]]
    print(summary(glb_sel_mdl))
    plot_models_df[, "inv.AIC.fit"] <- 1.0 / plot_models_df[, "AIC.fit"] 
    print(myplot_scatter(plot_models_df, "inv.AIC.fit", "auc.OOB") + 
          geom_text(aes(label=feats.label), data=plot_models_df, color="NavyBlue", 
                    size=3.5, angle=45))
    print(myplot_scatter(plot_models_df, "auc.OOB", "accuracy.fit",  
                         colorcol_name="method") + 
          geom_errorbar(aes(x=auc.OOB, ymin=accuracy.fit - accuracySD.fit,
                            ymax=accuracy.fit + accuracySD.fit), 
            width=(max(plot_models_df$auc.OOB)-min(plot_models_df$auc.OOB))/25) +      
          geom_text(aes(label=feats.label), data=plot_models_df, color="NavyBlue", 
                    size=3.5, angle=45))
}
```

```
##   method
## 6     rf
## 4    glm
## 5  rpart
## 2    glm
## 3    glm
## 1    glm
##                                                                                  feats
## 6 Unconst, Circuit.fctr, Issue.fctr, Petitioner.fctr, Respondent.fctr, LowerCourt.fctr
## 4 Unconst, Circuit.fctr, Issue.fctr, Petitioner.fctr, Respondent.fctr, LowerCourt.fctr
## 5 Unconst, Circuit.fctr, Issue.fctr, Petitioner.fctr, Respondent.fctr, LowerCourt.fctr
## 2                                                                              Unconst
## 3                                                                              Unconst
## 1                                                                               .rnorm
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit   auc.fit
## 6   396       NA       NA           NA    0.000      NA       NA 0.8318287
## 4   396       NA       NA           NA 2419.121      NA 522.7144 0.7948431
## 5   396       NA       NA           NA    0.000      NA       NA 0.6881559
## 2   396       NA       NA           NA 1604.777      NA 547.9432 0.5277778
## 3   396       NA       NA           NA 1604.777      NA 547.9432 0.5277778
## 1   396       NA       NA           NA 1597.666      NA 549.5757 0.5175926
##     auc.OOB accuracy.fit accuracySD.fit
## 6 0.7444491    0.6514103    0.057005888
## 4 0.7267840    0.6113462    0.072459562
## 5 0.7107946    0.6589103    0.077378430
## 2 0.5842759    0.5453846    0.005958436
## 3 0.5842759    0.5453846    0.005958436
## 1 0.4197738    0.5402564    0.015207912
## [1] "Selected model:glb_sel_mdl:"
##                 Length Class      Mode     
## call               4   -none-     call     
## type               1   -none-     character
## predicted        396   factor     numeric  
## err.rate        1500   -none-     numeric  
## confusion          6   -none-     numeric  
## votes            792   matrix     numeric  
## oob.times        396   -none-     numeric  
## classes            2   -none-     character
## importance        46   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y                396   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            46   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
```

```
## Warning: Removed 2 rows containing missing values (geom_point).
```

```
## Warning: Removed 2 rows containing missing values (geom_text).
```

![](stevens_caret_files/figure-html/run.models-21.png) ![](stevens_caret_files/figure-html/run.models-22.png) 

```r
replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "model.selected")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0
```

![](stevens_caret_files/figure-html/run.models-23.png) 

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="fit.data.training.all", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                    chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed8            run.models                5                0   6.412
## elapsed9 fit.data.training.all                6                0  48.313
```

## Step `6`: fit.data.training.all

```r
print(mdl_feats_df <- myextract_mdl_feats( sel_mdl=glb_sel_mdl, 
                                           entity_df=glb_entity_df))
```

```
##                              id importance
## LowerCourt.fctr Petitioner.fctr  100.00000
## Respondent.fctr      Issue.fctr   44.87067
## Petitioner.fctr    Circuit.fctr   27.24806
## Unconst                 Unconst   20.75909
## Circuit.fctr    LowerCourt.fctr   20.07151
## Issue.fctr      Respondent.fctr   16.66406
```

```r
ret_lst <- myrun_mdl_fn(indep_vars_vctr=mdl_feats_df$id,
                         rsp_var=glb_rsp_var, 
                         rsp_var_out=glb_rsp_var_out, 
                        fit_df=glb_entity_df,
                        method=glb_sel_mdl$method,
                         tune_models_df=glb_tune_models_df)
```

![](stevens_caret_files/figure-html/fit.data.training.all_0-1.png) ![](stevens_caret_files/figure-html/fit.data.training.all_0-2.png) 

```
## 
## Call:
##  randomForest(x = x, y = y, mtry = param$mtry) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 3
## 
##         OOB estimate of  error rate: 35.61%
## Confusion matrix:
##    A   R class.error
## A 95  85   0.4722222
## R 56 160   0.2592593
```

```r
glb_sel_mdl <- ret_lst[["model"]]; #glb_sel_mdl_fn <- ret_lst[["model_fn"]]        
# print(glb_models_df[nrow(glb_models_df), ])

glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="fit.data.training.all", 
    chunk_step_major=glb_script_df[nrow(glb_script_df), "chunk_step_major"], 
    chunk_step_minor=glb_script_df[nrow(glb_script_df), "chunk_step_minor"]+1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                     chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed9  fit.data.training.all                6                0  48.313
## elapsed10 fit.data.training.all                6                1  72.870
```


```r
if (glb_is_regression) {
    glb_entity_df[, glb_rsp_var_out] <- predict(glb_sel_mdl, newdata=glb_entity_df)
    print(myplot_scatter(glb_entity_df, glb_rsp_var, glb_rsp_var_out, 
                         smooth=TRUE))
    glb_entity_df[, paste0(glb_rsp_var_out, ".err")] <- 
        abs(glb_entity_df[, glb_rsp_var_out] - glb_entity_df[, glb_rsp_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_rsp_var_out, ".err"))), 
                       glb_entity_df)))                             
}    

if (glb_is_classification) {
            if (any(class(glb_sel_mdl) %in% c("train"))) {
        glb_entity_df[, paste0(glb_rsp_var_out, ".proba")] <- 
            predict(glb_sel_mdl, newdata=glb_entity_df, type="prob")[, 2]
    } else  if (any(class(glb_sel_mdl) %in% c("rpart", "randomForest"))) {
        glb_entity_df[, paste0(glb_rsp_var_out, ".proba")] <- 
            predict(glb_sel_mdl, newdata=glb_entity_df, type="prob")[, 2]
    } else  if (class(glb_sel_mdl) == "glm") {
        stop("not implemented yet")
        glb_entity_df[, paste0(glb_rsp_var_out, ".proba")] <- 
            predict(glb_sel_mdl, newdata=glb_entity_df, type="response")
    } else  stop("not implemented yet")   

    require(ROCR)
    ROCRpred <- prediction(glb_entity_df[, paste0(glb_rsp_var_out, ".proba")],
                           glb_entity_df[, glb_rsp_var])
    ROCRperf <- performance(ROCRpred, "tpr", "fpr")
    plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2,1.7))
    
    thresholds_df <- data.frame(threshold=seq(0.0, 1.0, 0.1))
    thresholds_df$f.score <- sapply(1:nrow(thresholds_df), function(row_ix) 
        mycompute_classifier_f.score(mdl=glb_sel_mdl, obs_df=glb_entity_df, 
                                     proba_threshold=thresholds_df[row_ix, "threshold"], 
                                      rsp_var=glb_rsp_var, 
                                      rsp_var_out=glb_rsp_var_out))
    print(thresholds_df)
    print(myplot_line(thresholds_df, "threshold", "f.score"))
    
    proba_threshold <- thresholds_df[which.max(thresholds_df$f.score), 
                                             "threshold"]
    # This should change to maximize f.score.OOB ???
    print(sprintf("Classifier Probability Threshold: %0.4f to maximize f.score.fit",
                  proba_threshold))
    if (is.null(glb_clf_proba_threshold)) 
        glb_clf_proba_threshold <- proba_threshold else {
        print(sprintf("Classifier Probability Threshold: %0.4f per user specs",
                      glb_clf_proba_threshold))
    }

    if ((class(glb_entity_df[, glb_rsp_var]) != "factor") | 
    	(length(levels(glb_entity_df[, glb_rsp_var])) != 2))
		stop("expecting a factor with two levels:", glb_rsp_var)
	glb_entity_df[, glb_rsp_var_out] <- 
		factor(levels(glb_entity_df[, glb_rsp_var])[
			(glb_entity_df[, paste0(glb_rsp_var_out, ".proba")] >= 
				glb_clf_proba_threshold) * 1 + 1])
    
    print(mycreate_xtab(glb_entity_df, c(glb_rsp_var, glb_rsp_var_out)))
    print(sprintf("f.score=%0.4f", 
        mycompute_classifier_f.score(glb_sel_mdl, glb_entity_df, 
                                     glb_clf_proba_threshold, 
                                     glb_rsp_var, glb_rsp_var_out)))    
}    
```

![](stevens_caret_files/figure-html/fit.data.training.all_1-1.png) 

```
##    threshold   f.score
## 1        0.0 0.7058824
## 2        0.1 0.7188020
## 3        0.2 0.7517730
## 4        0.3 0.7790262
## 5        0.4 0.8016194
## 6        0.5 0.8183807
## 7        0.6 0.7931873
## 8        0.7 0.7237569
## 9        0.8 0.5833333
## 10       0.9 0.3320463
## 11       1.0 0.0000000
```

![](stevens_caret_files/figure-html/fit.data.training.all_1-2.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
## [1] "Classifier Probability Threshold: 0.5000 per user specs"
##   Decision.fctr Decision.fctr.prediction.A Decision.fctr.prediction.R
## 1             A                        126                         54
## 2             R                         29                        187
## [1] "f.score=0.8184"
```

```r
print(glb_feats_df <- mymerge_feats_importance(glb_feats_df, glb_sel_mdl, glb_entity_df))
```

```
##                id      cor.y  cor.y.abs cor.low importance
## 4 Petitioner.fctr         NA         NA      NA  100.00000
## 2      Issue.fctr         NA         NA      NA   48.57694
## 1    Circuit.fctr         NA         NA      NA   31.90709
## 6         Unconst 0.06627144 0.06627144       1   28.01844
## 3 LowerCourt.fctr         NA         NA      NA   26.65404
## 5 Respondent.fctr         NA         NA      NA   20.12564
```

```r
# Most of this code is used again in predict.data.new chunk
glb_analytics_diag_plots <- function(obs_df) {
    for (var in subset(glb_feats_df, !is.na(importance))$id) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_rsp_var, glb_rsp_var_out))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", 
                                 facet_colcol_name="variable", jitter=TRUE))
    }
    
    if (glb_is_regression) {
        plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
        print(myplot_prediction_regression(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"), 
                                           plot_vars_df$id[1],
                    glb_rsp_var, glb_rsp_var_out)
#               + facet_wrap(reformulate(plot_vars_df$id[2])) # if [1,2] is a factor                                                         
#               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
              )
    }    
    
    if (glb_is_classification) {
        if (nrow(plot_vars_df <- subset(glb_feats_df, !is.na(importance))) == 0)
            warning("No features in selected model are statistically important")
        else print(myplot_prediction_classification(df=obs_df, 
                feat_x=ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], 
                              ".rownames"),
                                               feat_y=plot_vars_df$id[1],
                     rsp_var=glb_rsp_var, 
                     rsp_var_out=glb_rsp_var_out, 
                     id_vars=glb_id_vars)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
glb_analytics_diag_plots(obs_df=glb_entity_df)
```

![](stevens_caret_files/figure-html/fit.data.training.all_1-3.png) ![](stevens_caret_files/figure-html/fit.data.training.all_1-4.png) ![](stevens_caret_files/figure-html/fit.data.training.all_1-5.png) ![](stevens_caret_files/figure-html/fit.data.training.all_1-6.png) ![](stevens_caret_files/figure-html/fit.data.training.all_1-7.png) ![](stevens_caret_files/figure-html/fit.data.training.all_1-8.png) 

```
##  [1] Docket                            Term                             
##  [3] Circuit                           Issue                            
##  [5] Petitioner                        Respondent                       
##  [7] LowerCourt                        Unconst                          
##  [9] Reverse                           Circuit.fctr                     
## [11] Issue.fctr                        Petitioner.fctr                  
## [13] Respondent.fctr                   LowerCourt.fctr                  
## [15] .rnorm                            Decision.fctr                    
## [17] Decision.fctr.prediction.proba    Decision.fctr.prediction         
## [19] Decision.fctr.prediction.accurate .label                           
## <0 rows> (or 0-length row.names)
```

![](stevens_caret_files/figure-html/fit.data.training.all_1-9.png) 

```r
replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all.prediction","model.final")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0 
## 3.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  data.training.all.prediction 
## 4.0000 	 5 	 0 1 1 1 
## 4.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  model.final 
## 5.0000 	 4 	 0 0 2 1
```

![](stevens_caret_files/figure-html/fit.data.training.all_1-10.png) 

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="predict.data.new", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                     chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed10 fit.data.training.all                6                1  72.870
## elapsed11      predict.data.new                7                0  78.452
```

## Step `7`: predict data.new

```r
if (glb_is_regression)
    glb_newent_df[, glb_rsp_var_out] <- predict(glb_sel_mdl, 
                                        newdata=glb_newent_df, type="response")

if (glb_is_classification) {
    # Compute selected model predictions
            if (any(class(glb_sel_mdl) %in% c("train"))) {
        glb_newent_df[, paste0(glb_rsp_var_out, ".proba")] <- 
            predict(glb_sel_mdl, newdata=glb_newent_df, type="prob")[, 2]
    } else  if (any(class(glb_sel_mdl) %in% c("rpart", "randomForest"))) {
        glb_newent_df[, paste0(glb_rsp_var_out, ".proba")] <- 
            predict(glb_sel_mdl, newdata=glb_newent_df, type="prob")[, 2]
    } else  if (class(glb_sel_mdl) == "glm") {
        stop("not implemented yet")
        glb_newent_df[, paste0(glb_rsp_var_out, ".proba")] <- 
            predict(glb_sel_mdl, newdata=glb_newent_df, type="response")
    } else  stop("not implemented yet")   

    if ((class(glb_newent_df[, glb_rsp_var]) != "factor") | 
		(length(levels(glb_newent_df[, glb_rsp_var])) != 2))
		stop("expecting a factor with two levels:", glb_rsp_var)
	glb_newent_df[, glb_rsp_var_out] <- 
		factor(levels(glb_newent_df[, glb_rsp_var])[
			(glb_newent_df[, paste0(glb_rsp_var_out, ".proba")] >= 
				glb_clf_proba_threshold) * 1 + 1])
    
    # Compute dummy model predictions
    glb_newent_df[, paste0(glb_rsp_var, ".preddmy.proba")] <- 
        predict(glb_dmy_mdl, newdata=glb_newent_df, type="prob")[, 2]
    if ((class(glb_newent_df[, glb_rsp_var]) != "factor") | 
    	(length(levels(glb_newent_df[, glb_rsp_var])) != 2))
		stop("expecting a factor with two levels:", glb_rsp_var)
	glb_newent_df[, paste0(glb_rsp_var, ".preddmy")] <- 
		factor(levels(glb_newent_df[, glb_rsp_var])[
			(glb_newent_df[, paste0(glb_rsp_var, ".preddmy.proba")] >= 
				glb_clf_proba_threshold) * 1 + 1])

}
    
myprint_df(glb_newent_df[, c(glb_id_vars, glb_rsp_var, glb_rsp_var_out)])
```

```
##     Docket Decision.fctr Decision.fctr.prediction
## 1  93-1408             R                        R
## 3  93-1612             R                        R
## 4   94-623             R                        R
## 6   95-129             A                        R
## 8  96-1768             R                        R
## 21 97-1704             A                        R
##      Docket Decision.fctr Decision.fctr.prediction
## 3   93-1612             R                        R
## 187  99-536             R                        R
## 272 98-1991             A                        A
## 358   97-16             R                        R
## 381  98-405             R                        R
## 416  95-323             R                        R
##      Docket Decision.fctr Decision.fctr.prediction
## 545  00-507             A                        R
## 546 95-1478             A                        A
## 551  00-157             A                        A
## 552 00-1567             A                        A
## 556  95-173             R                        R
## 558 96-6839             R                        R
```

```r
if (glb_is_regression) {
    print(sprintf("Total SSE: %0.4f", 
                  sum((glb_newent_df[, glb_rsp_var_out] - 
                        glb_newent_df[, glb_rsp_var]) ^ 2)))
    print(sprintf("RMSE: %0.4f", 
                  (sum((glb_newent_df[, glb_rsp_var_out] - 
                        glb_newent_df[, glb_rsp_var]) ^ 2) / nrow(glb_newent_df)) ^ 0.5))                        
    print(myplot_scatter(glb_newent_df, glb_rsp_var, glb_rsp_var_out, 
                         smooth=TRUE))
                         
    glb_newent_df[, paste0(glb_rsp_var_out, ".err")] <- 
        abs(glb_newent_df[, glb_rsp_var_out] - glb_newent_df[, glb_rsp_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_rsp_var_out, ".err"))), 
                       glb_newent_df)))                                                      

#     glb_newent_df[, "<Output Pred variable>"] <- func(glb_newent_df[, glb_pred_var_name])                         
}                         

if (glb_is_classification) {
    ROCRpred <- prediction(glb_newent_df[, paste0(glb_rsp_var_out, ".proba")],
                           glb_newent_df[, glb_rsp_var])
    print(sprintf("auc=%0.4f", auc <- as.numeric(performance(ROCRpred, "auc")@y.values)))   
    
    print(sprintf("probability threshold=%0.4f", glb_clf_proba_threshold))
    print(newent_conf_df <- mycreate_xtab(glb_newent_df, 
                                        c(glb_rsp_var, glb_rsp_var_out)))
    print(sprintf("f.score.sel=%0.4f", 
        mycompute_classifier_f.score(mdl=glb_sel_mdl, obs_df=glb_newent_df, 
                                     proba_threshold=glb_clf_proba_threshold, 
                                      rsp_var=glb_rsp_var, 
                                      rsp_var_out=glb_rsp_var_out)))
    print(sprintf("sensitivity=%0.4f", newent_conf_df[2, 3] / 
                      (newent_conf_df[2, 3] + newent_conf_df[2, 2])))
    print(sprintf("specificity=%0.4f", newent_conf_df[1, 2] / 
                      (newent_conf_df[1, 2] + newent_conf_df[1, 3])))
    print(sprintf("accuracy=%0.4f", (newent_conf_df[1, 2] + newent_conf_df[2, 3]) / 
                      (newent_conf_df[1, 2] + newent_conf_df[2, 3] + 
                       newent_conf_df[1, 3] + newent_conf_df[2, 2])))
    
    print(mycreate_xtab(glb_newent_df, c(glb_rsp_var, paste0(glb_rsp_var, ".preddmy"))))
    print(sprintf("f.score.dmy=%0.4f", 
        mycompute_classifier_f.score(mdl=glb_dmy_mdl, obs_df=glb_newent_df, 
                                     proba_threshold=glb_clf_proba_threshold, 
                                      rsp_var=glb_rsp_var, 
                                      rsp_var_out=paste0(glb_rsp_var, ".preddmy"))))
}    
```

```
## [1] "auc=0.7449"
## [1] "probability threshold=0.5000"
##   Decision.fctr Decision.fctr.prediction.A Decision.fctr.prediction.R
## 1             A                         42                         35
## 2             R                         20                         73
## [1] "f.score.sel=0.7264"
## [1] "sensitivity=0.7849"
## [1] "specificity=0.5455"
## [1] "accuracy=0.6765"
##   Decision.fctr Decision.fctr.preddmy.R
## 1             A                      77
## 2             R                      93
## [1] "f.score.dmy=0.7072"
```

```r
glb_analytics_diag_plots(glb_newent_df)
```

![](stevens_caret_files/figure-html/predict.data.new-1.png) ![](stevens_caret_files/figure-html/predict.data.new-2.png) ![](stevens_caret_files/figure-html/predict.data.new-3.png) ![](stevens_caret_files/figure-html/predict.data.new-4.png) ![](stevens_caret_files/figure-html/predict.data.new-5.png) ![](stevens_caret_files/figure-html/predict.data.new-6.png) 

```
##  [1] Docket                            Term                             
##  [3] Circuit                           Issue                            
##  [5] Petitioner                        Respondent                       
##  [7] LowerCourt                        Unconst                          
##  [9] Reverse                           Circuit.fctr                     
## [11] Issue.fctr                        Petitioner.fctr                  
## [13] Respondent.fctr                   LowerCourt.fctr                  
## [15] .rnorm                            Decision.fctr                    
## [17] Decision.fctr.prediction.proba    Decision.fctr.prediction         
## [19] Decision.fctr.preddmy.proba       Decision.fctr.preddmy            
## [21] Decision.fctr.prediction.accurate .label                           
## <0 rows> (or 0-length row.names)
```

![](stevens_caret_files/figure-html/predict.data.new-7.png) 

```r
tmp_replay_lst <- replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.new.prediction")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0 
## 3.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  data.training.all.prediction 
## 4.0000 	 5 	 0 1 1 1 
## 4.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  model.final 
## 5.0000 	 4 	 0 0 2 1 
## 6.0000 	 6 	 0 0 1 2
```

![](stevens_caret_files/figure-html/predict.data.new-8.png) 

```r
#print(ggplot.petrinet(tmp_replay_lst[["pn"]]) + coord_flip())
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 

```r
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
##                   chunk_label chunk_step_major chunk_step_minor elapsed
## 10      fit.data.training.all                6                0  48.313
## 11      fit.data.training.all                6                1  72.870
## 12           predict.data.new                7                0  78.452
## 6            extract_features                3                0   4.869
## 7             select_features                4                0   6.122
## 4         manage_missing_data                2                2   1.721
## 2                cleanse_data                2                0   0.675
## 5          encode_retype_data                2                3   2.001
## 8  remove_correlated_features                4                1   6.320
## 9                  run.models                5                0   6.412
## 3       inspectORexplore.data                2                1   0.706
## 1                 import_data                1                0   0.002
##    elapsed_diff
## 10       41.901
## 11       24.557
## 12        5.582
## 6         2.868
## 7         1.253
## 4         1.015
## 2         0.673
## 5         0.280
## 8         0.198
## 9         0.092
## 3         0.031
## 1         0.000
```

```
## [1] "Total Elapsed Time: 78.452 secs"
```

![](stevens_caret_files/figure-html/print_sessionInfo-1.png) 

```
## R version 3.1.3 (2015-03-09)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.2 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] tcltk     grid      stats     graphics  grDevices utils     datasets 
## [8] methods   base     
## 
## other attached packages:
##  [1] randomForest_4.6-10 rpart.plot_1.5.2    rpart_4.1-9        
##  [4] caret_6.0-41        lattice_0.20-30     ROCR_1.0-6         
##  [7] gplots_2.16.0       reshape2_1.4.1      sqldf_0.4-10       
## [10] RSQLite_1.0.0       DBI_0.3.1           gsubfn_0.6-6       
## [13] proto_0.3-10        plyr_1.8.1          caTools_1.17.1     
## [16] doBy_4.5-13         survival_2.38-1     ggplot2_1.0.1      
## 
## loaded via a namespace (and not attached):
##  [1] bitops_1.0-6        BradleyTerry2_1.0-6 brglm_0.5-9        
##  [4] car_2.0-25          chron_2.3-45        class_7.3-12       
##  [7] codetools_0.2-10    colorspace_1.2-6    compiler_3.1.3     
## [10] digest_0.6.8        e1071_1.6-4         evaluate_0.5.5     
## [13] foreach_1.4.2       formatR_1.0         gdata_2.13.3       
## [16] gtable_0.1.2        gtools_3.4.1        htmltools_0.2.6    
## [19] iterators_1.0.7     KernSmooth_2.23-14  knitr_1.9          
## [22] labeling_0.3        lme4_1.1-7          MASS_7.3-39        
## [25] Matrix_1.1-5        mgcv_1.8-4          minqa_1.2.4        
## [28] munsell_0.4.2       nlme_3.1-120        nloptr_1.0.4       
## [31] nnet_7.3-9          parallel_3.1.3      pbkrtest_0.4-2     
## [34] quantreg_5.11       Rcpp_0.11.5         rmarkdown_0.5.1    
## [37] scales_0.2.4        SparseM_1.6         splines_3.1.3      
## [40] stringr_0.6.2       tools_3.1.3         yaml_2.1.13
```
