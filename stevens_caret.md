# SCOTUS-Stevens: Decision.fctr classification: caret Random Forest
bdanalytics  

**  **    
**Date: (Thu) Apr 02, 2015**    

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

# for random forests (method="rf") & classification, the prediction variable has to be a factor
# for caret if the factor is based on numbers e.g (0/1 vs. "A"/"B"), caret prediction probabilities crashes
glb_predct_var <- "Decision.fctr"           # or NULL
glb_predct_var_name <- paste0(glb_predct_var, ".predict")
glb_id_vars <- c("Docket")                # or NULL

glb_exclude_vars_as_features <- union(glb_id_vars, ".rnorm")     # or NULL                      
# List chrs (convert into factors if it's a valid feature); num/int transformed  
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
        c("Reverse", "Reverse.fctr", 
          "Circuit", "Issue", "Petitioner", "Respondent", "LowerCourt")
                                      )
# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(union(glb_exclude_vars_as_features, 
                        paste(glb_predct_var_name, c("", ".proba"), sep="")),
                                      c(NULL)) # or "<col_name>"

glb_impute_na_data <- FALSE            # or TRUE
glb_mice_complete.seed <- 144               # or any integer
glb_is_regression <- FALSE; glb_is_classification <- TRUE

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
## elapsed1 cleanse_data                2                0   0.564
```

## Step `2`: cleanse data

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="inspect_explore_data", 
                              chunk_step_major=max(glb_script_df$chunk_step_major), 
                              chunk_step_minor=1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                   chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed1         cleanse_data                2                0   0.564
## elapsed2 inspect_explore_data                2                1   0.595
```

### Step `2`.`1`: inspect/explore data

```r
#print(str(glb_entity_df))
#View(glb_entity_df)

# List info gathered for various columns
# <col_name>:   <description>; <notes>

# Create new features that help diagnostics
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

add_new_diag_feats <- function(obs_df, obs_twin_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

        Reverse.fctr=relevel(factor(Reverse, 
                    as.factor(union(obs_df$Reverse, obs_twin_df$Reverse))),
                    ref="0"),
        Decision.fctr=relevel(factor(ifelse(Reverse == 1, "R", "A")), 
                              as.factor(c("R", "A")),
                              ref="A"),
        Circuit.fctr=factor(Circuit, 
                    as.factor(union(obs_df$Circuit, obs_twin_df$Circuit))), 
        Issue.fctr=factor(Issue, 
                    as.factor(union(obs_df$Issue, obs_twin_df$Issue))), 
        Petitioner.fctr=factor(Petitioner, 
                    as.factor(union(obs_df$Petitioner, obs_twin_df$Petitioner))), 
        Respondent.fctr=factor(Respondent, 
                    as.factor(union(obs_df$Respondent, obs_twin_df$Respondent))), 
        LowerCourt.fctr=factor(LowerCourt, 
                    as.factor(union(obs_df$LowerCourt, obs_twin_df$LowerCourt))), 
#         <col_name>.fctr=factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))), 
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
##     Reverse       Reverse.fctr Decision.fctr  Circuit.fctr
##  Min.   :0.0000   0:180        A:180         9th    : 79  
##  1st Qu.:0.0000   1:216        R:216         11th   : 41  
##  Median :1.0000                              5th    : 40  
##  Mean   :0.5455                              4th    : 33  
##  3rd Qu.:1.0000                              8th    : 32  
##  Max.   :1.0000                              7th    : 31  
##                                              (Other):140  
##              Issue.fctr            Petitioner.fctr
##  CriminalProcedure:92   OTHER              :123   
##  JudicialPower    :78   CRIMINAL.DEFENDENT : 58   
##  EconomicActivity :58   BUSINESS           : 53   
##  CivilRights      :55   STATE              : 38   
##  DueProcess       :36   US                 : 34   
##  FirstAmendment   :25   GOVERNMENT.OFFICIAL: 25   
##  (Other)          :52   (Other)            : 65   
##            Respondent.fctr LowerCourt.fctr     .rnorm        
##  OTHER             :120    liberal:185     Min.   :-2.74448  
##  BUSINESS          : 58    conser :211     1st Qu.:-0.58477  
##  US                : 52                    Median :-0.04916  
##  CRIMINAL.DEFENDENT: 45                    Mean   : 0.07455  
##  STATE             : 35                    3rd Qu.: 0.80318  
##  EMPLOYEE          : 18                    Max.   : 3.06873  
##  (Other)           : 68                                      
##          Docket            Term         Circuit           Issue 
##               0               0               0               0 
##      Petitioner      Respondent      LowerCourt         Unconst 
##               0               0               0               0 
##         Reverse    Reverse.fctr   Decision.fctr    Circuit.fctr 
##               0               0               0               0 
##      Issue.fctr Petitioner.fctr Respondent.fctr LowerCourt.fctr 
##               0               0               0               0 
##          .rnorm 
##               0
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
##     Reverse       Reverse.fctr Decision.fctr  Circuit.fctr
##  Min.   :0.0000   0:77         A:77          9th    :43   
##  1st Qu.:0.0000   1:93         R:93          7th    :16   
##  Median :1.0000                              6th    :14   
##  Mean   :0.5471                              5th    :13   
##  3rd Qu.:1.0000                              4th    :13   
##  Max.   :1.0000                              3rd    :12   
##                                              (Other):59   
##                             Issue.fctr            Petitioner.fctr
##  EconomicActivity                :40   OTHER              :52    
##  CriminalProcedure               :40   CRIMINAL.DEFENDENT :31    
##  JudicialPower                   :24   BUSINESS           :26    
##  CivilRights                     :19   US                 :14    
##  FirstAmendment                  :14   GOVERNMENT.OFFICIAL:13    
##  FederalismAndInterstateRelations:10   STATE              :10    
##  (Other)                         :23   (Other)            :24    
##            Respondent.fctr LowerCourt.fctr     .rnorm       
##  OTHER             :57     liberal:88      Min.   :-3.0077  
##  BUSINESS          :22     conser :82      1st Qu.:-0.6076  
##  STATE             :21                     Median : 0.1553  
##  US                :17                     Mean   : 0.1304  
##  CRIMINAL.DEFENDENT:13                     3rd Qu.: 0.8110  
##  EMPLOYEE          :10                     Max.   : 3.5577  
##  (Other)           :30                                      
##          Docket            Term         Circuit           Issue 
##               0               0               0               0 
##      Petitioner      Respondent      LowerCourt         Unconst 
##               0               0               0               0 
##         Reverse    Reverse.fctr   Decision.fctr    Circuit.fctr 
##               0               0               0               0 
##      Issue.fctr Petitioner.fctr Respondent.fctr LowerCourt.fctr 
##               0               0               0               0 
##          .rnorm 
##               0
```

```r
# Histogram of predictor in glb_entity_df & glb_newent_df
print(myplot_histogram(glb_entity_df, glb_predct_var))
```

![](stevens_caret_files/figure-html/inspect_explore_data_1-1.png) 

```r
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
##                   chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed2 inspect_explore_data                2                1   0.595
## elapsed3  manage_missing_data                2                2   1.428
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
                                                       glb_predct_var), 
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
## elapsed3 manage_missing_data                2                2   1.428
## elapsed4  encode_retype_data                2                3   1.668
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

glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                 chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed4 encode_retype_data                2                3   1.668
## elapsed5   extract_features                3                0   1.715
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
## elapsed5 extract_features                3                0   1.715
## elapsed6  select_features                4                0   2.526
```

## Step `4`: select features

```r
print(glb_feats_df <- 
    myselect_features(lcl_entity_df=glb_entity_df, 
                      lcl_exclude_vars_as_features=glb_exclude_vars_as_features, 
                      lcl_predct_var=glb_predct_var))
```

```
##              id      cor.y  cor.y.abs
## Unconst Unconst 0.06627144 0.06627144
## Term       Term 0.03032304 0.03032304
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
## elapsed6   2.526
## elapsed7   2.710
```

### Step `4`.`1`: remove correlated features

```r
print(glb_feats_df <- orderBy(~-cor.y, merge(glb_feats_df, 
          mydelete_cor_features(glb_feats_df, glb_entity_df, glb_predct_var, 
                                glb_exclude_vars_as_features), 
          all.x=TRUE)))
```

```
## Loading required package: reshape2
```

```
##             Unconst        Term
## Unconst 1.000000000 0.002248096
## Term    0.002248096 1.000000000
##             Unconst        Term
## Unconst 0.000000000 0.002248096
## Term    0.002248096 0.000000000
##        id      cor.y  cor.y.abs cor.low
## 2 Unconst 0.06627144 0.06627144       1
## 1    Term 0.03032304 0.03032304       1
```

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="run_models", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                         chunk_label chunk_step_major chunk_step_minor
## elapsed7 remove_correlated_features                4                1
## elapsed8                 run_models                5                0
##          elapsed
## elapsed7   2.710
## elapsed8   2.777
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
#           distribution of glb_predct_var values; Right now it always generates
#           0 (most frequent ?)
ret_lst <- myrun_mdl_fn(indep_vars_vctr=".rnorm",
                        lcl_predct_var=glb_predct_var, 
                        lcl_predct_var_name=glb_predct_var_name,
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

![](stevens_caret_files/figure-html/run_models-1.png) ![](stevens_caret_files/figure-html/run_models-2.png) ![](stevens_caret_files/figure-html/run_models-3.png) ![](stevens_caret_files/figure-html/run_models-4.png) 

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
                        lcl_predct_var=glb_predct_var, 
                        lcl_predct_var_name=glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df,
                        method="glm")
```

![](stevens_caret_files/figure-html/run_models-5.png) ![](stevens_caret_files/figure-html/run_models-6.png) ![](stevens_caret_files/figure-html/run_models-7.png) ![](stevens_caret_files/figure-html/run_models-8.png) 

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
                        glb_predct_var, glb_predct_var_name,
                            fit_df=glb_entity_df, OOB_df=glb_newent_df,
                        method="glm")    

# Low correlated X
ret_lst <- myrun_mdl_fn(indep_vars_vctr=subset(glb_feats_df, 
                                               cor.low == 1)[, "id"],
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df,
                        method="glm")
```

![](stevens_caret_files/figure-html/run_models-9.png) ![](stevens_caret_files/figure-html/run_models-10.png) ![](stevens_caret_files/figure-html/run_models-11.png) ![](stevens_caret_files/figure-html/run_models-12.png) 

```
## 
## Call:  NULL
## 
## Coefficients:
## (Intercept)      Unconst         Term  
##   -57.42477      0.32271      0.02881  
## 
## Degrees of Freedom: 395 Total (i.e. Null);  393 Residual
## Null Deviance:	    545.7 
## Residual Deviance: 543.6 	AIC: 549.6
```

```r
# All X that is not user excluded
ret_lst <- myrun_mdl_fn(indep_vars_vctr=setdiff(names(glb_entity_df), 
    union(glb_predct_var, glb_exclude_vars_as_features)),
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df,
                        method="glm")
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](stevens_caret_files/figure-html/run_models-13.png) ![](stevens_caret_files/figure-html/run_models-14.png) ![](stevens_caret_files/figure-html/run_models-15.png) ![](stevens_caret_files/figure-html/run_models-16.png) 

```
## 
## Call:  NULL
## 
## Coefficients:
##                                (Intercept)  
##                                  -47.91172  
##                                       Term  
##                                    0.02433  
##                                    Unconst  
##                                   -0.17555  
##                            Circuit.fctr7th  
##                                   -0.65021  
##                            Circuit.fctrFED  
##                                   -0.72046  
##                             Circuit.fctrDC  
##                                   -0.76550  
##                           Circuit.fctr11th  
##                                   -0.23589  
##                            Circuit.fctr2nd  
##                                    0.93630  
##                            Circuit.fctr6th  
##                                    0.13244  
##                            Circuit.fctr3rd  
##                                   -0.90093  
##                           Circuit.fctr10th  
##                                   -1.35086  
##                            Circuit.fctr8th  
##                                   -0.56162  
##                            Circuit.fctr5th  
##                                    0.68012  
##                            Circuit.fctr4th  
##                                    0.59484  
##                            Circuit.fctr1st  
##                                   -0.87873  
##                    Issue.fctrJudicialPower  
##                                   -0.32049  
##                Issue.fctrCriminalProcedure  
##                                   -0.30717  
##                       Issue.fctrDueProcess  
##                                    0.25310  
## Issue.fctrFederalismAndInterstateRelations  
##                                    0.08384  
##                      Issue.fctrCivilRights  
##                                   -0.07564  
##                   Issue.fctrFirstAmendment  
##                                   -0.73942  
##                  Issue.fctrFederalTaxation  
##                                   -1.95641  
##                           Issue.fctrUnions  
##                                   -1.01470  
##                          Issue.fctrPrivacy  
##                                    2.12821  
##                        Issue.fctrAttorneys  
##                                   -0.21996  
##                        Petitioner.fctrCITY  
##                                   -1.54739  
##                    Petitioner.fctrEMPLOYEE  
##                                    0.72629  
##             Petitioner.fctrAMERICAN.INDIAN  
##                                   -1.30924  
##              Petitioner.fctrINJURED.PERSON  
##                                    0.08859  
##         Petitioner.fctrGOVERNMENT.OFFICIAL  
##                                   -1.00691  
##                       Petitioner.fctrOTHER  
##                                    0.05366  
##                       Petitioner.fctrSTATE  
##                                   -0.54022  
##                          Petitioner.fctrUS  
##                                    0.11006  
##          Petitioner.fctrCRIMINAL.DEFENDENT  
##                                    0.67585  
##                    Petitioner.fctrEMPLOYER  
##                                   -0.64909  
##                  Petitioner.fctrPOLITICIAN  
##                                   -0.59568  
##                        Respondent.fctrCITY  
##                                   -0.86937  
##          Respondent.fctrCRIMINAL.DEFENDENT  
##                                   -1.27037  
##                    Respondent.fctrEMPLOYEE  
##                                   -0.58328  
##                    Respondent.fctrEMPLOYER  
##                                    0.83259  
##             Respondent.fctrAMERICAN.INDIAN  
##                                    1.74125  
##              Respondent.fctrINJURED.PERSON  
##                                   -1.71043  
##         Respondent.fctrGOVERNMENT.OFFICIAL  
##                                   -0.62522  
##                       Respondent.fctrOTHER  
##                                   -0.49674  
##                  Respondent.fctrPOLITICIAN  
##                                    0.08058  
##                       Respondent.fctrSTATE  
##                                    0.38060  
##                          Respondent.fctrUS  
##                                   -1.29052  
##                      LowerCourt.fctrconser  
##                                    0.96186  
## 
## Degrees of Freedom: 395 Total (i.e. Null);  348 Residual
## Null Deviance:	    545.7 
## Residual Deviance: 428.5 	AIC: 524.5
```

```r
# User specified
for (method in glb_models_method_vctr) {
    #print(sprintf("iterating over method:%s", method))
    
    # easier to exclude features
    indep_vars_vctr <- setdiff(names(glb_entity_df), 
        union(union(glb_predct_var, glb_exclude_vars_as_features), 
              c("Term")))
    
    # easier to include features
#     indep_vars_vctr <- c("<feat1_name>", "<feat2_name>")

    # User specified bivariate models
#     indep_vars_vctr_lst <- list()
#     for (feat in setdiff(names(glb_entity_df), 
#                          union(glb_predct_var, glb_exclude_vars_as_features)))
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
                            glb_predct_var, glb_predct_var_name,
                            fit_df=glb_entity_df, OOB_df=glb_newent_df,
                            method=method)
#     glb_<id>_mdl <- ret_lst[["model"]]
}
```

![](stevens_caret_files/figure-html/run_models-17.png) ![](stevens_caret_files/figure-html/run_models-18.png) ![](stevens_caret_files/figure-html/run_models-19.png) 

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

![](stevens_caret_files/figure-html/run_models-20.png) 

```
## Loading required package: rpart.plot
```

![](stevens_caret_files/figure-html/run_models-21.png) 

```
## n= 396 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 396 180 R (0.4545455 0.5454545)  
##   2) LowerCourt.fctrconser< 0.5 185  73 A (0.6054054 0.3945946)  
##     4) Respondent.fctrAMERICAN.INDIAN< 0.5 175  65 A (0.6285714 0.3714286) *
##     5) Respondent.fctrAMERICAN.INDIAN>=0.5 10   2 R (0.2000000 0.8000000) *
##   3) LowerCourt.fctrconser>=0.5 211  68 R (0.3222749 0.6777251) *
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

![](stevens_caret_files/figure-html/run_models-22.png) ![](stevens_caret_files/figure-html/run_models-23.png) ![](stevens_caret_files/figure-html/run_models-24.png) 

```
## 
## Call:
##  randomForest(x = x, y = y, mtry = param$mtry) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 2
## 
##         OOB estimate of  error rate: 36.11%
## Confusion matrix:
##    A   R class.error
## A 81  99   0.5500000
## R 44 172   0.2037037
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
## 7     rf
## 4    glm
## 5    glm
## 6  rpart
## 2    glm
## 3    glm
## 1    glm
##                                                                                        feats
## 7       Unconst, Circuit.fctr, Issue.fctr, Petitioner.fctr, Respondent.fctr, LowerCourt.fctr
## 4 Term, Unconst, Circuit.fctr, Issue.fctr, Petitioner.fctr, Respondent.fctr, LowerCourt.fctr
## 5       Unconst, Circuit.fctr, Issue.fctr, Petitioner.fctr, Respondent.fctr, LowerCourt.fctr
## 6       Unconst, Circuit.fctr, Issue.fctr, Petitioner.fctr, Respondent.fctr, LowerCourt.fctr
## 2                                                                                    Unconst
## 3                                                                              Unconst, Term
## 1                                                                                     .rnorm
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit   auc.fit
## 7   396       NA       NA           NA    0.000      NA       NA 0.8312757
## 4   396       NA       NA           NA 2394.341      NA 524.5440 0.7945602
## 5   396       NA       NA           NA 2419.121      NA 522.7144 0.7948431
## 6   396       NA       NA           NA    0.000      NA       NA 0.6584105
## 2   396       NA       NA           NA 1604.777      NA 547.9432 0.5277778
## 3   396       NA       NA           NA 1606.558      NA 549.5810 0.5427083
## 1   396       NA       NA           NA 1597.666      NA 549.5757 0.5175926
##     auc.OOB accuracy.fit accuracySD.fit
## 7 0.7420751    0.6538462    0.060549689
## 4 0.7304846    0.6105128    0.114868742
## 5 0.7267840    0.6113462    0.072459562
## 6 0.7228739    0.6361538    0.078428526
## 2 0.5842759    0.5453846    0.005958436
## 3 0.5704511    0.5403205    0.026811684
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

![](stevens_caret_files/figure-html/run_models-25.png) ![](stevens_caret_files/figure-html/run_models-26.png) 

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

![](stevens_caret_files/figure-html/run_models-27.png) 

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
## elapsed8            run_models                5                0   2.777
## elapsed9 fit.data.training.all                6                0  48.782
```

## Step `6`: fit.data.training.all

```r
print(mdl_feats_df <- myextract_mdl_feats(lcl_sel_mdl=glb_sel_mdl, 
                                          lcl_entity_df=glb_entity_df))
```

```
##                              id  importance
## LowerCourt.fctr Petitioner.fctr 100.0000000
## Unconst                 Unconst  19.2694009
## Respondent.fctr      Issue.fctr   6.2600400
## Circuit.fctr    LowerCourt.fctr   5.3118057
## Issue.fctr      Respondent.fctr   0.5524182
## Petitioner.fctr    Circuit.fctr   0.0000000
```

```r
ret_lst <- myrun_mdl_fn(indep_vars_vctr=mdl_feats_df$id,
                        lcl_predct_var=glb_predct_var, 
                        lcl_predct_var_name=glb_predct_var_name, 
                        fit_df=glb_entity_df,
                        method=glb_sel_mdl$method,
                        lcl_tune_models_df=glb_tune_models_df)
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
##         OOB estimate of  error rate: 36.87%
## Confusion matrix:
##    A   R class.error
## A 90  90   0.5000000
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
## elapsed9  fit.data.training.all                6                0  48.782
## elapsed10 fit.data.training.all                6                1  71.905
```


```r
if (glb_is_regression) {
    glb_entity_df[, glb_predct_var_name] <- predict(glb_sel_mdl, newdata=glb_entity_df)
    print(myplot_scatter(glb_entity_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
    glb_entity_df[, paste0(glb_predct_var_name, ".err")] <- 
        abs(glb_entity_df[, glb_predct_var_name] - glb_entity_df[, glb_predct_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_predct_var_name, ".err"))), 
                       glb_entity_df)))                             
}    

if (glb_is_classification) {
            if (any(class(glb_sel_mdl) %in% c("train"))) {
        glb_entity_df[, paste0(glb_predct_var_name, ".proba")] <- 
            predict(glb_sel_mdl, newdata=glb_entity_df, type="prob")[, 2]
    } else  if (any(class(glb_sel_mdl) %in% c("rpart", "randomForest"))) {
        glb_entity_df[, paste0(glb_predct_var_name, ".proba")] <- 
            predict(glb_sel_mdl, newdata=glb_entity_df, type="prob")[, 2]
    } else  if (class(glb_sel_mdl) == "glm") {
        stop("not implemented yet")
        glb_entity_df[, paste0(glb_predct_var_name, ".proba")] <- 
            predict(glb_sel_mdl, newdata=glb_entity_df, type="response")
    } else  stop("not implemented yet")   

    require(ROCR)
    ROCRpred <- prediction(glb_entity_df[, paste0(glb_predct_var_name, ".proba")],
                           glb_entity_df[, glb_predct_var])
    ROCRperf <- performance(ROCRpred, "tpr", "fpr")
    plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2,1.7))
    
    thresholds_df <- data.frame(threshold=seq(0.0, 1.0, 0.1))
    thresholds_df$f.score <- sapply(1:nrow(thresholds_df), function(row_ix) 
        mycompute_classifier_f.score(mdl=glb_sel_mdl, obs_df=glb_entity_df, 
                                     proba_threshold=thresholds_df[row_ix, "threshold"], 
                                     lcl_predct_var=glb_predct_var, 
                                     lcl_predct_var_name=glb_predct_var_name))
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

    if ((class(glb_entity_df[, glb_predct_var]) != "factor") | 
    	(length(levels(glb_entity_df[, glb_predct_var])) != 2))
		stop("expecting a factor with two levels:", glb_predct_var)
	glb_entity_df[, glb_predct_var_name] <- 
		factor(levels(glb_entity_df[, glb_predct_var])[
			(glb_entity_df[, paste0(glb_predct_var_name, ".proba")] >= 
				glb_clf_proba_threshold) * 1 + 1])
    
    print(mycreate_xtab(glb_entity_df, c(glb_predct_var, glb_predct_var_name)))
    print(sprintf("f.score=%0.4f", 
        mycompute_classifier_f.score(glb_sel_mdl, glb_entity_df, 
                                     glb_clf_proba_threshold, 
                                     glb_predct_var, glb_predct_var_name)))    
}    
```

![](stevens_caret_files/figure-html/fit.data.training.all_1-1.png) 

```
##    threshold   f.score
## 1        0.0 0.7058824
## 2        0.1 0.7200000
## 3        0.2 0.7544484
## 4        0.3 0.7842402
## 5        0.4 0.7967480
## 6        0.5 0.8219780
## 7        0.6 0.8000000
## 8        0.7 0.7022472
## 9        0.8 0.5833333
## 10       0.9 0.2650602
## 11       1.0 0.0000000
```

![](stevens_caret_files/figure-html/fit.data.training.all_1-2.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
## [1] "Classifier Probability Threshold: 0.5000 per user specs"
##   Decision.fctr Decision.fctr.predict.A Decision.fctr.predict.R
## 1             A                     128                      52
## 2             R                      29                     187
## [1] "f.score=0.8220"
```

```r
print(glb_feats_df <- mymerge_feats_importance(glb_feats_df, glb_sel_mdl, glb_entity_df))
```

```
##                id      cor.y  cor.y.abs cor.low  importance
## 4 Petitioner.fctr         NA         NA      NA 100.0000000
## 7         Unconst 0.06627144 0.06627144       1  25.4763117
## 2      Issue.fctr         NA         NA      NA   7.2530286
## 3 LowerCourt.fctr         NA         NA      NA   5.4965117
## 5 Respondent.fctr         NA         NA      NA   0.6156672
## 1    Circuit.fctr         NA         NA      NA   0.0000000
## 6            Term 0.03032304 0.03032304       1          NA
```

```r
# Most of this code is used again in predict.data.new chunk
glb_analytics_diag_plots <- function(obs_df) {
    for (var in subset(glb_feats_df, !is.na(importance))$id) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_predct_var, glb_predct_var_name))
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
                    glb_predct_var, glb_predct_var_name)
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
                    lcl_predct_var=glb_predct_var, 
                    lcl_predct_var_name=glb_predct_var_name, 
                    lcl_id_vars=glb_id_vars)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
glb_analytics_diag_plots(obs_df=glb_entity_df)
```

![](stevens_caret_files/figure-html/fit.data.training.all_1-3.png) ![](stevens_caret_files/figure-html/fit.data.training.all_1-4.png) ![](stevens_caret_files/figure-html/fit.data.training.all_1-5.png) ![](stevens_caret_files/figure-html/fit.data.training.all_1-6.png) ![](stevens_caret_files/figure-html/fit.data.training.all_1-7.png) ![](stevens_caret_files/figure-html/fit.data.training.all_1-8.png) 

```
##     Docket Term Circuit            Issue          Petitioner Respondent
## 2  93-1577 1994     9th EconomicActivity            BUSINESS   BUSINESS
## 12 99-1571 2000     6th EconomicActivity            BUSINESS   BUSINESS
## 20 95-1872 1996     8th      CivilRights     AMERICAN.INDIAN   BUSINESS
## 28  00-152 2000     9th    JudicialPower GOVERNMENT.OFFICIAL   BUSINESS
## 35 94-1592 1995     7th           Unions               OTHER   BUSINESS
## 37   95-26 1995     FED EconomicActivity               OTHER   BUSINESS
##    LowerCourt Unconst Reverse Reverse.fctr Decision.fctr Circuit.fctr
## 2     liberal       0       1            1             R          9th
## 12     conser       1       1            1             R          6th
## 20     conser       0       0            0             A          8th
## 28    liberal       0       1            1             R          9th
## 35     conser       0       0            0             A          7th
## 37     conser       1       0            0             A          FED
##          Issue.fctr     Petitioner.fctr Respondent.fctr LowerCourt.fctr
## 2  EconomicActivity            BUSINESS        BUSINESS         liberal
## 12 EconomicActivity            BUSINESS        BUSINESS          conser
## 20      CivilRights     AMERICAN.INDIAN        BUSINESS          conser
## 28    JudicialPower GOVERNMENT.OFFICIAL        BUSINESS         liberal
## 35           Unions               OTHER        BUSINESS          conser
## 37 EconomicActivity               OTHER        BUSINESS          conser
##        .rnorm Decision.fctr.predict.proba Decision.fctr.predict
## 2   0.3156464                       0.484                     A
## 12  0.2705301                       0.834                     R
## 20  1.5200465                       0.568                     R
## 28 -0.3929619                       0.214                     A
## 35 -0.3006087                       0.652                     R
## 37 -0.4273334                       0.510                     R
##    Decision.fctr.fctr Decision.fctr.predict.accurate  .label
## 2                   R                          FALSE 93-1577
## 12                  R                           TRUE 99-1571
## 20                  A                          FALSE 95-1872
## 28                  R                          FALSE  00-152
## 35                  A                          FALSE 94-1592
## 37                  A                          FALSE   95-26
##      Docket Term Circuit             Issue         Petitioner
## 12  99-1571 2000     6th  EconomicActivity           BUSINESS
## 20  95-1872 1996     8th       CivilRights    AMERICAN.INDIAN
## 61   00-276 2000     6th  EconomicActivity              OTHER
## 200  94-771 1994    10th       CivilRights              OTHER
## 270   98-84 1998     3rd       CivilRights           BUSINESS
## 529 97-7164 1998     2nd CriminalProcedure CRIMINAL.DEFENDENT
##          Respondent LowerCourt Unconst Reverse Reverse.fctr Decision.fctr
## 12         BUSINESS     conser       1       1            1             R
## 20         BUSINESS     conser       0       0            0             A
## 61         BUSINESS    liberal       0       0            0             A
## 200 AMERICAN.INDIAN    liberal       0       0            0             A
## 270           OTHER    liberal       0       1            1             R
## 529              US     conser       0       0            0             A
##     Circuit.fctr        Issue.fctr    Petitioner.fctr Respondent.fctr
## 12           6th  EconomicActivity           BUSINESS        BUSINESS
## 20           8th       CivilRights    AMERICAN.INDIAN        BUSINESS
## 61           6th  EconomicActivity              OTHER        BUSINESS
## 200         10th       CivilRights              OTHER AMERICAN.INDIAN
## 270          3rd       CivilRights           BUSINESS           OTHER
## 529          2nd CriminalProcedure CRIMINAL.DEFENDENT              US
##     LowerCourt.fctr     .rnorm Decision.fctr.predict.proba
## 12           conser  0.2705301                       0.834
## 20           conser  1.5200465                       0.568
## 61          liberal  0.9395453                       0.656
## 200         liberal -0.5603275                       0.670
## 270         liberal -0.2974204                       0.426
## 529          conser -1.1304355                       0.696
##     Decision.fctr.predict Decision.fctr.fctr
## 12                      R                  R
## 20                      R                  A
## 61                      R                  A
## 200                     R                  A
## 270                     A                  R
## 529                     R                  A
##     Decision.fctr.predict.accurate  .label
## 12                            TRUE 99-1571
## 20                           FALSE 95-1872
## 61                           FALSE  00-276
## 200                          FALSE  94-771
## 270                          FALSE   98-84
## 529                          FALSE 97-7164
##      Docket Term Circuit             Issue         Petitioner Respondent
## 529 97-7164 1998     2nd CriminalProcedure CRIMINAL.DEFENDENT         US
## 532 97-9217 1998     3rd CriminalProcedure CRIMINAL.DEFENDENT         US
## 542 99-9073 2000     7th CriminalProcedure CRIMINAL.DEFENDENT         US
## 557  96-262 1996     FED     JudicialPower              OTHER         US
## 562 99-5153 1999     6th CriminalProcedure              OTHER         US
## 564 99-8508 2000     9th CriminalProcedure              OTHER         US
##     LowerCourt Unconst Reverse Reverse.fctr Decision.fctr Circuit.fctr
## 529     conser       0       0            0             A          2nd
## 532     conser       0       0            0             A          3rd
## 542     conser       0       0            0             A          7th
## 557     conser       0       0            0             A          FED
## 562     conser       0       0            0             A          6th
## 564     conser       1       0            0             A          9th
##            Issue.fctr    Petitioner.fctr Respondent.fctr LowerCourt.fctr
## 529 CriminalProcedure CRIMINAL.DEFENDENT              US          conser
## 532 CriminalProcedure CRIMINAL.DEFENDENT              US          conser
## 542 CriminalProcedure CRIMINAL.DEFENDENT              US          conser
## 557     JudicialPower              OTHER              US          conser
## 562 CriminalProcedure              OTHER              US          conser
## 564 CriminalProcedure              OTHER              US          conser
##         .rnorm Decision.fctr.predict.proba Decision.fctr.predict
## 529 -1.1304355                       0.696                     R
## 532 -0.9045173                       0.572                     R
## 542  0.8678893                       0.568                     R
## 557  1.6463979                       0.566                     R
## 562  0.4310960                       0.522                     R
## 564 -0.1638353                       0.560                     R
##     Decision.fctr.fctr Decision.fctr.predict.accurate  .label
## 529                  A                          FALSE 97-7164
## 532                  A                          FALSE 97-9217
## 542                  A                          FALSE 99-9073
## 557                  A                          FALSE  96-262
## 562                  A                          FALSE 99-5153
## 564                  A                          FALSE 99-8508
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
## elapsed10 fit.data.training.all                6                1  71.905
## elapsed11      predict.data.new                7                0  76.635
```

## Step `7`: predict data.new

```r
if (glb_is_regression)
    glb_newent_df[, glb_predct_var_name] <- predict(glb_sel_mdl, 
                                        newdata=glb_newent_df, type="response")

if (glb_is_classification) {
    # Compute selected model predictions
            if (any(class(glb_sel_mdl) %in% c("train"))) {
        glb_newent_df[, paste0(glb_predct_var_name, ".proba")] <- 
            predict(glb_sel_mdl, newdata=glb_newent_df, type="prob")[, 2]
    } else  if (any(class(glb_sel_mdl) %in% c("rpart", "randomForest"))) {
        glb_newent_df[, paste0(glb_predct_var_name, ".proba")] <- 
            predict(glb_sel_mdl, newdata=glb_newent_df, type="prob")[, 2]
    } else  if (class(glb_sel_mdl) == "glm") {
        stop("not implemented yet")
        glb_newent_df[, paste0(glb_predct_var_name, ".proba")] <- 
            predict(glb_sel_mdl, newdata=glb_newent_df, type="response")
    } else  stop("not implemented yet")   

    if ((class(glb_newent_df[, glb_predct_var]) != "factor") | 
		(length(levels(glb_newent_df[, glb_predct_var])) != 2))
		stop("expecting a factor with two levels:", glb_predct_var)
	glb_newent_df[, glb_predct_var_name] <- 
		factor(levels(glb_newent_df[, glb_predct_var])[
			(glb_newent_df[, paste0(glb_predct_var_name, ".proba")] >= 
				glb_clf_proba_threshold) * 1 + 1])
    
    # Compute dummy model predictions
    glb_newent_df[, paste0(glb_predct_var, ".preddmy.proba")] <- 
        predict(glb_dmy_mdl, newdata=glb_newent_df, type="prob")[, 2]
    if ((class(glb_newent_df[, glb_predct_var]) != "factor") | 
    	(length(levels(glb_newent_df[, glb_predct_var])) != 2))
		stop("expecting a factor with two levels:", glb_predct_var)
	glb_newent_df[, paste0(glb_predct_var, ".preddmy")] <- 
		factor(levels(glb_newent_df[, glb_predct_var])[
			(glb_newent_df[, paste0(glb_predct_var, ".preddmy.proba")] >= 
				glb_clf_proba_threshold) * 1 + 1])

}
    
myprint_df(glb_newent_df[, c(glb_id_vars, glb_predct_var, glb_predct_var_name)])
```

```
##     Docket Decision.fctr Decision.fctr.predict
## 1  93-1408             R                     R
## 3  93-1612             R                     R
## 4   94-623             R                     R
## 6   95-129             A                     R
## 8  96-1768             R                     R
## 21 97-1704             A                     R
##      Docket Decision.fctr Decision.fctr.predict
## 47  96-1470             R                     R
## 268  97-303             A                     A
## 350  95-938             R                     R
## 432 99-1864             R                     R
## 478 00-5961             R                     R
## 480  98-942             R                     R
##      Docket Decision.fctr Decision.fctr.predict
## 545  00-507             A                     R
## 546 95-1478             A                     A
## 551  00-157             A                     A
## 552 00-1567             A                     A
## 556  95-173             R                     R
## 558 96-6839             R                     R
```

```r
if (glb_is_regression) {
    print(sprintf("Total SSE: %0.4f", 
                  sum((glb_newent_df[, glb_predct_var_name] - 
                        glb_newent_df[, glb_predct_var]) ^ 2)))
    print(sprintf("RMSE: %0.4f", 
                  (sum((glb_newent_df[, glb_predct_var_name] - 
                        glb_newent_df[, glb_predct_var]) ^ 2) / nrow(glb_newent_df)) ^ 0.5))                        
    print(myplot_scatter(glb_newent_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
                         
    glb_newent_df[, paste0(glb_predct_var_name, ".err")] <- 
        abs(glb_newent_df[, glb_predct_var_name] - glb_newent_df[, glb_predct_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_predct_var_name, ".err"))), 
                       glb_newent_df)))                                                      

#     glb_newent_df[, "<Output Pred variable>"] <- func(glb_newent_df[, glb_pred_var_name])                         
}                         

if (glb_is_classification) {
    ROCRpred <- prediction(glb_newent_df[, paste0(glb_predct_var_name, ".proba")],
                           glb_newent_df[, glb_predct_var])
    print(sprintf("auc=%0.4f", auc <- as.numeric(performance(ROCRpred, "auc")@y.values)))   
    
    print(sprintf("probability threshold=%0.4f", glb_clf_proba_threshold))
    print(newent_conf_df <- mycreate_xtab(glb_newent_df, 
                                        c(glb_predct_var, glb_predct_var_name)))
    print(sprintf("f.score.sel=%0.4f", 
        mycompute_classifier_f.score(mdl=glb_sel_mdl, obs_df=glb_newent_df, 
                                     proba_threshold=glb_clf_proba_threshold, 
                                     lcl_predct_var=glb_predct_var, 
                                     lcl_predct_var_name=glb_predct_var_name)))
    print(sprintf("sensitivity=%0.4f", newent_conf_df[2, 3] / 
                      (newent_conf_df[2, 3] + newent_conf_df[2, 2])))
    print(sprintf("specificity=%0.4f", newent_conf_df[1, 2] / 
                      (newent_conf_df[1, 2] + newent_conf_df[1, 3])))
    print(sprintf("accuracy=%0.4f", (newent_conf_df[1, 2] + newent_conf_df[2, 3]) / 
                      (newent_conf_df[1, 2] + newent_conf_df[2, 3] + 
                       newent_conf_df[1, 3] + newent_conf_df[2, 2])))
    
    print(mycreate_xtab(glb_newent_df, c(glb_predct_var, paste0(glb_predct_var, ".preddmy"))))
    print(sprintf("f.score.dmy=%0.4f", 
        mycompute_classifier_f.score(mdl=glb_dmy_mdl, obs_df=glb_newent_df, 
                                     proba_threshold=glb_clf_proba_threshold, 
                                     lcl_predct_var=glb_predct_var, 
                                     lcl_predct_var_name=paste0(glb_predct_var, ".preddmy"))))
}    
```

```
## [1] "auc=0.7366"
## [1] "probability threshold=0.5000"
##   Decision.fctr Decision.fctr.predict.A Decision.fctr.predict.R
## 1             A                      44                      33
## 2             R                      21                      72
## [1] "f.score.sel=0.7273"
## [1] "sensitivity=0.7742"
## [1] "specificity=0.5714"
## [1] "accuracy=0.6824"
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
##     Docket Term Circuit            Issue     Petitioner Respondent
## 1  93-1408 1994     2nd EconomicActivity       BUSINESS   BUSINESS
## 6   95-129 1995     9th EconomicActivity       BUSINESS   BUSINESS
## 21 97-1704 1998     5th    JudicialPower INJURED.PERSON   BUSINESS
## 40 95-1065 1996     2nd   FirstAmendment          OTHER   BUSINESS
## 53 97-1130 1998     FED EconomicActivity          OTHER   BUSINESS
## 66 99-1871 2000     9th          Privacy          OTHER   BUSINESS
##    LowerCourt Unconst Reverse Reverse.fctr Decision.fctr Circuit.fctr
## 1     liberal       0       1            1             R          2nd
## 6      conser       1       0            0             A          9th
## 21    liberal       1       0            0             A          5th
## 40    liberal       1       0            0             A          2nd
## 53    liberal       0       0            0             A          FED
## 66     conser       0       0            0             A          9th
##          Issue.fctr Petitioner.fctr Respondent.fctr LowerCourt.fctr
## 1  EconomicActivity        BUSINESS        BUSINESS         liberal
## 6  EconomicActivity        BUSINESS        BUSINESS          conser
## 21    JudicialPower  INJURED.PERSON        BUSINESS         liberal
## 40   FirstAmendment           OTHER        BUSINESS         liberal
## 53 EconomicActivity           OTHER        BUSINESS         liberal
## 66          Privacy           OTHER        BUSINESS          conser
##        .rnorm Decision.fctr.predict.proba Decision.fctr.predict
## 1  -0.6413986                       0.690                     R
## 6  -0.8008941                       0.756                     R
## 21 -0.4487406                       0.528                     R
## 40 -0.3238044                       0.512                     R
## 53 -0.4352795                       0.572                     R
## 66  0.9040435                       0.918                     R
##    Decision.fctr.preddmy.proba Decision.fctr.preddmy Decision.fctr.fctr
## 1                    0.5514549                     R                  R
## 6                    0.5527867                     R                  A
## 21                   0.5498453                     R                  A
## 40                   0.5488009                     R                  A
## 53                   0.5497328                     R                  A
## 66                   0.5385155                     R                  A
##    Decision.fctr.predict.accurate  .label
## 1                            TRUE 93-1408
## 6                           FALSE  95-129
## 21                          FALSE 97-1704
## 40                          FALSE 95-1065
## 53                          FALSE 97-1130
## 66                          FALSE 99-1871
##      Docket Term Circuit            Issue         Petitioner Respondent
## 66  99-1871 2000     9th          Privacy              OTHER   BUSINESS
## 148  94-325 1994     2nd EconomicActivity           BUSINESS   EMPLOYEE
## 175  96-651 1996     3rd           Unions              STATE   EMPLOYEE
## 394  99-603 2000     2nd   FirstAmendment              OTHER      OTHER
## 441  94-558 1994     5th      CivilRights                 US POLITICIAN
## 482 99-1964 2000     3rd    JudicialPower CRIMINAL.DEFENDENT      STATE
##     LowerCourt Unconst Reverse Reverse.fctr Decision.fctr Circuit.fctr
## 66      conser       0       0            0             A          9th
## 148    liberal       0       0            0             A          2nd
## 175    liberal       0       1            1             R          3rd
## 394    liberal       0       0            0             A          2nd
## 441    liberal       0       1            1             R          5th
## 482     conser       0       0            0             A          3rd
##           Issue.fctr    Petitioner.fctr Respondent.fctr LowerCourt.fctr
## 66           Privacy              OTHER        BUSINESS          conser
## 148 EconomicActivity           BUSINESS        EMPLOYEE         liberal
## 175           Unions              STATE        EMPLOYEE         liberal
## 394   FirstAmendment              OTHER           OTHER         liberal
## 441      CivilRights                 US      POLITICIAN         liberal
## 482    JudicialPower CRIMINAL.DEFENDENT           STATE          conser
##         .rnorm Decision.fctr.predict.proba Decision.fctr.predict
## 66   0.9040435                       0.918                     R
## 148 -0.2926154                       0.584                     R
## 175  0.4397330                       0.202                     A
## 394  0.5146458                       0.594                     R
## 441 -0.4416542                       0.458                     A
## 482  1.3568613                       0.672                     R
##     Decision.fctr.preddmy.proba Decision.fctr.preddmy Decision.fctr.fctr
## 66                    0.5385155                     R                  A
## 148                   0.5485401                     R                  A
## 175                   0.5424092                     R                  R
## 394                   0.5417813                     R                  A
## 441                   0.5497861                     R                  R
## 482                   0.5347135                     R                  A
##     Decision.fctr.predict.accurate  .label
## 66                           FALSE 99-1871
## 148                          FALSE  94-325
## 175                          FALSE  96-651
## 394                          FALSE  99-603
## 441                          FALSE  94-558
## 482                          FALSE 99-1964
##      Docket Term Circuit             Issue         Petitioner Respondent
## 468 98-5864 1998     4th CriminalProcedure CRIMINAL.DEFENDENT      STATE
## 482 99-1964 2000     3rd     JudicialPower CRIMINAL.DEFENDENT      STATE
## 494 99-2047 2000     1st       CivilRights              OTHER      STATE
## 504 94-1664 1995     9th CriminalProcedure CRIMINAL.DEFENDENT         US
## 527 97-6270 1997     1st CriminalProcedure CRIMINAL.DEFENDENT         US
## 545  00-507 2001    10th       CivilRights    AMERICAN.INDIAN         US
##     LowerCourt Unconst Reverse Reverse.fctr Decision.fctr Circuit.fctr
## 468     conser       1       0            0             A          4th
## 482     conser       0       0            0             A          3rd
## 494    liberal       1       0            0             A          1st
## 504     conser       0       0            0             A          9th
## 527     conser       0       0            0             A          1st
## 545     conser       0       0            0             A         10th
##            Issue.fctr    Petitioner.fctr Respondent.fctr LowerCourt.fctr
## 468 CriminalProcedure CRIMINAL.DEFENDENT           STATE          conser
## 482     JudicialPower CRIMINAL.DEFENDENT           STATE          conser
## 494       CivilRights              OTHER           STATE         liberal
## 504 CriminalProcedure CRIMINAL.DEFENDENT              US          conser
## 527 CriminalProcedure CRIMINAL.DEFENDENT              US          conser
## 545       CivilRights    AMERICAN.INDIAN              US          conser
##           .rnorm Decision.fctr.predict.proba Decision.fctr.predict
## 468 -0.536313833                       0.990                     R
## 482  1.356861338                       0.672                     R
## 494 -0.221110732                       0.532                     R
## 504  1.236920941                       0.732                     R
## 527 -0.003943005                       0.678                     R
## 545  0.222748927                       0.510                     R
##     Decision.fctr.preddmy.proba Decision.fctr.preddmy Decision.fctr.fctr
## 468                   0.5505771                     R                  A
## 482                   0.5347135                     R                  A
## 494                   0.5479421                     R                  A
## 504                   0.5357210                     R                  A
## 527                   0.5461251                     R                  A
## 545                   0.5442272                     R                  A
##     Decision.fctr.predict.accurate  .label
## 468                          FALSE 98-5864
## 482                          FALSE 99-1964
## 494                          FALSE 99-2047
## 504                          FALSE 94-1664
## 527                          FALSE 97-6270
## 545                          FALSE  00-507
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
## 10      fit.data.training.all                6                0  48.782
## 11      fit.data.training.all                6                1  71.905
## 12           predict.data.new                7                0  76.635
## 4         manage_missing_data                2                2   1.428
## 7             select_features                4                0   2.526
## 2                cleanse_data                2                0   0.564
## 5          encode_retype_data                2                3   1.668
## 8  remove_correlated_features                4                1   2.710
## 9                  run_models                5                0   2.777
## 6            extract_features                3                0   1.715
## 3        inspect_explore_data                2                1   0.595
## 1                 import_data                1                0   0.002
##    elapsed_diff
## 10       46.005
## 11       23.123
## 12        4.730
## 4         0.833
## 7         0.811
## 2         0.562
## 5         0.240
## 8         0.184
## 9         0.067
## 6         0.047
## 3         0.031
## 1         0.000
```

```
## [1] "Total Elapsed Time: 76.635 secs"
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
## [1] grid      stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
##  [1] randomForest_4.6-10 rpart.plot_1.5.2    rpart_4.1-9        
##  [4] caret_6.0-41        lattice_0.20-30     ROCR_1.0-6         
##  [7] gplots_2.16.0       reshape2_1.4.1      plyr_1.8.1         
## [10] caTools_1.17.1      doBy_4.5-13         survival_2.38-1    
## [13] ggplot2_1.0.1      
## 
## loaded via a namespace (and not attached):
##  [1] bitops_1.0-6        BradleyTerry2_1.0-6 brglm_0.5-9        
##  [4] car_2.0-25          class_7.3-12        codetools_0.2-10   
##  [7] colorspace_1.2-6    compiler_3.1.3      digest_0.6.8       
## [10] e1071_1.6-4         evaluate_0.5.5      foreach_1.4.2      
## [13] formatR_1.0         gdata_2.13.3        gtable_0.1.2       
## [16] gtools_3.4.1        htmltools_0.2.6     iterators_1.0.7    
## [19] KernSmooth_2.23-14  knitr_1.9           labeling_0.3       
## [22] lme4_1.1-7          MASS_7.3-39         Matrix_1.1-5       
## [25] mgcv_1.8-4          minqa_1.2.4         munsell_0.4.2      
## [28] nlme_3.1-120        nloptr_1.0.4        nnet_7.3-9         
## [31] parallel_3.1.3      pbkrtest_0.4-2      proto_0.3-10       
## [34] quantreg_5.11       Rcpp_0.11.5         rmarkdown_0.5.1    
## [37] scales_0.2.4        SparseM_1.6         splines_3.1.3      
## [40] stringr_0.6.2       tools_3.1.3         yaml_2.1.13
```
