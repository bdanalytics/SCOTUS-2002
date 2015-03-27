# SCOTUS-Stevens: Reverse classification: glm
bdanalytics  

**  **    
**Date: (Fri) Mar 27, 2015**    

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

glb_predct_var <- "Reverse"           # or NULL
glb_predct_var_name <- paste0(glb_predct_var, ".predict")
glb_id_vars <- c("Docket")                # or NULL

glb_exclude_vars_as_features <- union(glb_id_vars, ".rnorm")     # or NULL                      
# List chrs (convert into factors if it's a valid feature); num/int transformed  
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                c("Circuit", "Issue", "Petitioner", "Respondent", "LowerCourt")
                                      )
# List feats that shd be excluded due to known causation by prediction variable
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("<col_name>")     # or NULL
#                                       )

glb_impute_na_data <- FALSE            # or TRUE
glb_mice_complete.seed <- 144               # or any integer
glb_is_regression <- FALSE; glb_is_classification <- TRUE

glb_sel_mdl <- glb_dmy_mdl <- NULL
glb_models_lst <- list()
glb_models_df <- data.frame()

script_df <- data.frame(chunk_label="import_data", chunk_step_major=1, chunk_step_minor=0)
print(script_df)
```

```
##   chunk_label chunk_step_major chunk_step_minor
## 1 import_data                1                0
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
                split <- sample.split(glb_entity_df[, glb_predct_var], 
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
script_df <- rbind(script_df,
                   data.frame(chunk_label="cleanse_data", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##    chunk_label chunk_step_major chunk_step_minor
## 1  import_data                1                0
## 2 cleanse_data                2                0
```

## Step `2`: cleanse data

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="inspect_explore_data", 
                              chunk_step_major=max(script_df$chunk_step_major), 
                              chunk_step_minor=1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
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
#                                   "<max_n_val>"), 

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
print(myplot_histogram(glb_entity_df, glb_predct_var))
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](stevens_glm_files/figure-html/inspect_explore_data_1-1.png) 

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

script_df <- rbind(script_df, 
    data.frame(chunk_label="manage_missing_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
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

script_df <- rbind(script_df, 
    data.frame(chunk_label="encode_retype_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
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

script_df <- rbind(script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
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

script_df <- rbind(script_df, 
                   data.frame(chunk_label="select_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
## 7      select_features                4                0
```

## Step `4`: select features

```r
print(glb_feats_df <- 
    myselect_features(glb_entity_df, glb_exclude_vars_as_features, glb_predct_var))
```

```
##              id      cor.y  cor.y.abs
## Unconst Unconst 0.06627144 0.06627144
## Term       Term 0.03032304 0.03032304
```

```r
script_df <- rbind(script_df, 
    data.frame(chunk_label="remove_correlated_features", 
        chunk_step_major=max(script_df$chunk_step_major),
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))        
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
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
script_df <- rbind(script_df, 
                   data.frame(chunk_label="run_models", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
## 9                 run_models                5                0
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
if (glb_is_classification) {
    #   Logit Regression:
    myrun_mdl_fn <- myrun_mdl_glm
}    
    
# Add dummy model - random variable
#   Potential Enhancements:
#       For classifiers, it shd generate proba/outcomes that mimics the freq
#           distribution of glb_predct_var values; Right now it always generates
#           0 (most frequent ?)
ret_lst <- myrun_mdl_fn(indep_vars_vctr=".rnorm",
                        lcl_predct_var=glb_predct_var, 
                        lcl_predct_var_name=glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
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
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -1.297  -1.254   1.081   1.100   1.143  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)  
## (Intercept)  0.18489    0.10122   1.827   0.0678 .
## .rnorm      -0.03377    0.09766  -0.346   0.7295  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 545.70  on 395  degrees of freedom
## Residual deviance: 545.58  on 394  degrees of freedom
## AIC: 549.58
## 
## Number of Fisher Scoring iterations: 3
```

```r
glb_dmy_mdl <- ret_lst[["model"]]

# Highest cor.y
ret_lst <- myrun_mdl_fn(indep_vars_vctr=max_cor_y_x_var,
                        lcl_predct_var=glb_predct_var, 
                        lcl_predct_var_name=glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.3662  -1.2248   0.9997   1.1307   1.1307  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)   0.1109     0.1143   0.970    0.332
## Unconst       0.3228     0.2453   1.316    0.188
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 545.70  on 395  degrees of freedom
## Residual deviance: 543.94  on 394  degrees of freedom
## AIC: 547.94
## 
## Number of Fisher Scoring iterations: 4
```

```r
# Enhance Highest cor.y model with additions of interaction terms that were 
#   dropped due to high correlations
if (nrow(subset(glb_feats_df, is.na(cor.low))) > 0)
    ret_lst <- myrun_mdl_fn(indep_vars_vctr=c(max_cor_y_x_var, 
        paste(max_cor_y_x_var, 
              subset(glb_feats_df, is.na(cor.low))[, "id"], sep=":")),
                        glb_predct_var, glb_predct_var_name,
                            fit_df=glb_entity_df, OOB_df=glb_newent_df)    

# Low correlated X
ret_lst <- myrun_mdl_fn(indep_vars_vctr=subset(glb_feats_df, 
                                               cor.low == 1)[, "id"],
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.4163  -1.2236   0.9895   1.1199   1.1682  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)
## (Intercept) -57.42477   95.66601  -0.600    0.548
## Unconst       0.32271    0.24538   1.315    0.188
## Term          0.02881    0.04790   0.601    0.548
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 545.70  on 395  degrees of freedom
## Residual deviance: 543.58  on 393  degrees of freedom
## AIC: 549.58
## 
## Number of Fisher Scoring iterations: 4
```

```r
# All X that is not user excluded
ret_lst <- myrun_mdl_fn(indep_vars_vctr=setdiff(names(glb_entity_df), 
    union(glb_predct_var, glb_exclude_vars_as_features)),
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.3586  -0.9189   0.3353   0.8482   2.2275  
## 
## Coefficients:
##                                             Estimate Std. Error z value
## (Intercept)                                -47.91172  117.74191  -0.407
## Term                                         0.02433    0.05896   0.413
## Unconst                                     -0.17555    0.35551  -0.494
## Circuit.fctr7th                             -0.65021    0.52433  -1.240
## Circuit.fctrFED                             -0.72046    0.58866  -1.224
## Circuit.fctrDC                              -0.76550    0.58557  -1.307
## Circuit.fctr11th                            -0.23589    0.48786  -0.484
## Circuit.fctr2nd                              0.93630    0.56962   1.644
## Circuit.fctr6th                              0.13244    0.55786   0.237
## Circuit.fctr3rd                             -0.90093    0.60570  -1.487
## Circuit.fctr10th                            -1.35086    0.58492  -2.309
## Circuit.fctr8th                             -0.56162    0.53060  -1.058
## Circuit.fctr5th                              0.68012    0.50960   1.335
## Circuit.fctr4th                              0.59484    0.58229   1.022
## Circuit.fctr1st                             -0.87873    1.09527  -0.802
## Issue.fctrJudicialPower                     -0.32049    0.41594  -0.771
## Issue.fctrCriminalProcedure                 -0.30717    0.53909  -0.570
## Issue.fctrDueProcess                         0.25310    0.58779   0.431
## Issue.fctrFederalismAndInterstateRelations   0.08384    0.67605   0.124
## Issue.fctrCivilRights                       -0.07564    0.52162  -0.145
## Issue.fctrFirstAmendment                    -0.73942    0.61249  -1.207
## Issue.fctrFederalTaxation                   -1.95641    1.08332  -1.806
## Issue.fctrUnions                            -1.01470    0.79575  -1.275
## Issue.fctrPrivacy                            2.12821    1.23021   1.730
## Issue.fctrAttorneys                         -0.21996    1.40648  -0.156
## Petitioner.fctrCITY                         -1.54739    0.90887  -1.703
## Petitioner.fctrEMPLOYEE                      0.72629    0.75986   0.956
## Petitioner.fctrAMERICAN.INDIAN              -1.30924    1.60433  -0.816
## Petitioner.fctrINJURED.PERSON                0.08859    1.05472   0.084
## Petitioner.fctrGOVERNMENT.OFFICIAL          -1.00691    0.62015  -1.624
## Petitioner.fctrOTHER                         0.05366    0.39947   0.134
## Petitioner.fctrSTATE                        -0.54022    0.62695  -0.862
## Petitioner.fctrUS                            0.11006    0.57317   0.192
## Petitioner.fctrCRIMINAL.DEFENDENT            0.67585    0.68002   0.994
## Petitioner.fctrEMPLOYER                     -0.64909    0.79034  -0.821
## Petitioner.fctrPOLITICIAN                   -0.59568    0.86220  -0.691
## Respondent.fctrCITY                         -0.86937    0.98730  -0.881
## Respondent.fctrCRIMINAL.DEFENDENT           -1.27037    0.63214  -2.010
## Respondent.fctrEMPLOYEE                     -0.58328    0.69649  -0.837
## Respondent.fctrEMPLOYER                      0.83259    1.19694   0.696
## Respondent.fctrAMERICAN.INDIAN               1.74125    0.98331   1.771
## Respondent.fctrINJURED.PERSON               -1.71043    0.84245  -2.030
## Respondent.fctrGOVERNMENT.OFFICIAL          -0.62522    0.81888  -0.764
## Respondent.fctrOTHER                        -0.49674    0.40026  -1.241
## Respondent.fctrPOLITICIAN                    0.08058    0.78787   0.102
## Respondent.fctrSTATE                         0.38060    0.69878   0.545
## Respondent.fctrUS                           -1.29052    0.59322  -2.175
## LowerCourt.fctrconser                        0.96186    0.30558   3.148
##                                            Pr(>|z|)   
## (Intercept)                                 0.68407   
## Term                                        0.67990   
## Unconst                                     0.62146   
## Circuit.fctr7th                             0.21495   
## Circuit.fctrFED                             0.22099   
## Circuit.fctrDC                              0.19112   
## Circuit.fctr11th                            0.62873   
## Circuit.fctr2nd                             0.10023   
## Circuit.fctr6th                             0.81234   
## Circuit.fctr3rd                             0.13690   
## Circuit.fctr10th                            0.02092 * 
## Circuit.fctr8th                             0.28984   
## Circuit.fctr5th                             0.18200   
## Circuit.fctr4th                             0.30699   
## Circuit.fctr1st                             0.42238   
## Issue.fctrJudicialPower                     0.44098   
## Issue.fctrCriminalProcedure                 0.56882   
## Issue.fctrDueProcess                        0.66677   
## Issue.fctrFederalismAndInterstateRelations  0.90130   
## Issue.fctrCivilRights                       0.88471   
## Issue.fctrFirstAmendment                    0.22734   
## Issue.fctrFederalTaxation                   0.07093 . 
## Issue.fctrUnions                            0.20226   
## Issue.fctrPrivacy                           0.08364 . 
## Issue.fctrAttorneys                         0.87573   
## Petitioner.fctrCITY                         0.08865 . 
## Petitioner.fctrEMPLOYEE                     0.33916   
## Petitioner.fctrAMERICAN.INDIAN              0.41446   
## Petitioner.fctrINJURED.PERSON               0.93306   
## Petitioner.fctrGOVERNMENT.OFFICIAL          0.10445   
## Petitioner.fctrOTHER                        0.89315   
## Petitioner.fctrSTATE                        0.38887   
## Petitioner.fctrUS                           0.84773   
## Petitioner.fctrCRIMINAL.DEFENDENT           0.32029   
## Petitioner.fctrEMPLOYER                     0.41149   
## Petitioner.fctrPOLITICIAN                   0.48964   
## Respondent.fctrCITY                         0.37856   
## Respondent.fctrCRIMINAL.DEFENDENT           0.04447 * 
## Respondent.fctrEMPLOYEE                     0.40234   
## Respondent.fctrEMPLOYER                     0.48668   
## Respondent.fctrAMERICAN.INDIAN              0.07659 . 
## Respondent.fctrINJURED.PERSON               0.04232 * 
## Respondent.fctrGOVERNMENT.OFFICIAL          0.44516   
## Respondent.fctrOTHER                        0.21459   
## Respondent.fctrPOLITICIAN                   0.91853   
## Respondent.fctrSTATE                        0.58598   
## Respondent.fctrUS                           0.02960 * 
## LowerCourt.fctrconser                       0.00165 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 545.70  on 395  degrees of freedom
## Residual deviance: 428.54  on 348  degrees of freedom
## AIC: 524.54
## 
## Number of Fisher Scoring iterations: 5
```

```r
# User specified - easier to exclude features
ret_lst <- myrun_mdl_fn(indep_vars_vctr=setdiff(names(glb_entity_df), 
    union(union(glb_predct_var, glb_exclude_vars_as_features), 
          c("Term"))),
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.3832  -0.9186   0.3458   0.8470   2.2290  
## 
## Coefficients:
##                                            Estimate Std. Error z value
## (Intercept)                                 0.66858    0.55283   1.209
## Unconst                                    -0.18029    0.35504  -0.508
## Circuit.fctr7th                            -0.64115    0.52484  -1.222
## Circuit.fctrFED                            -0.70973    0.58851  -1.206
## Circuit.fctrDC                             -0.76663    0.58489  -1.311
## Circuit.fctr11th                           -0.22491    0.48744  -0.461
## Circuit.fctr2nd                             0.97359    0.56287   1.730
## Circuit.fctr6th                             0.14258    0.55659   0.256
## Circuit.fctr3rd                            -0.88092    0.60384  -1.459
## Circuit.fctr10th                           -1.34112    0.58353  -2.298
## Circuit.fctr8th                            -0.55667    0.53003  -1.050
## Circuit.fctr5th                             0.69556    0.50837   1.368
## Circuit.fctr4th                             0.61347    0.57922   1.059
## Circuit.fctr1st                            -0.88244    1.09766  -0.804
## Issue.fctrJudicialPower                    -0.32201    0.41573  -0.775
## Issue.fctrCriminalProcedure                -0.29264    0.53779  -0.544
## Issue.fctrDueProcess                        0.27264    0.58510   0.466
## Issue.fctrFederalismAndInterstateRelations  0.06834    0.67644   0.101
## Issue.fctrCivilRights                      -0.05890    0.51969  -0.113
## Issue.fctrFirstAmendment                   -0.71501    0.60953  -1.173
## Issue.fctrFederalTaxation                  -1.98079    1.08383  -1.828
## Issue.fctrUnions                           -0.99795    0.79350  -1.258
## Issue.fctrPrivacy                           2.18674    1.22350   1.787
## Issue.fctrAttorneys                        -0.21192    1.39093  -0.152
## Petitioner.fctrCITY                        -1.55222    0.90816  -1.709
## Petitioner.fctrEMPLOYEE                     0.73154    0.75988   0.963
## Petitioner.fctrAMERICAN.INDIAN             -1.36136    1.60051  -0.851
## Petitioner.fctrINJURED.PERSON               0.10632    1.05754   0.101
## Petitioner.fctrGOVERNMENT.OFFICIAL         -1.00998    0.62139  -1.625
## Petitioner.fctrOTHER                        0.04596    0.39889   0.115
## Petitioner.fctrSTATE                       -0.54067    0.62697  -0.862
## Petitioner.fctrUS                           0.09438    0.57188   0.165
## Petitioner.fctrCRIMINAL.DEFENDENT           0.65127    0.67669   0.962
## Petitioner.fctrEMPLOYER                    -0.67638    0.78771  -0.859
## Petitioner.fctrPOLITICIAN                  -0.58921    0.86124  -0.684
## Respondent.fctrCITY                        -0.89541    0.98709  -0.907
## Respondent.fctrCRIMINAL.DEFENDENT          -1.27049    0.63187  -2.011
## Respondent.fctrEMPLOYEE                    -0.58380    0.69677  -0.838
## Respondent.fctrEMPLOYER                     0.81539    1.19430   0.683
## Respondent.fctrAMERICAN.INDIAN              1.73754    0.98272   1.768
## Respondent.fctrINJURED.PERSON              -1.72998    0.84075  -2.058
## Respondent.fctrGOVERNMENT.OFFICIAL         -0.64414    0.81763  -0.788
## Respondent.fctrOTHER                       -0.51043    0.39896  -1.279
## Respondent.fctrPOLITICIAN                   0.06309    0.78797   0.080
## Respondent.fctrSTATE                        0.36823    0.69845   0.527
## Respondent.fctrUS                          -1.29002    0.59345  -2.174
## LowerCourt.fctrconser                       0.95835    0.30572   3.135
##                                            Pr(>|z|)   
## (Intercept)                                 0.22652   
## Unconst                                     0.61159   
## Circuit.fctr7th                             0.22186   
## Circuit.fctrFED                             0.22783   
## Circuit.fctrDC                              0.18995   
## Circuit.fctr11th                            0.64450   
## Circuit.fctr2nd                             0.08369 . 
## Circuit.fctr6th                             0.79782   
## Circuit.fctr3rd                             0.14460   
## Circuit.fctr10th                            0.02154 * 
## Circuit.fctr8th                             0.29360   
## Circuit.fctr5th                             0.17125   
## Circuit.fctr4th                             0.28954   
## Circuit.fctr1st                             0.42144   
## Issue.fctrJudicialPower                     0.43860   
## Issue.fctrCriminalProcedure                 0.58634   
## Issue.fctrDueProcess                        0.64124   
## Issue.fctrFederalismAndInterstateRelations  0.91952   
## Issue.fctrCivilRights                       0.90977   
## Issue.fctrFirstAmendment                    0.24078   
## Issue.fctrFederalTaxation                   0.06761 . 
## Issue.fctrUnions                            0.20852   
## Issue.fctrPrivacy                           0.07389 . 
## Issue.fctrAttorneys                         0.87891   
## Petitioner.fctrCITY                         0.08742 . 
## Petitioner.fctrEMPLOYEE                     0.33570   
## Petitioner.fctrAMERICAN.INDIAN              0.39500   
## Petitioner.fctrINJURED.PERSON               0.91992   
## Petitioner.fctrGOVERNMENT.OFFICIAL          0.10409   
## Petitioner.fctrOTHER                        0.90828   
## Petitioner.fctrSTATE                        0.38849   
## Petitioner.fctrUS                           0.86891   
## Petitioner.fctrCRIMINAL.DEFENDENT           0.33583   
## Petitioner.fctrEMPLOYER                     0.39052   
## Petitioner.fctrPOLITICIAN                   0.49389   
## Respondent.fctrCITY                         0.36434   
## Respondent.fctrCRIMINAL.DEFENDENT           0.04436 * 
## Respondent.fctrEMPLOYEE                     0.40211   
## Respondent.fctrEMPLOYER                     0.49478   
## Respondent.fctrAMERICAN.INDIAN              0.07705 . 
## Respondent.fctrINJURED.PERSON               0.03962 * 
## Respondent.fctrGOVERNMENT.OFFICIAL          0.43080   
## Respondent.fctrOTHER                        0.20075   
## Respondent.fctrPOLITICIAN                   0.93618   
## Respondent.fctrSTATE                        0.59805   
## Respondent.fctrUS                           0.02972 * 
## LowerCourt.fctrconser                       0.00172 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 545.70  on 395  degrees of freedom
## Residual deviance: 428.71  on 349  degrees of freedom
## AIC: 522.71
## 
## Number of Fisher Scoring iterations: 5
```

```r
glb_sel_mdl <- ret_lst[["model"]]

# User specified - easier to include features
# ret_lst <- myrun_mdl_fn(indep_vars_vctr=c("<feat1_name>", "<feat2_name>"),
#                         glb_predct_var, glb_predct_var_name,
#                         fit_df=glb_entity_df, OOB_df=glb_newent_df)

# User specified bivariate models
# for (feat in setdiff(names(glb_entity_df), 
#                      union(glb_predct_var, glb_exclude_vars_as_features))) {
#     ret_lst <- myrun_mdl_fn(indep_vars_vctr=feat,
#                             glb_predct_var, glb_predct_var_name,
#                             fit_df=glb_entity_df, OOB_df=glb_newent_df)
#     if (feat == "<feat_name>")
#         glb_sel_mdl <- glb_mdl
# }    

# User specified combinatorial models
# combn_mtrx <- combn(c("<feat1_name>", "<feat2_name>", "<featn_name>"), <num_feats_to_choose>)
# for (combn_ix in 1:ncol(combn_mtrx))
#     #print(indep_vars_vctr <- combn_mtrx[, combn_ix])
#     ret_lst <- myrun_mdl_fn(indep_vars_vctr=combn_mtrx[, combn_ix],
#                             glb_predct_var, glb_predct_var_name,
#                             fit_df=glb_entity_df, OOB_df=glb_newent_df)

# Simplify a model
# fit_df <- glb_entity_df; glb_mdl <- step(<complex>_mdl)

plot_models_df <- mutate(glb_models_df, feats.label=substr(feats, 1, 20))
if (glb_is_regression) {
    print(orderBy(~ -R.sq.OOB -Adj.R.sq.fit, glb_models_df))
    stop("glb_sel_mdl not selected")
    print(myplot_scatter(plot_models_df, "Adj.R.sq.fit", "R.sq.OOB") + 
          geom_text(aes(label=feats.label), data=plot_models_df, color="NavyBlue", 
                    size=3.5, angle=45))
}    

if (glb_is_classification) {
    # Lower AIC is better
    print(tmp_models_df <- orderBy(~ -auc.OOB +AIC.fit, glb_models_df))
    print("Selected model:glb_sel_mdl:")
#   glb_sel_mdl <- glb_models_lst[[as.numeric(rownames(tmp_models_df)[1])]]
    print(summary(glb_sel_mdl))
    plot_models_df[, "inv.AIC.fit"] <- 1.0 / plot_models_df[, "AIC.fit"] 
    print(myplot_scatter(plot_models_df, "inv.AIC.fit", "auc.OOB") + 
          geom_text(aes(label=feats.label), data=plot_models_df, color="NavyBlue", 
                    size=3.5, angle=45))
}
```

```
##                                                                                        feats
## 4 Term, Unconst, Circuit.fctr, Issue.fctr, Petitioner.fctr, Respondent.fctr, LowerCourt.fctr
## 5       Unconst, Circuit.fctr, Issue.fctr, Petitioner.fctr, Respondent.fctr, LowerCourt.fctr
## 2                                                                                    Unconst
## 3                                                                              Unconst, Term
## 1                                                                                     .rnorm
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit   auc.fit
## 4   396       NA       NA           NA 2394.341      NA 524.5440 0.7945602
## 5   396       NA       NA           NA 2419.121      NA 522.7144 0.7948431
## 2   396       NA       NA           NA 1604.777      NA 547.9432 0.5277778
## 3   396       NA       NA           NA 1606.558      NA 549.5810 0.5427083
## 1   396       NA       NA           NA 1597.666      NA 549.5757 0.5175926
##     auc.OOB
## 4 0.7304846
## 5 0.7267840
## 2 0.5842759
## 3 0.5704511
## 1 0.4197738
## [1] "Selected model:glb_sel_mdl:"
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.3832  -0.9186   0.3458   0.8470   2.2290  
## 
## Coefficients:
##                                            Estimate Std. Error z value
## (Intercept)                                 0.66858    0.55283   1.209
## Unconst                                    -0.18029    0.35504  -0.508
## Circuit.fctr7th                            -0.64115    0.52484  -1.222
## Circuit.fctrFED                            -0.70973    0.58851  -1.206
## Circuit.fctrDC                             -0.76663    0.58489  -1.311
## Circuit.fctr11th                           -0.22491    0.48744  -0.461
## Circuit.fctr2nd                             0.97359    0.56287   1.730
## Circuit.fctr6th                             0.14258    0.55659   0.256
## Circuit.fctr3rd                            -0.88092    0.60384  -1.459
## Circuit.fctr10th                           -1.34112    0.58353  -2.298
## Circuit.fctr8th                            -0.55667    0.53003  -1.050
## Circuit.fctr5th                             0.69556    0.50837   1.368
## Circuit.fctr4th                             0.61347    0.57922   1.059
## Circuit.fctr1st                            -0.88244    1.09766  -0.804
## Issue.fctrJudicialPower                    -0.32201    0.41573  -0.775
## Issue.fctrCriminalProcedure                -0.29264    0.53779  -0.544
## Issue.fctrDueProcess                        0.27264    0.58510   0.466
## Issue.fctrFederalismAndInterstateRelations  0.06834    0.67644   0.101
## Issue.fctrCivilRights                      -0.05890    0.51969  -0.113
## Issue.fctrFirstAmendment                   -0.71501    0.60953  -1.173
## Issue.fctrFederalTaxation                  -1.98079    1.08383  -1.828
## Issue.fctrUnions                           -0.99795    0.79350  -1.258
## Issue.fctrPrivacy                           2.18674    1.22350   1.787
## Issue.fctrAttorneys                        -0.21192    1.39093  -0.152
## Petitioner.fctrCITY                        -1.55222    0.90816  -1.709
## Petitioner.fctrEMPLOYEE                     0.73154    0.75988   0.963
## Petitioner.fctrAMERICAN.INDIAN             -1.36136    1.60051  -0.851
## Petitioner.fctrINJURED.PERSON               0.10632    1.05754   0.101
## Petitioner.fctrGOVERNMENT.OFFICIAL         -1.00998    0.62139  -1.625
## Petitioner.fctrOTHER                        0.04596    0.39889   0.115
## Petitioner.fctrSTATE                       -0.54067    0.62697  -0.862
## Petitioner.fctrUS                           0.09438    0.57188   0.165
## Petitioner.fctrCRIMINAL.DEFENDENT           0.65127    0.67669   0.962
## Petitioner.fctrEMPLOYER                    -0.67638    0.78771  -0.859
## Petitioner.fctrPOLITICIAN                  -0.58921    0.86124  -0.684
## Respondent.fctrCITY                        -0.89541    0.98709  -0.907
## Respondent.fctrCRIMINAL.DEFENDENT          -1.27049    0.63187  -2.011
## Respondent.fctrEMPLOYEE                    -0.58380    0.69677  -0.838
## Respondent.fctrEMPLOYER                     0.81539    1.19430   0.683
## Respondent.fctrAMERICAN.INDIAN              1.73754    0.98272   1.768
## Respondent.fctrINJURED.PERSON              -1.72998    0.84075  -2.058
## Respondent.fctrGOVERNMENT.OFFICIAL         -0.64414    0.81763  -0.788
## Respondent.fctrOTHER                       -0.51043    0.39896  -1.279
## Respondent.fctrPOLITICIAN                   0.06309    0.78797   0.080
## Respondent.fctrSTATE                        0.36823    0.69845   0.527
## Respondent.fctrUS                          -1.29002    0.59345  -2.174
## LowerCourt.fctrconser                       0.95835    0.30572   3.135
##                                            Pr(>|z|)   
## (Intercept)                                 0.22652   
## Unconst                                     0.61159   
## Circuit.fctr7th                             0.22186   
## Circuit.fctrFED                             0.22783   
## Circuit.fctrDC                              0.18995   
## Circuit.fctr11th                            0.64450   
## Circuit.fctr2nd                             0.08369 . 
## Circuit.fctr6th                             0.79782   
## Circuit.fctr3rd                             0.14460   
## Circuit.fctr10th                            0.02154 * 
## Circuit.fctr8th                             0.29360   
## Circuit.fctr5th                             0.17125   
## Circuit.fctr4th                             0.28954   
## Circuit.fctr1st                             0.42144   
## Issue.fctrJudicialPower                     0.43860   
## Issue.fctrCriminalProcedure                 0.58634   
## Issue.fctrDueProcess                        0.64124   
## Issue.fctrFederalismAndInterstateRelations  0.91952   
## Issue.fctrCivilRights                       0.90977   
## Issue.fctrFirstAmendment                    0.24078   
## Issue.fctrFederalTaxation                   0.06761 . 
## Issue.fctrUnions                            0.20852   
## Issue.fctrPrivacy                           0.07389 . 
## Issue.fctrAttorneys                         0.87891   
## Petitioner.fctrCITY                         0.08742 . 
## Petitioner.fctrEMPLOYEE                     0.33570   
## Petitioner.fctrAMERICAN.INDIAN              0.39500   
## Petitioner.fctrINJURED.PERSON               0.91992   
## Petitioner.fctrGOVERNMENT.OFFICIAL          0.10409   
## Petitioner.fctrOTHER                        0.90828   
## Petitioner.fctrSTATE                        0.38849   
## Petitioner.fctrUS                           0.86891   
## Petitioner.fctrCRIMINAL.DEFENDENT           0.33583   
## Petitioner.fctrEMPLOYER                     0.39052   
## Petitioner.fctrPOLITICIAN                   0.49389   
## Respondent.fctrCITY                         0.36434   
## Respondent.fctrCRIMINAL.DEFENDENT           0.04436 * 
## Respondent.fctrEMPLOYEE                     0.40211   
## Respondent.fctrEMPLOYER                     0.49478   
## Respondent.fctrAMERICAN.INDIAN              0.07705 . 
## Respondent.fctrINJURED.PERSON               0.03962 * 
## Respondent.fctrGOVERNMENT.OFFICIAL          0.43080   
## Respondent.fctrOTHER                        0.20075   
## Respondent.fctrPOLITICIAN                   0.93618   
## Respondent.fctrSTATE                        0.59805   
## Respondent.fctrUS                           0.02972 * 
## LowerCourt.fctrconser                       0.00172 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 545.70  on 395  degrees of freedom
## Residual deviance: 428.71  on 349  degrees of freedom
## AIC: 522.71
## 
## Number of Fisher Scoring iterations: 5
```

![](stevens_glm_files/figure-html/run_models-1.png) 

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="fit_training.all", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
```

## Step `6`: fit training.all

```r
print(mdl_feats_df <- myextract_mdl_feats(lcl_sel_mdl=glb_sel_mdl, 
                                          lcl_entity_df=glb_entity_df))
```

```
##                              id        Pr.z
## LowerCourt.fctr Respondent.fctr 0.001720312
## Circuit.fctr    LowerCourt.fctr 0.021544640
## Respondent.fctr Petitioner.fctr 0.029722631
## Issue.fctr         Circuit.fctr 0.067610839
## Petitioner.fctr      Issue.fctr 0.087416577
## Unconst                 Unconst 0.611592799
```

```r
if (glb_is_regression) {
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=mdl_feats_df$id,
                        glb_predct_var, glb_predct_var_name, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl    
#     print(glb_models_df[nrow(glb_models_df), ])
    glb_entity_df[, glb_predct_var_name] <- predict(glb_sel_mdl, newdata=glb_entity_df)
    print(myplot_scatter(glb_entity_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
    glb_entity_df[, paste0(glb_predct_var_name, ".err")] <- 
        abs(glb_entity_df[, glb_predct_var_name] - glb_entity_df[, glb_predct_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_predct_var_name, ".err"))), 
                       glb_entity_df)))                             
}    

if (glb_is_classification) {
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=mdl_feats_df$id,
                        glb_predct_var, glb_predct_var_name, fit_df=glb_entity_df)
    glb_sel_mdl <- ret_lst[["model"]]        
#     print(glb_models_df[nrow(glb_models_df), ])
    glb_entity_df[, paste0(glb_predct_var_name, ".proba")] <- 
        predict(glb_sel_mdl, newdata=glb_entity_df, type="response")

    require(ROCR)
    ROCRpred <- prediction(glb_entity_df[, paste0(glb_predct_var_name, ".proba")],
                           glb_entity_df[, glb_predct_var])
    ROCRperf <- performance(ROCRpred, "tpr", "fpr")
    plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2,1.7))
    
    thresholds_df <- data.frame(threshold=seq(0.0, 1.0, 0.1))
    thresholds_df$f.score <- sapply(1:nrow(thresholds_df), function(row_ix) 
        mycompute_classifier_f.score(glb_sel_mdl, glb_entity_df, 
                                     thresholds_df[row_ix, "threshold"], 
                                     glb_predct_var, glb_predct_var_name))
    print(thresholds_df)
    print(myplot_line(thresholds_df, "threshold", "f.score"))
    
    glb_clf_proba_threshold <- thresholds_df[which.max(thresholds_df$f.score), 
                                             "threshold"]
    # This should change to maximize f.score.OOB ???
    print(sprintf("Classifier Probability Threshold: %0.4f to maximize f.score.fit",
                  glb_clf_proba_threshold))

    glb_entity_df[, glb_predct_var_name] <- 
        (glb_entity_df[, paste0(glb_predct_var_name, ".proba")] >= 
             glb_clf_proba_threshold) * 1.0
    print(mycreate_xtab(glb_entity_df, c(glb_predct_var, glb_predct_var_name)))
    print(sprintf("f.score=%0.4f", 
        mycompute_classifier_f.score(glb_sel_mdl, glb_entity_df, 
                                     glb_clf_proba_threshold, 
                                     glb_predct_var, glb_predct_var_name)))    
}    
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.3832  -0.9186   0.3458   0.8470   2.2290  
## 
## Coefficients:
##                                            Estimate Std. Error z value
## (Intercept)                                 0.66858    0.55283   1.209
## Respondent.fctrCITY                        -0.89541    0.98709  -0.907
## Respondent.fctrCRIMINAL.DEFENDENT          -1.27049    0.63187  -2.011
## Respondent.fctrEMPLOYEE                    -0.58380    0.69677  -0.838
## Respondent.fctrEMPLOYER                     0.81539    1.19430   0.683
## Respondent.fctrAMERICAN.INDIAN              1.73754    0.98272   1.768
## Respondent.fctrINJURED.PERSON              -1.72998    0.84075  -2.058
## Respondent.fctrGOVERNMENT.OFFICIAL         -0.64414    0.81763  -0.788
## Respondent.fctrOTHER                       -0.51043    0.39896  -1.279
## Respondent.fctrPOLITICIAN                   0.06309    0.78797   0.080
## Respondent.fctrSTATE                        0.36823    0.69845   0.527
## Respondent.fctrUS                          -1.29002    0.59345  -2.174
## LowerCourt.fctrconser                       0.95835    0.30572   3.135
## Petitioner.fctrCITY                        -1.55222    0.90816  -1.709
## Petitioner.fctrEMPLOYEE                     0.73154    0.75988   0.963
## Petitioner.fctrAMERICAN.INDIAN             -1.36136    1.60051  -0.851
## Petitioner.fctrINJURED.PERSON               0.10632    1.05754   0.101
## Petitioner.fctrGOVERNMENT.OFFICIAL         -1.00998    0.62139  -1.625
## Petitioner.fctrOTHER                        0.04596    0.39889   0.115
## Petitioner.fctrSTATE                       -0.54067    0.62697  -0.862
## Petitioner.fctrUS                           0.09438    0.57188   0.165
## Petitioner.fctrCRIMINAL.DEFENDENT           0.65127    0.67669   0.962
## Petitioner.fctrEMPLOYER                    -0.67638    0.78771  -0.859
## Petitioner.fctrPOLITICIAN                  -0.58921    0.86124  -0.684
## Circuit.fctr7th                            -0.64115    0.52484  -1.222
## Circuit.fctrFED                            -0.70973    0.58851  -1.206
## Circuit.fctrDC                             -0.76663    0.58489  -1.311
## Circuit.fctr11th                           -0.22491    0.48744  -0.461
## Circuit.fctr2nd                             0.97359    0.56287   1.730
## Circuit.fctr6th                             0.14258    0.55659   0.256
## Circuit.fctr3rd                            -0.88092    0.60384  -1.459
## Circuit.fctr10th                           -1.34112    0.58353  -2.298
## Circuit.fctr8th                            -0.55667    0.53003  -1.050
## Circuit.fctr5th                             0.69556    0.50837   1.368
## Circuit.fctr4th                             0.61347    0.57922   1.059
## Circuit.fctr1st                            -0.88244    1.09766  -0.804
## Issue.fctrJudicialPower                    -0.32201    0.41573  -0.775
## Issue.fctrCriminalProcedure                -0.29264    0.53779  -0.544
## Issue.fctrDueProcess                        0.27264    0.58510   0.466
## Issue.fctrFederalismAndInterstateRelations  0.06834    0.67644   0.101
## Issue.fctrCivilRights                      -0.05890    0.51969  -0.113
## Issue.fctrFirstAmendment                   -0.71501    0.60953  -1.173
## Issue.fctrFederalTaxation                  -1.98079    1.08383  -1.828
## Issue.fctrUnions                           -0.99795    0.79350  -1.258
## Issue.fctrPrivacy                           2.18674    1.22350   1.787
## Issue.fctrAttorneys                        -0.21192    1.39093  -0.152
## Unconst                                    -0.18029    0.35504  -0.508
##                                            Pr(>|z|)   
## (Intercept)                                 0.22652   
## Respondent.fctrCITY                         0.36434   
## Respondent.fctrCRIMINAL.DEFENDENT           0.04436 * 
## Respondent.fctrEMPLOYEE                     0.40211   
## Respondent.fctrEMPLOYER                     0.49478   
## Respondent.fctrAMERICAN.INDIAN              0.07705 . 
## Respondent.fctrINJURED.PERSON               0.03962 * 
## Respondent.fctrGOVERNMENT.OFFICIAL          0.43080   
## Respondent.fctrOTHER                        0.20075   
## Respondent.fctrPOLITICIAN                   0.93618   
## Respondent.fctrSTATE                        0.59805   
## Respondent.fctrUS                           0.02972 * 
## LowerCourt.fctrconser                       0.00172 **
## Petitioner.fctrCITY                         0.08742 . 
## Petitioner.fctrEMPLOYEE                     0.33570   
## Petitioner.fctrAMERICAN.INDIAN              0.39500   
## Petitioner.fctrINJURED.PERSON               0.91992   
## Petitioner.fctrGOVERNMENT.OFFICIAL          0.10409   
## Petitioner.fctrOTHER                        0.90828   
## Petitioner.fctrSTATE                        0.38849   
## Petitioner.fctrUS                           0.86891   
## Petitioner.fctrCRIMINAL.DEFENDENT           0.33583   
## Petitioner.fctrEMPLOYER                     0.39052   
## Petitioner.fctrPOLITICIAN                   0.49389   
## Circuit.fctr7th                             0.22186   
## Circuit.fctrFED                             0.22783   
## Circuit.fctrDC                              0.18995   
## Circuit.fctr11th                            0.64450   
## Circuit.fctr2nd                             0.08369 . 
## Circuit.fctr6th                             0.79782   
## Circuit.fctr3rd                             0.14460   
## Circuit.fctr10th                            0.02154 * 
## Circuit.fctr8th                             0.29360   
## Circuit.fctr5th                             0.17125   
## Circuit.fctr4th                             0.28954   
## Circuit.fctr1st                             0.42144   
## Issue.fctrJudicialPower                     0.43860   
## Issue.fctrCriminalProcedure                 0.58634   
## Issue.fctrDueProcess                        0.64124   
## Issue.fctrFederalismAndInterstateRelations  0.91952   
## Issue.fctrCivilRights                       0.90977   
## Issue.fctrFirstAmendment                    0.24078   
## Issue.fctrFederalTaxation                   0.06761 . 
## Issue.fctrUnions                            0.20852   
## Issue.fctrPrivacy                           0.07389 . 
## Issue.fctrAttorneys                         0.87891   
## Unconst                                     0.61159   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 545.70  on 395  degrees of freedom
## Residual deviance: 428.71  on 349  degrees of freedom
## AIC: 522.71
## 
## Number of Fisher Scoring iterations: 5
```

![](stevens_glm_files/figure-html/fit_training.all-1.png) 

```
##    threshold   f.score
## 1        0.0 0.7058824
## 2        0.1 0.7166667
## 3        0.2 0.7508772
## 4        0.3 0.7709497
## 5        0.4 0.7510373
## 6        0.5 0.7361111
## 7        0.6 0.6976744
## 8        0.7 0.6442577
## 9        0.8 0.4884488
## 10       0.9 0.2357724
## 11       1.0 0.0000000
```

![](stevens_glm_files/figure-html/fit_training.all-2.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Reverse Reverse.predict.0 Reverse.predict.1
## 1       0                66               114
## 2       1                 9               207
## [1] "f.score=0.7709"
```

```r
print(glb_feats_df <- mymerge_feats_Pr.z(glb_feats_df, glb_sel_mdl, glb_entity_df))
```

```
##                id      cor.y  cor.y.abs cor.low        Pr.z
## 5 Respondent.fctr         NA         NA      NA 0.001720312
## 3 LowerCourt.fctr         NA         NA      NA 0.021544640
## 4 Petitioner.fctr         NA         NA      NA 0.029722631
## 1    Circuit.fctr         NA         NA      NA 0.067610839
## 2      Issue.fctr         NA         NA      NA 0.087416577
## 7         Unconst 0.06627144 0.06627144       1 0.611592799
## 6            Term 0.03032304 0.03032304       1          NA
```

```r
# Most of this code is used again in predict_newdata chunk
glb_analytics_diag_plots <- function(obs_df) {
    for (var in subset(glb_feats_df, Pr.z < 0.1)$id) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_predct_var, glb_predct_var_name))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", facet_colcol_name="variable"))
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
        if (nrow(plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)) == 0)
            warning("No coefficients in selected model are statistically significant")
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

![](stevens_glm_files/figure-html/fit_training.all-3.png) ![](stevens_glm_files/figure-html/fit_training.all-4.png) ![](stevens_glm_files/figure-html/fit_training.all-5.png) ![](stevens_glm_files/figure-html/fit_training.all-6.png) ![](stevens_glm_files/figure-html/fit_training.all-7.png) 

```
##  [1] Docket                   Term                    
##  [3] Circuit                  Issue                   
##  [5] Petitioner               Respondent              
##  [7] LowerCourt               Unconst                 
##  [9] Reverse                  Circuit.fctr            
## [11] Issue.fctr               Petitioner.fctr         
## [13] Respondent.fctr          LowerCourt.fctr         
## [15] .rnorm                   Reverse.predict.proba   
## [17] Reverse.predict          Reverse.fctr            
## [19] Reverse.predict.accurate .label                  
## <0 rows> (or 0-length row.names)
```

![](stevens_glm_files/figure-html/fit_training.all-8.png) 

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="predict_newdata", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
## 11            predict_newdata                7                0
```

## Step `7`: predict newdata

```r
if (glb_is_regression)
    glb_newent_df[, glb_predct_var_name] <- predict(glb_sel_mdl, 
                                        newdata=glb_newent_df, type="response")

if (glb_is_classification) {
    # Compute selected model predictions
    glb_newent_df[, paste0(glb_predct_var_name, ".proba")] <- 
        predict(glb_sel_mdl, newdata=glb_newent_df, type="response")
    glb_newent_df[, glb_predct_var_name] <- 
        (predict(glb_sel_mdl, newdata=glb_newent_df, type="response") >= 
            glb_clf_proba_threshold) * 1.0

    # Compute dummy model predictions
    glb_newent_df[, paste0(glb_predct_var, ".preddmy.proba")] <- 
        predict(glb_dmy_mdl, newdata=glb_newent_df, type="response")
    glb_newent_df[, paste0(glb_predct_var, ".preddmy")] <- 
        (predict(glb_dmy_mdl, newdata=glb_newent_df, type="response") >= 
            glb_clf_proba_threshold) * 1.0
}
    
myprint_df(glb_newent_df[, c(glb_id_vars, glb_predct_var, glb_predct_var_name)])
```

```
##     Docket Reverse Reverse.predict
## 1  93-1408       1               1
## 3  93-1612       1               1
## 4   94-623       1               1
## 6   95-129       0               1
## 8  96-1768       1               1
## 21 97-1704       0               1
##      Docket Reverse Reverse.predict
## 47  96-1470       1               1
## 112  98-223       0               0
## 244 95-1225       1               0
## 274  99-699       0               0
## 434 95-1608       0               1
## 441  94-558       1               1
##      Docket Reverse Reverse.predict
## 545  00-507       0               0
## 546 95-1478       0               0
## 551  00-157       0               0
## 552 00-1567       0               0
## 556  95-173       1               1
## 558 96-6839       1               1
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
## [1] "auc=0.7268"
## [1] "probability threshold=0.3000"
##   Reverse Reverse.predict.0 Reverse.predict.1
## 1       0                28                49
## 2       1                 7                86
## [1] "f.score.sel=0.7544"
## [1] "sensitivity=0.9247"
## [1] "specificity=0.3636"
## [1] "accuracy=0.6706"
##   Reverse Reverse.preddmy.1
## 1       0                77
## 2       1                93
## [1] "f.score.dmy=0.7072"
```

```r
glb_analytics_diag_plots(glb_newent_df)
```

![](stevens_glm_files/figure-html/predict_newdata-1.png) ![](stevens_glm_files/figure-html/predict_newdata-2.png) ![](stevens_glm_files/figure-html/predict_newdata-3.png) ![](stevens_glm_files/figure-html/predict_newdata-4.png) ![](stevens_glm_files/figure-html/predict_newdata-5.png) 

```
##  [1] Docket                   Term                    
##  [3] Circuit                  Issue                   
##  [5] Petitioner               Respondent              
##  [7] LowerCourt               Unconst                 
##  [9] Reverse                  Circuit.fctr            
## [11] Issue.fctr               Petitioner.fctr         
## [13] Respondent.fctr          LowerCourt.fctr         
## [15] .rnorm                   Reverse.predict.proba   
## [17] Reverse.predict          Reverse.preddmy.proba   
## [19] Reverse.preddmy          Reverse.fctr            
## [21] Reverse.predict.accurate .label                  
## <0 rows> (or 0-length row.names)
```

![](stevens_glm_files/figure-html/predict_newdata-6.png) 

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 

```r
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
## R version 3.1.3 (2015-03-09)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.2 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] ROCR_1.0-6      gplots_2.16.0   reshape2_1.4.1  plyr_1.8.1     
## [5] caTools_1.17.1  doBy_4.5-13     survival_2.38-1 ggplot2_1.0.1  
## 
## loaded via a namespace (and not attached):
##  [1] bitops_1.0-6       colorspace_1.2-6   digest_0.6.8      
##  [4] evaluate_0.5.5     formatR_1.0        gdata_2.13.3      
##  [7] grid_3.1.3         gtable_0.1.2       gtools_3.4.1      
## [10] htmltools_0.2.6    KernSmooth_2.23-14 knitr_1.9         
## [13] labeling_0.3       lattice_0.20-30    MASS_7.3-39       
## [16] Matrix_1.1-5       munsell_0.4.2      proto_0.3-10      
## [19] Rcpp_0.11.5        rmarkdown_0.5.1    scales_0.2.4      
## [22] splines_3.1.3      stringr_0.6.2      tools_3.1.3       
## [25] yaml_2.1.13
```
