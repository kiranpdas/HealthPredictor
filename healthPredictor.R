# Read Data

dat0 <- read.table('data/data_0000_part_00',sep='|')
dat1 <- read.table('data/data_0001_part_00',sep='|')
dat2 <- read.table('data/data_0002_part_00',sep='|')
dat3 <- read.table('data/data_0003_part_00',sep='|')
df <- rbind(dat0,dat1,dat2,dat3)
head(df,2)

# Rename header using sample_data
sample <- read.csv('data/data_sample.csv')[1,]
sample$X <- NULL

names(df) <- names(sample)
df_bkp <- df

str(df)
# Find the count of missing values
missing <- sort(sapply(df,function(x) sum(is.na(x))/length(x)*100),decreasing=TRUE) 
missing


# Remove features with majority missing values , dates and other outcomes
df <- within(df,rm(nofloor_flag,lab_value_lipase,outpt_weight,lab_value_pco2,chpl_supp_oxygen,lab_value_calcium,lab_value_bilitot,lab_value_ph
                            ,rehab_flag,lab_value_egfr,lab_value_platelet,chpl_dbp,frailty_getup,frailty_patient_mobility,urine_output,ped_flag
                            ,flowv_dbp,resp_rate,supp_oxygen,hr,time_since_hospital,lab_value_sedrate,lab_value_amylase,lab_value_breaths,lab_value_po2,adt_room
                            ,lab_value_ast,chpl_temp,lab_value_phos,psych_flag,lab_value_bun,lab_value_potas,lab_value_hc03,frailty_fall_risk_score
                            ,frailty_patient_nutrition,chpl_spo2,flowv_temp,sbp,flowv_modrass,flowv_spo2,meds_drug_dose,pt_episode_id,unsched_xfer_to_icu
                            ,lab_value_ammonia,lab_value_base,lab_value_albumin,lab_value_alk,lab_value_calcion,lab_value_aptt,lab_value_nphils,flowv_weight
                            ,lab_value_creat,lab_value_sodium,lab_value_hemog,lab_value_glucose,frailty_pt_activity_level,v66_7,temp,dbp,flowv_modrass_abs,flowv_hr
                            ,flowv_height,patient_id,lab_value_troponin,lab_value_crp,lab_value_o2flow,lab_value_bilidir,lab_value_lactate,lab_value_alt
                            ,lab_value_inr,lab_value_magnes,lab_value_aniongap,lab_value_leuko,chpl_sbp,chpl_resp_rate,frailty_braden_skin_score
                            ,frailty_sensory_perception,chpl_hr,flowv_sbp,flowv_resp_rate,flowv_supp_oxygen,spo2,dialysis_drug_start,encounter_id
                            ,minutes_to_outcome1,meds_drug_name,dt_episode_start,dt_episode_end,birthdt,dschstan ,dt_record
                            ,dt_outcome,dt_admit,dt_discharge,outcome_8hr,outcome_4hr,outcome_24hr,adt_fromtable,adt_bed,rrt_code,lab_device ,
                            meds_drug_class,meds_drug_route,meds_dose_units,outcome_event,outcome_enc_event,outcome, outcome_enc, married
                            
                      )
             )
# Exclude records having Exclude = 1 and det_pt_eligible != "in"
df <- df[! df$EXCLUDE==1 ,]
df <- df[df$det_pt_eligible=="in",]
dim(df)

# Remove variables Exclude and det_pt_eligible
df <- within(df,rm(EXCLUDE,det_pt_eligible))

# Change type of factor variables from numeric to factor
df$male <- as.factor(df$male)
df$outcome_12hr <- as.factor(df$outcome_12hr)


str(df)
# Value distribution of factors
table(df$code_status)
table(df$combined_category)
table(df$race)   # 474 unknown
table(df$ethncity) # 474 unknown
table(df$dschgdsp)     
table(df$admsour)
table(df$surgery)
table(df$admtype)
table(df$marstat)
table(df$married)
table(df$male)
table(df$admit_source)
table(df$discharge)
table(df$episode_cnt)
table(df$agebracket)


#Correlation
cor(df$los,df$los_hours) # 0.86
table(df$ageyear,df$agebracket)
cor(df[,3:19])

#Remove correlated fields
df$los_hours <-  NULL
df$ageyear <- NULL

# Data Partition
set.seed(1234)
idx <- sample(2, nrow(df), replace = TRUE,prob=c(0.7,0.3))
train <- df[idx==1,]
test <- df[idx==2,]
prop.table(table(train$outcome_12hr))
prop.table(table(test$outcome_12hr))

#Sample Train
trainSample <- SMOTE(outcome_12hr~.,data=train,perc.over=100,perc.under=200)
#Build Model
        #Decision Tree
            library(rpart)
            library(rpart.plot)
            model_tree <- rpart(outcome_12hr~.,data=trainSample)
            prp(model_tree,varlen = 15)
            #Predict
            pred_tree=predict(model_tree,test,type = "class")
            #CM and Accuraacy
            confusionMatrix(pred_tree,test$outcome_12hr,positive = '1')
            #AUC
            pred_tree_n <- as.numeric(pred_tree)
            auc_tree <- roc(test$outcome_12hr, pred_tree_n)
            auc_tree
        #Random Forest
            library(randomForest)
            mod_rf <- randomForest(outcome_12hr~.,data=trainSample)
              summary(mod_rf)
              importance(mod_rf)
            #Predict
            pred1=predict(mod_rf,test)
            #CM and Accuraacy
            library(caret)
            cm1 <- confusionMatrix(pred1,test$outcome_12hr,positive = '1')
            cm1
            #AUC
            library(pROC)
            pred2<- as.numeric(pred1)
            auc1 <- roc(test$outcome_12hr, pred2)
            auc1
        

        





