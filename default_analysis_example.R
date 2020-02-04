#check if packages required here are installed and if not, download/install them.
list_packages <- c("tidyverse","scales","RColorBrewer","glmnet", "Information"
                   , "pROC", "caret", "mboost","ROCR", "xgboost", "car")

if(length(list_packages[which(!list_packages %in% library()$results[,"Package"])])>0){
  install.packages(list_packages[which(!list_packages %in% library()$results[,"Package"])])
}

#load neceessary packages.
library(tidyverse)
library(scales)
library(RColorBrewer)
library(glmnet)
library(pROC)
library(car)
library(caret)
library(mboost)
library(ROCR)
library(xgboost)
library(Information)

#set seed for in order to reproduce
set.seed(123)
#turn off scientific notation
options(scipen=999)

#import the data
name_data <- "default_data.csv"

#Combine the working directory and fileName to get the full filePath of the data file.
filepath_data <- paste(getwd(), name_data, sep='/')

#read in the data
df_main <- read.csv(file = filepath_data, header=TRUE, sep=',', stringsAsFactors = FALSE)

########################################################################################################################################################
############################################################################################################################
############################################################################################################################

#get rid of duplicate row number column
df_main$X <- NULL

#see the different data classes
str(df_main)

#what kind of values are in each variable?
summary(df_main)

#see what values are in the character fields (except the date field)
unique(df_main[,-1][,lapply(df_main[,-1], class) == "character"])

#some of the loan numbers are NA, along with a few of the loan attribute columns.
##it's not too many observations, so let's drop that data.
df_main <- na.omit(df_main)

#check if all the NAs are gone
sapply(df_main, function(col){sum(is.na(col))/length(col)})

#add relevant transformations of variables

#add a credit utilization column to know how much of the approved loan amount the borrower used (original_loan_amt/credit_limit)
df_main <- cbind(df_main, credit_util = df_main$original_loan_amt/df_main$credit_limit)



########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

#Insights

#different loan statuses
df_main %>%
  group_by(loan_status) %>%
  summarize(loans = n(), unique_loans = n_distinct(loan_no))

#so almost all loans start out as current and move to different statuses over time
##based on number of months on book. There are approx 50,000 unique loans.

#get the first month on book
df_main_mob_min <- 
  data.frame(
    df_main %>%
      group_by(loan_no) %>%
      filter(month_on_book == min(month_on_book))
  )


#let's see the distribution of loan statuses for the last month on book
#get the dataframe with only the last month on book for the loans.
df_main_mob_max <- 
  data.frame(
    df_main %>%
      group_by(loan_no) %>%
      filter(month_on_book == max(month_on_book))
  )


#observations by latest month on book and what share are in each loan status
df_main_mob_max %>%
  group_by(loan_status) %>%
  summarize(loans_num = n()) %>%
  mutate(share_total = loans_num/sum(loans_num))

#replace the infinite value of credit utilization in the min and max month on book
df_main_mob_min[which(is.infinite(df_main_mob_min$credit_util)==TRUE),"credit_util"] <- mean(df_main_mob_min[which(is.infinite(df_main_mob_min$credit_util)==FALSE),"credit_util"])
df_main_mob_max[which(is.infinite(df_main_mob_max$credit_util)==TRUE),"credit_util"] <- mean(df_main_mob_max[which(is.infinite(df_main_mob_max$credit_util)==FALSE),"credit_util"])


#Less than 1 percent of the loans are in 30DPD, 60DPD, and 90DPD each, so we can either discard
##those observations or consider them in default. The status 1 DPD is a little trickier given
##the account is not yet in delinquency, but the consumer is late paying (by fewer than 30 days.)

#Let's look at loans that were 30DPD at some point and what ended up being their status
##in the last month on book:
df_30DPD <- df_main[df_main$loan_no %in% 
                      df_main[df_main$loan_status == "30 DPD","loan_no"],]
#get the dataframe with only the last month on book for the loans.
df_30DPD_max <- 
  data.frame(
    df_30DPD %>%
      group_by(loan_no) %>%
      filter(month_on_book == max(month_on_book))
  )

#Summarize. It looks like only ~12% of the 30DPD loans end up as Current or Paid,
##so we can consider them a default.
summarize(group_by(df_30DPD_max,loan_status)
                 , obs_grp = n()
                 , share = n()/nrow(df_30DPD_max))

#let's do the same exercise for loans that were 1DPD at some point in the loan life.
df_1DPD <- df_main[df_main$loan_no %in% 
                    df_main[df_main$loan_status == "1 DPD","loan_no"],]

#get the dataframe with only the last month on book for the loans.
df_1DPD_max <- 
  data.frame(
    df_1DPD %>%
      group_by(loan_no) %>%
      filter(month_on_book == max(month_on_book))
  )

#Summarize.Since they're only 4% of the observations, let's drop them to not mix signals since
##their charge off rate of 27% is much higher than the total average of 7% but also 50% ended up
##Current or Paid, and the rest in some kind of default.
summarize(group_by(df_1DPD_max,loan_status)
                 , obs_grp = n()
                 , share = n()/nrow(df_1DPD_max))


#drop loans with last status of 1DPD from the last month on book data set
df_main_mob_max_2 <- df_main_mob_max[!(df_main_mob_max$loan_no %in% df_main_mob_max[df_main_mob_max$loan_status == "1 DPD","loan_no"]),]

#drop the loans from the month on book minimum data that ended up being 1DPD in their max month on book
df_main_mob_min_2 <- df_main_mob_min[df_main_mob_min$loan_no %in% df_main_mob_max_2$loan_no,]

#add a loan status binary column
df_main_mob_max_2 <- cbind(df_main_mob_max_2, isDefault = ifelse(!df_main_mob_max_2$loan_status %in% c("Current","Paid"),1,0))


#add the binary loan status column to df_main_mob_min.
df_main_mob_min_2 <- merge(df_main_mob_min_2, df_main_mob_max_2[,c("loan_no","isDefault")], by = "loan_no", all.x = TRUE)


#Let's look at loan_status vs month_on_book
grph_1 <- plyr::ddply(df_main,plyr::.(month_on_book, loan_status), summarize,
                percent = length(loan_no))

ggplot(grph_1,aes(x = month_on_book, y = percent, fill = as.factor(loan_status))) + 
  geom_bar(position = position_fill(), stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  labs(fill="Loan Status") +
  ggtitle("Month on Book vs Loan Status") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Set3")

#Let's look at loan_status vs loan_apr for last month on book. This will be more informative than
##combining across all months on book since loans can go into DPD and out of DPD and all the loans
##started as "Current" so that would be artifically higher since some of those loans are riskier
##(i.e. higher APR) and will end up defaulting.
grph_2 <- plyr::ddply(df_main_mob_max,plyr::.(loan_status), summarize,
                mean_apr = mean(loan_apr),
                mean_fico = mean(cra_fico),
                obs = length(loan_apr))

ggplot(grph_2,aes(x = reorder(loan_status,-mean_apr), y = mean_apr,fill = as.factor(loan_status))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  geom_text(aes(label = percent(round(mean_apr, 4))), vjust = 1.5, color = "black") +
  geom_text(aes(label = comma_format(accuracy=1)(obs)), vjust = 3, color = "black") +
  geom_text(aes(label = round(mean_fico,0)), vjust = 4.5, color = "black") + 
  xlab("Loan Status") + 
  labs(fill="Loan Status") +
  ggtitle("Loan Status vs Mean APR/FICO Last Month on Book") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Set1")

#see what the correlation is between binary loan default and other variables in the
##df_main_mob_min_2 dataframe so we don't have month_on_book and days_past_due, which are not
##known in month 0.
df_corr <- df_main_mob_min_2[lapply(df_main_mob_min_2,class) %in% c("numeric","integer")]

cor(x = as.matrix(df_corr[, !names(df_corr) == "isDefault"])
    , y = df_corr[, names(df_corr) == "isDefault"]
    , method = "pearson", use = "complete.obs")

#let's look at default rates vs cra_FICO, cra_num_credit_inquiries_last_6
##also credit_util and loan_apr, even though those are chosen by the customer after they
##pass the PD model, just for the sake of interest.

#look at a histrogram of FICO scores to group them better
hist(df_main_mob_min_2$cra_fico, labels=TRUE)

#look at the histrogram between 550 and 600
hist(df_main_mob_min_2$cra_fico[df_main_mob_min_2$cra_fico >= 550 &
                                df_main_mob_min_2$cra_fico < 600], labels=TRUE)

grph_3 <- plyr::ddply(df_main_mob_min_2[df_main_mob_min_2$cra_fico >= 580,]
                , plyr::.(fico_grp=cut(cra_fico, breaks = 
                                   c(seq(580,780,by=20),max(cra_fico)), include.lowest=TRUE, right=FALSE))
                , summarize, prop=mean(isDefault)
                , obs = length(isDefault))

ggplot(grph_3,aes(as.factor(fico_grp),y=prop, x=fico_grp)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "grey40") +
  geom_text(aes(label = percent(round(prop, 4))), vjust = 1.5, color = "black") +
  xlab("FICO Score")+ ylab("Default Rate") +
  scale_y_continuous(labels = percent_format()) +
  ggtitle("FICO Score vs Default Rates") +
  theme(plot.title = element_text(hjust = 0.5))


#look at a histrogram of cra_num_credit_inquiries_last_6 to group them better
hist(df_main_mob_min_2$cra_num_credit_inquiries_last_6, labels=TRUE)

#let's group more than 10 as a single group
grph_4 <- plyr::ddply(df_main_mob_min_2
                , plyr::.(cra_num_credit_inquiries_last_6 = 
                      cut(cra_num_credit_inquiries_last_6, breaks = c(seq(0,11,by=1),max(cra_num_credit_inquiries_last_6)), include.lowest=TRUE, right=FALSE))
                , summarize, prop=mean(isDefault)
                , obs = length(isDefault))

ggplot(grph_4,aes(as.factor(cra_num_credit_inquiries_last_6),y=prop, x=cra_num_credit_inquiries_last_6)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "grey40") +
  geom_text(aes(label = percent(round(prop, 4))), vjust = 1.5, color = "black") +
  xlab("Number Inquiries Last Six Months")+ ylab("Default Rate") +
  scale_y_continuous(labels = percent_format()) +
  ggtitle("Six Month Inquiries vs Default Rates") +
  theme(plot.title = element_text(hjust = 0.5))

#look at a histrogram of loan_terms to group them better
hist(df_main_mob_min_2$loan_terms, labels=TRUE)

#looks like 48 months only has a few observations, so drop it for this graph
grph_5 <- plyr::ddply(df_main_mob_min_2[df_main_mob_min_2$loan_terms < 48,]
                , plyr::.(loan_terms = loan_terms)
                , summarise, prop = mean(loan_apr)
                , mean_fico = mean(cra_fico)
                , obs = length(isDefault))

ggplot(grph_5,aes(y=prop, x=as.factor(loan_terms))) +
  geom_bar(stat = "identity", fill = "lightblue", color = "grey40") +
  geom_text(aes(label = percent(round(prop, 4))), vjust = 1.5, color = "black") +
  geom_text(aes(label = paste("FICO:",round(mean_fico,0))), vjust = 3, color = "black") +
  xlab("Loan Term")+ ylab("Mean APR") +
  scale_y_continuous(labels = percent_format()) +
  ggtitle("Loan Terms vs Mean APR and FICO") +
  theme(plot.title = element_text(hjust = 0.5))


#look at credit_util vs default rate and credit risk (FICO)
grph_6 <- plyr::ddply(df_main_mob_min_2
                , plyr::.(credit_util = cut(credit_util, breaks = c(seq(0,1,by=0.1),max(credit_util))))
                , summarise, prop = mean(isDefault)
                , mean_fico = mean(cra_fico)
                , obs = length(isDefault))

ggplot(grph_6,aes(y=prop, x=as.factor(credit_util))) +
  geom_bar(stat = "identity", fill = "lightblue", color = "grey40") +
  geom_text(aes(label = percent(round(prop, 4))), vjust = 1.5, color = "black") +
  geom_text(aes(label = round(mean_fico,0)), color = "black", vjust=3) +
  xlab("Credit Utilization")+ ylab("Default Rate") +
  scale_y_continuous(labels = percent_format()) +
  ggtitle("Credit Utilization vs Default Rate and FICO") +
  theme(plot.title = element_text(hjust = 0.5))



#check other features as well

#summarize each variable to see what its values are, if default rate varies, etc
feat_other <- c("cv_link_score","email_count","mobile_count",
                "bs_mismatch_ind", "bs_name_mismatch_ind", "vertical"
                , "cra_num_foreclosures", "cra_num_repos", "cra_num_derogs"
                , "cra_num_past_due", "cra_num_total_collections", "cra_num_credit_inquiries_last_6")

feat_other_q <- c(quo(cv_link_score),quo(email_count),quo(mobile_count),
                  quo(bs_mismatch_ind), quo(bs_name_mismatch_ind), quo(vertical)
                  , quo(cra_num_foreclosures), quo(cra_num_repos), quo(cra_num_derogs)
                  , quo(cra_num_past_due), quo(cra_num_total_collections), quo(cra_num_credit_inquiries_last_6))

for (i in seq_along(feat_other_q)) {
  df_main_mob_min_2 %>% 
    group_by(!!feat_other_q[[i]]) %>%
    dplyr::summarize(default_rate = mean(isDefault), obs = length(isDefault)) %>% 
    print()
}


#see if vertical is the same for min and max month on book data sets
sum(df_main_mob_max_2[order(df_main_mob_max_2$loan_no),"vertical"] != df_main_mob_min_2[order(df_main_mob_min_2$loan_no),"vertical"])

#see if email counts are the same for min and max month on book data sets
sum(df_main_mob_max_2[order(df_main_mob_max_2$loan_no),"email_count"] != df_main_mob_min_2[order(df_main_mob_min_2$loan_no),"email_count"])

#see if mobile counts are the same for min and max month on book data sets
sum(df_main_mob_max_2[order(df_main_mob_max_2$loan_no),"mobile_count"] != df_main_mob_min_2[order(df_main_mob_min_2$loan_no),"mobile_count"])

#see if cv link score is the same for min and max month on book data sets
sum(df_main_mob_max_2[order(df_main_mob_max_2$loan_no),"cv_link_score"] != df_main_mob_min_2[order(df_main_mob_min_2$loan_no),"cv_link_score"])


########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

#Model specification

#we will use the df_main_mob_min_2 data set for Probability of Default (PD) prediction
##since we are trying to predict default at the time of loan issuance.

#see which variables are available to us
names(df_main_mob_min_2)

#features we can eliminate:
##loan_no, loan_start_date (properties of the loan itself)
##original_loan_amt, loan_apr, loan_terms (part of the credit decision that should occur after someone is approved)
##month_on_book (don't know at the time of issuance)
##days_past_due/charge_off_dol/chrage_off_type (occurs after a loan goes into default)
##loan_status (occurs after issuance of loan)

#for mobile_count and email_count let's see if delinquent accounts got more emails and mobile calls than current ones

#first look at loans that went into delinquency at some point
df_main_mob_min_2[df_main_mob_min_2$loan_no %in% df_main[!df_main$loan_status %in% c("Paid","Current") & df_main$month_on_book > 0,"loan_no"],] %>%
  summarize(loans = n(), mean_email = mean(email_count), mean_mobile = mean(mobile_count))

#then look at loans that never went into delinquency
df_main_mob_min_2[df_main_mob_min_2$loan_no %in% df_main[df_main$loan_status %in% c("Paid","Current") & df_main$month_on_book > 0,"loan_no"],] %>%
  summarize(loans = n(), mean_email = mean(email_count), mean_mobile = mean(mobile_count))

#we can't know for sure, but it seems that these counts could represent the number of times a borrower was contacted by
##email or phone after loan issuance, so let's not use these features.


#use everything with the prefix "cra" since those are credit bureau variables and should be available at the time of underwriting
feat_cra <- regmatches(names(df_main_mob_min_2), regexpr('cra.*', names(df_main_mob_min_2)))

#look at a histogram of cv_link_score to see possible values
hist(df_main_mob_min_2$cv_link_score)

#now summarise again with different cuts
plyr::ddply(df_main_mob_min_2
      , plyr::.(cv_link_score=cut(cv_link_score, breaks = 
                         c(0,seq(600,750,by=50),max(cv_link_score)), include.lowest=TRUE, right=FALSE))
      , summarize, default_rate=mean(isDefault)
      , obs = length(isDefault))
#let's keep cv_link_score as well

#we can keep bs_mismatch_ind, bs_name_mismatch_ind

feat_all <- c(feat_cra, "cv_link_score", "bs_mismatch_ind", "bs_name_mismatch_ind")

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

#Test and Train Split/Feature Creation

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################


#Split into train and test sets

index_train <- createDataPartition(df_main_mob_min_2$loan_no, p = 0.66, list = FALSE)

df_model_train <- df_main_mob_min_2[index_train,]
df_model_test <- df_main_mob_min_2[-index_train,]

#Feature Creation

#given the features that we've selected, let's take a look at the information value (IV) and weight of evidence (WOE)
##within each feature and overall.
IV_train <- create_infotables(data = df_model_train[,names(df_model_train) %in% c("isDefault",feat_all,"vertical")], y = "isDefault")

#display results
print(IV_train)

########################################################################################################################################################
#Feature Creation
########################################################################################################################################################

#Feature: Vertical
##we have many options for converting this categorical features into a numeric one, including one-hot enconding.
##but let's make use of the weight of evidence method since our base model will likely be a logistic regression,
##and WOE is a log transformation of the odds of default to no default.

vec_vertical_1 <- NULL
vec_vertical_2 <- NULL

for(i in 1:length(str_split(IV_train$Tables$vertical[,1], ","))){
  vec_vertical_1 <- c(vec_vertical_1,str_split(IV_train$Tables$vertical[,1], ",")[[i]][1])
  vec_vertical_2 <- c(vec_vertical_2,str_split(IV_train$Tables$vertical[,1], ",")[[i]][2])
}


vec_vertical_1 <- gsub("\\[|\\]","",vec_vertical_1)
vec_vertical_2 <- gsub("\\[|\\]","",vec_vertical_2)

woe_cuts_vertical <- cbind.data.frame(low = vec_vertical_1, high = vec_vertical_2, stringsAsFactors = FALSE)

#append to the woe table
woe_table_vertical <- cbind.data.frame(IV_train$Tables$vertical, woe_cuts_vertical, stringsAsFactors = FALSE)

#now add the WOE values to the training and testing sets
vertical_woe <- NULL

for(i in 1:nrow(df_model_train)){
  for(j in nrow(woe_table_vertical):1){
   if(df_model_train[i,"vertical"] >= as.numeric(woe_table_vertical[j,"low"])){
      vertical_woe <- c(vertical_woe,woe_table_vertical[j,"WOE"])
      break
    }
  }
}

df_model_train <- cbind.data.frame(df_model_train, vertical_woe, stringsAsFactors = FALSE)

#test set using the training WOE
vertical_woe <- NULL

for(i in 1:nrow(df_model_test)){
  for(j in nrow(woe_table_vertical):1){
    if(df_model_test[i,"vertical"] >= as.numeric(woe_table_vertical[j,"low"])){
      vertical_woe <- c(vertical_woe,woe_table_vertical[j,"WOE"])
      break
    }
  }
}

df_model_test <- cbind.data.frame(df_model_test, vertical_woe, stringsAsFactors = FALSE)

remove(vertical_woe)

########################################################################################################################################################
########################################################################################################################################################

#Transform to WOE
#cra_oldest_trade, cra_num_total_collections, cra_num_credit_inquiries_last_6

#Feature: cra_oldest_trade

vec_cra_oldest_trade_1 <- NULL
vec_cra_oldest_trade_2 <- NULL

for(i in 1:length(str_split(IV_train$Tables$cra_oldest_trade[,1], ","))){
  vec_cra_oldest_trade_1 <- c(vec_cra_oldest_trade_1,str_split(IV_train$Tables$cra_oldest_trade[,1], ",")[[i]][1])
  vec_cra_oldest_trade_2 <- c(vec_cra_oldest_trade_2,str_split(IV_train$Tables$cra_oldest_trade[,1], ",")[[i]][2])
}


vec_cra_oldest_trade_1 <- gsub("\\[|\\]","",vec_cra_oldest_trade_1)
vec_cra_oldest_trade_2 <- gsub("\\[|\\]","",vec_cra_oldest_trade_2)

woe_cuts_cra_oldest_trade <- cbind.data.frame(low = vec_cra_oldest_trade_1, high = vec_cra_oldest_trade_2, stringsAsFactors = FALSE)

#append to the woe table
woe_table_cra_oldest_trade <- cbind.data.frame(IV_train$Tables$cra_oldest_trade, woe_cuts_cra_oldest_trade, stringsAsFactors = FALSE)

#now add the WOE values to the training and testing sets
cra_oldest_trade_woe <- NULL

for(i in 1:nrow(df_model_train)){
  for(j in nrow(woe_table_cra_oldest_trade):1){
    if(df_model_train[i,"cra_oldest_trade"] >= as.numeric(woe_table_cra_oldest_trade[j,"low"])){
      cra_oldest_trade_woe <- c(cra_oldest_trade_woe,woe_table_cra_oldest_trade[j,"WOE"])
      break
    }
  }
}

df_model_train <- cbind.data.frame(df_model_train, cra_oldest_trade_woe, stringsAsFactors = FALSE)

#test set using the training WOE
cra_oldest_trade_woe <- NULL

for(i in 1:nrow(df_model_test)){
  for(j in nrow(woe_table_cra_oldest_trade):1){
    if(df_model_test[i,"cra_oldest_trade"] >= as.numeric(woe_table_cra_oldest_trade[j,"low"])){
      cra_oldest_trade_woe <- c(cra_oldest_trade_woe,woe_table_cra_oldest_trade[j,"WOE"])
      break
    }
  }
}

df_model_test <- cbind.data.frame(df_model_test, cra_oldest_trade_woe, stringsAsFactors = FALSE)

remove(cra_oldest_trade_woe)

########################################################################################################################################################
########################################################################################################################################################

#transform to WOE
#cra_oldest_trade, cra_num_total_collections, cra_num_credit_inquiries_last_6

#Feature: cra_num_total_collections

vec_cra_num_total_collections_1 <- NULL
vec_cra_num_total_collections_2 <- NULL

for(i in 1:length(str_split(IV_train$Tables$cra_num_total_collections[,1], ","))){
  vec_cra_num_total_collections_1 <- c(vec_cra_num_total_collections_1,str_split(IV_train$Tables$cra_num_total_collections[,1], ",")[[i]][1])
  vec_cra_num_total_collections_2 <- c(vec_cra_num_total_collections_2,str_split(IV_train$Tables$cra_num_total_collections[,1], ",")[[i]][2])
}


vec_cra_num_total_collections_1 <- gsub("\\[|\\]","",vec_cra_num_total_collections_1)
vec_cra_num_total_collections_2 <- gsub("\\[|\\]","",vec_cra_num_total_collections_2)

woe_cuts_cra_num_total_collections <- cbind.data.frame(low = vec_cra_num_total_collections_1, high = vec_cra_num_total_collections_2, stringsAsFactors = FALSE)

#append to the woe table
woe_table_cra_num_total_collections <- cbind.data.frame(IV_train$Tables$cra_num_total_collections, woe_cuts_cra_num_total_collections, stringsAsFactors = FALSE)

#now add the WOE values to the training and testing sets
cra_num_total_collections_woe <- NULL

for(i in 1:nrow(df_model_train)){
  for(j in nrow(woe_table_cra_num_total_collections):1){
    if(df_model_train[i,"cra_num_total_collections"] >= as.numeric(woe_table_cra_num_total_collections[j,"low"])){
      cra_num_total_collections_woe <- c(cra_num_total_collections_woe,woe_table_cra_num_total_collections[j,"WOE"])
      break
    }
  }
}

df_model_train <- cbind.data.frame(df_model_train, cra_num_total_collections_woe, stringsAsFactors = FALSE)

#test set using the training WOE
cra_num_total_collections_woe <- NULL

for(i in 1:nrow(df_model_test)){
  for(j in nrow(woe_table_cra_num_total_collections):1){
    if(df_model_test[i,"cra_num_total_collections"] >= as.numeric(woe_table_cra_num_total_collections[j,"low"])){
      cra_num_total_collections_woe <- c(cra_num_total_collections_woe,woe_table_cra_num_total_collections[j,"WOE"])
      break
    }
  }
}

df_model_test <- cbind.data.frame(df_model_test, cra_num_total_collections_woe, stringsAsFactors = FALSE)

remove(cra_num_total_collections_woe)

########################################################################################################################################################
########################################################################################################################################################

########################################################################################################################################################
########################################################################################################################################################

#transform to WOE
#cra_oldest_trade, cra_num_total_collections, cra_num_credit_inquiries_last_6

#Feature: cra_num_credit_inquiries_last_6

vec_cra_num_credit_inquiries_last_6_1 <- NULL
vec_cra_num_credit_inquiries_last_6_2 <- NULL

for(i in 1:length(str_split(IV_train$Tables$cra_num_credit_inquiries_last_6[,1], ","))){
  vec_cra_num_credit_inquiries_last_6_1 <- c(vec_cra_num_credit_inquiries_last_6_1,str_split(IV_train$Tables$cra_num_credit_inquiries_last_6[,1], ",")[[i]][1])
  vec_cra_num_credit_inquiries_last_6_2 <- c(vec_cra_num_credit_inquiries_last_6_2,str_split(IV_train$Tables$cra_num_credit_inquiries_last_6[,1], ",")[[i]][2])
}


vec_cra_num_credit_inquiries_last_6_1 <- gsub("\\[|\\]","",vec_cra_num_credit_inquiries_last_6_1)
vec_cra_num_credit_inquiries_last_6_2 <- gsub("\\[|\\]","",vec_cra_num_credit_inquiries_last_6_2)

woe_cuts_cra_num_credit_inquiries_last_6 <- cbind.data.frame(low = vec_cra_num_credit_inquiries_last_6_1, high = vec_cra_num_credit_inquiries_last_6_2, stringsAsFactors = FALSE)

#append to the woe table
woe_table_cra_num_credit_inquiries_last_6 <- cbind.data.frame(IV_train$Tables$cra_num_credit_inquiries_last_6, woe_cuts_cra_num_credit_inquiries_last_6, stringsAsFactors = FALSE)

#now add the WOE values to the training and testing sets
cra_num_credit_inquiries_last_6_woe <- NULL

for(i in 1:nrow(df_model_train)){
  for(j in nrow(woe_table_cra_num_credit_inquiries_last_6):1){
    if(df_model_train[i,"cra_num_credit_inquiries_last_6"] >= as.numeric(woe_table_cra_num_credit_inquiries_last_6[j,"low"])){
      cra_num_credit_inquiries_last_6_woe <- c(cra_num_credit_inquiries_last_6_woe,woe_table_cra_num_credit_inquiries_last_6[j,"WOE"])
      break
    }
  }
}

df_model_train <- cbind.data.frame(df_model_train, cra_num_credit_inquiries_last_6_woe, stringsAsFactors = FALSE)

#test set using the training WOE
cra_num_credit_inquiries_last_6_woe <- NULL

for(i in 1:nrow(df_model_test)){
  for(j in nrow(woe_table_cra_num_credit_inquiries_last_6):1){
    if(df_model_test[i,"cra_num_credit_inquiries_last_6"] >= as.numeric(woe_table_cra_num_credit_inquiries_last_6[j,"low"])){
      cra_num_credit_inquiries_last_6_woe <- c(cra_num_credit_inquiries_last_6_woe,woe_table_cra_num_credit_inquiries_last_6[j,"WOE"])
      break
    }
  }
}

df_model_test <- cbind.data.frame(df_model_test, cra_num_credit_inquiries_last_6_woe, stringsAsFactors = FALSE)

remove(cra_num_credit_inquiries_last_6_woe)

########################################################################################################################################################
########################################################################################################################################################

#transform to binary
#cra_num_foreclosures, cra_num_repos, cra_num_derogs, cra_num_90_plus_12, cra_num_chrageoffs
##cra_num_past_due

feat_binary_trnsf <- c("cra_num_foreclosures", "cra_num_repos", "cra_num_derogs", "cra_num_90_plus_12", "cra_num_charge_offs", "cra_num_past_due")

for(col in feat_binary_trnsf){
  var_name <- NULL
  for(i in 1:nrow(df_model_train)){
    var_name <- c(var_name, ifelse(df_model_train[i,col] > 0, 1, 0))
  }
  assign(paste0(col,"_","binary"), var_name)
}

df_model_train <- cbind.data.frame(df_model_train
                                   , cra_num_foreclosures_binary, cra_num_repos_binary, cra_num_derogs_binary
                                   , cra_num_90_plus_12_binary, cra_num_charge_offs_binary, cra_num_past_due_binary
                                   , stringsAsFactors = FALSE)


remove(cra_num_foreclosures_binary, cra_num_repos_binary, cra_num_derogs_binary, cra_num_90_plus_12_binary, cra_num_charge_offs_binary, cra_num_past_due_binary)

#test set
for(col in feat_binary_trnsf){
  var_name <- NULL
  for(i in 1:nrow(df_model_test)){
    var_name <- c(var_name, ifelse(df_model_test[i,col] > 0, 1, 0))
  }
  assign(paste0(col,"_","binary"), var_name)
}

df_model_test <- cbind.data.frame(df_model_test
                                   , cra_num_foreclosures_binary, cra_num_repos_binary, cra_num_derogs_binary, cra_num_90_plus_12_binary
                                  , cra_num_charge_offs_binary, cra_num_past_due_binary
                                  , stringsAsFactors = FALSE)


remove(cra_num_foreclosures_binary, cra_num_repos_binary, cra_num_derogs_binary, cra_num_90_plus_12_binary, cra_num_charge_offs_binary, cra_num_past_due_binary)


#all features
feat_all <- c(feat_all, names(df_model_train)[grepl("_woe|_binary",names(df_model_train),ignore.case = TRUE)])

#transformed plus non-transformed features
feat_sub <- c(names(df_model_train)[grepl("_woe|_binary",names(df_model_train),ignore.case = TRUE)]
              , "cra_fico", "cv_link_score", "bs_mismatch_ind", "bs_name_mismatch_ind")

fmla_all <- as.formula(paste("isDefault ~ ", paste(feat_all, collapse = " + ")))

fmla_sub <- as.formula(paste("isDefault ~ ", paste(feat_sub, collapse = " + ")))

#choose which formula option
fmla_optn <- fmla_sub

#up weighted of positive examples (defaults)
wt_pos <- sum(df_model_train$isDefault == 0)/sum(df_model_train$isDefault == 1)

#neutral weighting of negative examples (non-defaults)
wt_neg <- 1

wt_mdl <- if_else(df_model_train$isDefault == 1, wt_pos, wt_neg)



###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################

#Boosted Logistic Regression

###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################

#run through glmboost to see if we can eliminate any of the indepedent variables

#specify the model control parameters
fit_control <- trainControl(method = "repeatedcv",
                            number = 5,
                            repeats = 10,
                            ## Estimate class probabilities
                            classProbs = TRUE,
                            ## Evaluate performance using 
                            ## the following function
                            summaryFunction = twoClassSummary)

#the glm boost function requires the response variable to be a factor
df_model_train[df_model_train$isDefault == 0,"isDefault"] <- "no"
df_model_train[df_model_train$isDefault == 1,"isDefault"] <- "yes"

mdl_glmBoost <- train(fmla_optn, data = df_model_train, method = "glmboost", metric = "ROC", trControl = fit_control
                      , tuneLength = 5, center = TRUE, family=Binomial(link = "logit")
                      , weights = wt_mdl)


pred_mdl_glmBoost <- as.vector(predict(mdl_glmBoost, newdata = df_model_test, type="prob")[,"yes"])


roc_mdl_glmBoost <- roc(df_model_test$isDefault, pred_mdl_glmBoost)

auc_mdl_glmBoost <- auc(roc_mdl_glmBoost)
auc_mdl_glmBoost
mdl_glmBoost$finalModel$coef()

#get the important features from the glm boost model
feat_bst <- names(mdl_glmBoost$finalModel$coef())
#sub out character
feat_bst <- gsub("`","",feat_bst)

fmla_optn_bst <- as.formula(paste("isDefault ~ ",paste(feat_bst[!grepl("intercept", feat_bst, ignore.case = TRUE)], collapse = " + ")))


#return response variable to 0 and 1
df_model_train[df_model_train$isDefault == "no","isDefault"] <- 0
df_model_train[df_model_train$isDefault == "yes","isDefault"] <- 1
df_model_train[,"isDefault"] <- as.numeric(df_model_train[,"isDefault"])

mdl_logit_bst <- glm(fmla_optn_bst, family = binomial(link="logit")
                     , data = df_model_train, weights = wt_mdl)

#variance inflation factor. Values above 4 indicate multi-collinearity
vif(mdl_logit_bst)

summary(mdl_logit_bst)

#predict probabilities
mdl_logit_bst_prob <- predict(mdl_logit_bst, newdata = df_model_test, type = "response")

#plot ROC curve
roc_logit_bst <- roc(predictor = as.numeric(mdl_logit_bst_prob), response = df_model_test$isDefault
               #, smooth=TRUE
               , ci = TRUE
               # arguments for plot
               , plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
               print.auc=TRUE, legacy.axes=TRUE, main = "Logistic Regression")
roc_logit_bst_ci <- ci.se(roc_logit_bst, conf.level = 0.95, boot.stratified = TRUE, boot.n = 2000)
plot(roc_logit_bst_ci, type = "shape", col = "lightblue")

#display AUC and 95% confidence interval
roc_logit_bst$auc
roc_logit_bst$ci


#calculate AUC again with different package to double check
mdl_logit_bst_pred <- prediction(mdl_logit_bst_prob, df_model_test$isDefault)

mdl_logit_bst_auc <- performance(mdl_logit_bst_pred, measure = "auc")

mdl_logit_bst_auc@y.values[[1]]


#since we're trying to predict defaults, we're most inerested in minimizing the instance where we falsely predict
##a borrower will not default (a False Negative).
##Statistics of interest:

#1. Sensitivity/Recall/True Positive Rate: TP/(TP + FN)
##We want to maximize this statistic since that will indicate our False Negatives are low.
#2. Miss Rate/False Negative Rate: FN/(TP+FN) or 1 - TPR
##This is just the complement of TPR and correspondingly, we should look to minimize this.
#3. Precision/Positive Predictive Power: TP/(TP + FP)
##This is also important to knowing if we incorrectly predict a lot of good borrowers as defaults (false positive)
##and so we want to maximize this statistic.

#for a give set of probability thresholds, find the various statistics in which we're interested.

prob_cutoffs <- seq(0,1,by = 0.05)

mtx_logit_bst_rates <- matrix(data = NA, nrow = length(prob_cutoffs), ncol = 6)

rownames(mtx_logit_bst_rates) <- prob_cutoffs
colnames(mtx_logit_bst_rates) <- c("TPR", "FNR", "PPV", "Obs", "FN", "FP")

#go through each cutoff and calcuclate the statistics of interest
for(c in 1:length(prob_cutoffs)){
  isDefault_logit_bst <- NULL
  for(p in 1:length(mdl_logit_bst_prob)){
    isDefault_logit_bst <- c(isDefault_logit_bst, ifelse(mdl_logit_bst_prob[p] > prob_cutoffs[c], 1, 0))
  }
  FN <- sum(df_model_test$isDefault == 1 & isDefault_logit_bst == 0)
  FP <- sum(df_model_test$isDefault == 0 & isDefault_logit_bst == 1)
  TN <- sum(df_model_test$isDefault == 0 & isDefault_logit_bst == 0)
  TP <- sum(df_model_test$isDefault == 1 & isDefault_logit_bst == 1)
  
  mtx_logit_bst_rates[c, "TPR"] <- TP/(TP + FN)
  mtx_logit_bst_rates[c, "FNR"] <- FN/(TP + FN)
  mtx_logit_bst_rates[c, "PPV"] <- TP/(TP + FP)
  mtx_logit_bst_rates[c, "Obs"] <- length(mdl_logit_bst_prob)
  mtx_logit_bst_rates[c, "FN"] <- FN
  mtx_logit_bst_rates[c, "FP"] <- FP
}


###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################

#XG Boost

###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################

#convert data to matrices for xgboost algorithm
mtx_mdl_train <- model.matrix(fmla_optn, data = df_model_train)
mtx_mdl_test <- model.matrix(fmla_optn, data = df_model_test)

#return response variable to 0 and 1
df_model_train[df_model_train$isDefault == "no","isDefault"] <- 0
df_model_train[df_model_train$isDefault == "yes","isDefault"] <- 1

target_train <- as.numeric(df_model_train$isDefault)
target_test <- as.numeric(df_model_test$isDefault)

mtx_xgb_train <- xgb.DMatrix(data = mtx_mdl_train, label = target_train, weight = wt_mdl)
mtx_xgb_test <- xgb.DMatrix(data = mtx_mdl_test, label = target_test)

#default parameters
xgb_params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.10, gamma=15, 
                   max_depth=4, min_child_weight=1, subsample=0.5, colsample_bytree=0.5
                   , eval_metric = "auc")

xgb_tune <- xgb.cv(params = xgb_params, data = mtx_xgb_train, nrounds = 2000, nfold = 5, showsd = T, stratified = T, 
                   print_every_n = 10, early_stop_round = 20, maximize = F)

#number of rounds that minimize average test error
which.max(xgb_tune$evaluation_log$test_auc_mean)

mdl_xgb <- xgboost(data = mtx_xgb_train, params = xgb_params, nrounds = which.max(xgb_tune$evaluation_log$test_auc_mean))

#variable importance
mtx_var_impt <- xgb.importance(feature_names = colnames(mtx_mdl_train), model = mdl_xgb)
xgb.plot.importance(importance_matrix = mtx_var_impt[1:20]
                    , main = "XG Boost Feature Importance", xpd = TRUE, cex = 0.6)

#predict probabilities
mdl_xgb_prob <- predict(mdl_xgb, newdata = mtx_xgb_test, outputmargin = FALSE)

#plot ROC curve
roc_xgb <- roc(predictor = as.numeric(mdl_xgb_prob), response = df_model_test$isDefault
               #, smooth=TRUE
               , ci = TRUE
               # arguments for plot
               , plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
               print.auc=TRUE, legacy.axes=TRUE, main = "XGBoost")
roc_xgb_ci <- ci.se(roc_xgb, conf.level = 0.95, boot.stratified = TRUE, boot.n = 2000)
plot(roc_xgb_ci, type = "shape", col = "lightblue")

#display AUC and 95% confidence interval
roc_xgb$auc
roc_xgb$ci


#calculate AUC again with different package to double check
mdl_xgb_pred <- prediction(mdl_xgb_prob, df_model_test$isDefault)

mdl_xgb_auc <- performance(mdl_xgb_pred, measure = "auc")

mdl_xgb_auc@y.values[[1]]


#since we're trying to predict defaults, we're most inerested in minimizing the instance where we falsely predict
##a borrower will not default (a False Negative).
##Statistics of interest:

#1. Sensitivity/Recall/True Positive Rate: TP/(TP + FN)
##We want to maximize this statistic since that will indicate our False Negatives are low.
#2. Miss Rate/False Negative Rate: FN/(TP+FN) or 1 - TPR
##This is just the complement of TPR and correspondingly, we should look to minimize this.
#3. Precision/Positive Predictive Power: TP/(TP + FP)
##This is also important to knowing if we incorrectly predict a lot of good borrowers as defaults (false positive)
##and so we want to maximize this statistic.

#for a give set of probability thresholds, find the various statistics in which we're interested.

prob_cutoffs <- seq(0,1,by = 0.05)

mtx_xgb_rates <- matrix(data = NA, nrow = length(prob_cutoffs), ncol = 6)

rownames(mtx_xgb_rates) <- prob_cutoffs
colnames(mtx_xgb_rates) <- c("TPR", "FNR", "PPV", "Obs", "FN", "FP")

#go through each cutoff and calcuclate the statistics of interest
for(c in 1:length(prob_cutoffs)){
  isDefault_xgb <- NULL
  for(p in 1:length(mdl_xgb_prob)){
    isDefault_xgb <- c(isDefault_xgb, ifelse(mdl_xgb_prob[p] > prob_cutoffs[c], 1, 0))
    }
    FN <- sum(df_model_test$isDefault == 1 & isDefault_xgb == 0)
    FP <- sum(df_model_test$isDefault == 0 & isDefault_xgb == 1)
    TN <- sum(df_model_test$isDefault == 0 & isDefault_xgb == 0)
    TP <- sum(df_model_test$isDefault == 1 & isDefault_xgb == 1)
    
    mtx_xgb_rates[c, "TPR"] <- TP/(TP + FN)
    mtx_xgb_rates[c, "FNR"] <- FN/(TP + FN)
    mtx_xgb_rates[c, "PPV"] <- TP/(TP + FP)
    mtx_xgb_rates[c, "Obs"] <- length(mdl_xgb_prob)
    mtx_xgb_rates[c, "FN"] <- FN
    mtx_xgb_rates[c, "FP"] <- FP
  }
  
#we would have to balance the number of false negatives and false positives, depending
##on risk tolerance. A futher step could be predicting the likely dollar value of chargeoffs
##from False Negatives and missed loan revenue from False Positives.



###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################

#Elastic Net Regression

###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################

#let's check Elastic Net as well to see if we can get a simpler model.

#split the training data into a train and tune set

#take a small subset of the train data for tuning
index_tune <- createDataPartition(df_model_train$loan_no, p=0.15, list = FALSE)

df_model_tune <- df_model_train[index_tune,]
df_model_train_sub <- df_model_train[-index_tune,]

#convert data to a matrix for elastic net
train_x_glmnet <- model.matrix(fmla_optn, data = df_model_train_sub)
train_y_glmnet <- as.numeric(df_model_train_sub$isDefault)

#need a new weight vector for the training set less tune set
wt_mdl_elnet <- if_else(df_model_train_sub$isDefault == 1, wt_pos, wt_neg)
wt_mdl_elnet_tune <- if_else(df_model_tune$isDefault == 1, wt_pos, wt_neg)

test_x_glmnet <- model.matrix(fmla_optn, data = df_model_test)
test_y_glmnet <- as.numeric(df_model_test$isDefault)

#get the lambdas within one standard error of the lambdas that minimize mean squared error.
##The one SE lambdas are a good compromise between minimizing MSE and minimizing the number
##of coefficients.
elnet_alpha <- seq(0,1,by=0.1)
elnet_lambda <- numeric(length(elnet_alpha))

for(i in 1:length(elnet_alpha)){
    model_cv <- cv.glmnet(train_x_glmnet, train_y_glmnet, alpha=elnet_alpha[i], type.measure = "auc"
                          , nfolds=10, weights = wt_mdl_elnet, family = "binomial")
    elnet_lambda[i] <- model_cv$lambda.1se
        }

elnet_grid <- expand.grid(.alpha = elnet_alpha, .lambda = elnet_lambda)
elnet_trControl <- trainControl(method = "cv",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     number = 10 #number of cross fold validations
                      )

y_tune <- ifelse(df_model_tune$isDefault == 1, "default","current")
levels(y_tune) <- make.names(levels(factor(y_tune)))

elnet_tuning <- train(x = model.frame(fmla_optn, df_model_tune)[-1]
                      , y = y_tune
                      , method = "glmnet", metric = "ROC", tuneGrid = elnet_grid
                      , trControl = elnet_trControl, weights = wt_mdl_elnet_tune)

#logistic regression with the chosen lambda and alpha
mdl_elnet <- glmnet(train_x_glmnet, train_y_glmnet, family = "binomial"
                       , lambda = elnet_tuning$finalModel$tuneValue$lambda, alpha = elnet_tuning$finalModel$tuneValue$alpha)

#look at elastic net coefficients
mdl_elnet$beta

#predict probabilities
mdl_elnet_prob <- predict(mdl_elnet, newx = test_x_glmnet, outputmargin = FALSE)

#plot ROC curve
roc_elnet <- roc(predictor = as.numeric(mdl_elnet_prob), response = df_model_test$isDefault
               #, smooth=TRUE
               , ci = TRUE
               # arguments for plot
               , plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
               print.auc=TRUE, legacy.axes=TRUE, main = "Elastic Net")
roc_elnet_ci <- ci.se(roc_elnet, conf.level = 0.95, boot.stratified = TRUE, boot.n = 2000)
plot(roc_elnet_ci, type = "shape", col = "lightblue")

#display AUC and 95% confidence interval
roc_elnet$auc
roc_elnet$ci



