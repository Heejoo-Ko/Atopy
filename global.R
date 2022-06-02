library(data.table);library(magrittr);library(jstable);library(lubridate);library(forecast);library(readxl)

setwd("/home/heejooko/ShinyApps/Atopy")

a <- read_excel("data 22-05-17.xlsx", 1)
# a <- read_excel("Green patrol_AD and air pollution for MR_pollen.xlsx", 1)
# a <- a %>% dplyr::filter(ID %in% c(39,49,92)) %>% as.data.table()
a <- as.data.table(a)
a$date <- as_date(a$date)

a$sum_objective <- a$redness + a$dryness + a$oozing + a$edema
a$sum_subjective <- a$itching + a$nonsleep

varlist <- list(
  # Symptom = c("symptom", "sum_score", "sum_objective", "sum_subjective", "itching", "nonsleep", "redness", "dryness", "oozing", "edema"),
  Symptom = c("symptom", "sum_score", "sum_objective", "sum_subjective"),
  Base = c("fever", "sex", "age", "BMI", "SCORAD", "TIgE", names(a)[c(41:63)]),
  Care = c("bath", "lotion", "drug"),
  Inout = names(a)[c(21:40, 68:71)]
)

out <- a[, .SD, .SDcols = c("ID", "date", unlist(varlist))]

# factor_vars <- c('symptom', 'itching', 'nonsleep', 'redness', 'dryness', 'oozing', 'edema', 'fever', 'sex', 'sensitization', 'income', 'alllergy_fam', 'FA_diag', 'AS_diag', 'AR_diag', 'AC__diag', 'DA__diag', 'house_type', 'fabricsofa', 'remodeling', 'newfurniture', 'pet', 'vent_sm', 'vent_win', 'airpurifier', 'humid_win', 'cookingfan', 'aircon', 'mold', 'airfreshner', 'smoke_indoor', 'bath', 'lotion', 'drug', 'season', 'dow')
factor_vars <- c('symptom', 'fever', 'sex', 'sensitization', 'income', 'alllergy_fam', 'FA_diag', 'AS_diag', 'AR_diag', 'AC__diag', 'DA__diag', 'house_type', 'fabricsofa', 'remodeling', 'newfurniture', 'pet', 'vent_sm', 'vent_win', 'airpurifier', 'humid_win', 'cookingfan', 'aircon', 'mold', 'airfreshner', 'smoke_indoor', 'bath', 'lotion', 'drug', 'season', 'dow')
out[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]


vars.01 <- setdiff(names(out)[sapply(lapply(out, levels), function(x){identical(x, c("0", "1"))})], "sex")

for (v in vars.01){
  out[[v]]<-as.factor(ifelse(out[[v]]==0,"No","Yes"))
  # out.label[variable == v, val_label := c("No", "Yes")]
}
out[["sex"]]<-as.factor(ifelse(out[[v]]==0,"Female","Male"))
# out.label[variable == 'sex', val_label:=c("Female", "Male")]

out.label <- mk.lev(out)

# for (v in setdiff(names(out), c("ID", "date", "age", "BMI", "birthweight"))){
# out.label[variable == v, var_label := info.label[name == v, `????`]]
# }

out.label[variable == "season", ":="(val_label = c("Spring", "Summer", "Autumn", "Winter"))]
out.label[variable == "dow", ":="(val_label = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))]
out.label[variable == "income", ":="(val_label = c("less than 1 million won", "1~3 million won", "3~5 million won", "more than 5 million won"))]
out.label[variable == "house_type", ":="(val_label = c("Multi Complex House", "Apartment", "Mixed-use Apartment Building", "Commercial Building", "Other"))]
out.label[variable == "vent_sm", ":="(val_label = c("Almost never", "<1 time per week", "1 time per week", "2~3 times per week", "4~6 times per week", "Everyday"))]
out.label[variable == "vent_win", ":="(val_label = c("Almost never", "<1 time per week", "1 time per week", "2~3 times per week", "4~6 times per week", "Everyday"))]
out.label[variable == "airfreshner", ":="(val_label = c("Never", "Occasionally", "Often", "Everyday"))]

## ID for prediction: Enough N, appropriate proportion

ID.pred <- out[, .(.N, Nsypmtom = sum(symptom =="Yes"), prop = round(sum(symptom == "Yes")/.N, 2)), keyby = "ID"][N >= 100 & prop <= 0.9 & prop >= 0.1]$ID
# ID.pred <- out[,.N,key=c("ID")][N>=100,ID,]

library(tsibble);library(fable)

# select only first row of each ID+date combination
out<-out[ID %in% ID.pred,.SD[1:1],key=c('ID','date')]

# zz <- tsibble(out[ID %in% ID.pred, .SD, .SDcols = -c(varlist$Base)], key = ID, index = date)

# # zz <- tsibble(out[ID %in% ID.pred$ID, .SD, .SD], key = ID, index = date)
# # zz_fg %>% names

# # has_gaps(zz, .full = TRUE)
# zz_fg <- fill_gaps(zz)

# vars.pred <- c("bath", "lotion", "drug", "HCHO_in", "CO2_in", "CO_in", "temp_in", "RH_in", "PM25_in", 
#                "PM25_out", "O3_out", "NO2_out", "CO_out", "SO2_out", "temp_out", "RH_out", "season", "dow")

vars.pred <- c("drug", "HCHO_in", "CO2_in", "CO_in", "temp_in", "RH_in", "PM25_in", "PM25_out",
               "O3_out", "NO2_out", "CO_out", "SO2_out", "temp_out", "RH_out", "pollen_ln", "trees_ln", "weeds_ln", "grasses_ln")
