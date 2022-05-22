library(data.table);library(magrittr);library(jstable);library(lubridate);library(forecast);library(readxl)

setwd("/home/heejooko/ShinyApps/Atopy_smc")

# a <- read_excel("sample_individual.xlsx", 1)
a <- read_excel("data 22-05-17.xlsx", 1)
# a <- a %>% dplyr::filter(ID %in% c(39,49,92)) %>% as.data.table()
a <- as.data.table(a)
a$date <- as_date(a$date)

a$sum_objective <- a$redness + a$dryness + a$oozing + a$edema
a$sum_subjective <- a$itching + a$nonsleep

varlist <- list(
  Symptom = c("symptom", "sum_score", "sum_objective", "sum_subjective", "itching", "nonsleep", "redness", "dryness", "oozing", "edema"),
  Base = c("fever", "sex", "age", "BMI", "SCORAD", "TIgE", names(a)[c(41:63,68:71)]),
  Care = c("bath", "lotion", "drug"),
  Inout = names(a)[21:40]
)

out <- a[, .SD, .SDcols = c("ID", "date", unlist(varlist))]

factor_vars <- c('symptom', 'itching', 'nonsleep', 'redness', 'dryness', 'oozing', 'edema', 'fever', 'sex', 'sensitization', 'income', 'alllergy_fam', 'FA_diag', 'AS_diag', 'AR_diag', 'AC__diag', 'DA__diag', 'house_type', 'fabricsofa', 'remodeling', 'newfurniture', 'pet', 'vent_sm', 'vent_win', 'airpurifier', 'humid_win', 'cookingfan', 'aircon', 'mold', 'airfreshner', 'smoke_indoor', 'bath', 'lotion', 'drug', 'season', 'dow')
out[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]

out.label <- mk.lev(out)
vars.01 <- setdiff(names(out)[sapply(lapply(out, levels), function(x){identical(x, c("0", "1"))})], "sex")

for (v in vars.01){
  out.label[variable == v, val_label := c("No", "Yes")]
}

# out.label[variable == 'sex', val_label:=c("Female", "Male")]

# for (v in setdiff(names(out), c("ID", "date", "age", "BMI", "birthweight"))){
# out.label[variable == v, var_label := info.label[name == v, `설명`]]
# }


out.label[variable == "season", ":="(val_label = c("봄", "여름", "가을", "겨울"))]
out.label[variable == "dow", ":="(val_label = c("일", "월", "화","수", "목", "금", "토"))]
out.label[variable == "income", ":="(val_label = c("100만원 이하", "100~300만원", "300~500만원", "500만원 이상"))]
out.label[variable == "house_type", ":="(val_label = c("연립주택(다세대주택)", "일반아파트", "주상복합아파트", "상가건물", "기타"))]
out.label[variable == "vent_sm", ":="(val_label = c("거의 안함", "주1회 미만", "주1회", "주2~3회", "주4~6회", "매일"))]
out.label[variable == "vent_win", ":="(val_label = c("거의 안함", "주1회 미만", "주1회", "주2~3회", "주4~6회", "매일"))]
out.label[variable == "airfreshner", ":="(val_label = c("전혀", "간혹", "자주", "매일"))]


## ID for prediction: Enough N, appropriate proportion
ID.pred <- out[, .(.N, Nsypmtom = sum(symptom == 1), prop = round(sum(symptom == 1)/.N, 2)), keyby = "ID"][N >= 100 & prop <= 0.9 & prop >= 0.1]
# ID.pred <- out

library(tsibble);library(fable)

# select only first row of each ID+date combination
out<-out[,.SD[1:1],key=c('ID','date')]

zz <- tsibble(out[ID %in% ID.pred$ID, .SD, .SDcols = -c(varlist$Base)], key = ID, index = date)
# zz <- tsibble(out[ID %in% ID.pred$ID, .SD, .SD], key = ID, index = date)
# zz_fg %>% names

# has_gaps(zz, .full = TRUE)
zz_fg <- fill_gaps(zz)

# vars.pred <- c("bath", "lotion", "drug", "HCHO_in", "CO2_in", "CO_in", "temp_in", "RH_in", "PM25_in", 
#                "PM25_out", "O3_out", "NO2_out", "CO_out", "SO2_out", "temp_out", "RH_out", "season", "dow")

vars.pred <- c("drug", "HCHO_in", "CO2_in", "CO_in", "temp_in", "RH_in", "PM25_in", 
               "PM25_out","PM25_in_lag1","PM25_in_lag2","PM25_in_lag3","PM25_in_lag4","PM25_in_lag5",
               "O3_out", "NO2_out", "CO_out", "SO2_out", "temp_out", "RH_out", "season", "dow")

