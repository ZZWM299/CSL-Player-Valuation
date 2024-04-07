library(dplyr)
library(lubridate)
library(ggplot2)
library(corrplot)
library(psych)
library(flextable)
library(xtable)
library(officer)

#加载数据文件
load(file = "pf_data.RData")

#身价评估实践为2023年12月14日
today <- ymd("2023-12-14")

#计算球员合同剩余时长
contract_exp <- function(x, d) {
  row <- which(d$player_name == x)
  #退役或待业
  if (d$current_club[row] == "Retired" || d$current_club[row] == "Without Club")  {
    return(0)
  }
  #租借
  if (!is.na(d$on_loan_from[row])) {
    if (d$contract_there_expires[row] != "-") {
      ex <- mdy(d$contract_there_expires[row])
    }
  }
  #缺失
  if (is.na(d$contract_expires[row])) {
    ex <- ymd(20241231)
  } else {
    ex <- ymd(d$contract_expires[row])
  }
  return(max(interval(today(), ex) / duration(1, "years"), 0))
}

#计算合同总时长
contract_len <- function(x, d) {
  row <- which(d$player_name == x)
  #退役或待业
  if (d$current_club[row] == "Retired" || d$current_club[row] == "Without Club")  {
    return(0)
  }
  #有续约时间
  if (!is.na(d$last_contract_extension[row])) {
    if (!is.na(d$contract_expires[row])) {
      ex <- ymd(d$contract_expires[row])
      st <- mdy(d$last_contract_extension[row])
    } else {
      ex <- ymd(20241231)
      st <- mdy(d$last_contract_extension[row])
    }
  }
  #合同到期时间or加盟时间缺失
  if (is.na(d$contract_expires[row])) {
    ex <- ymd(20241231)
  } else {
    ex <- ymd(d$contract_expires[row])
  }
  if (is.na(d$joined[row])) {
    st <- ymd(20240101)
  } else if (d$current_club[row] == d$squad[row]) {
    st <- ymd(d$date_joined[row])
  } else {
    st <- ymd(d$joined[row])
  }
  return(interval(st, ex) / duration(1, "years"))
}

#计算年龄
cal_age <- function(birth_date) {
  birth_date <- ymd(birth_date)
  age <- interval(birth_date, today) / duration(1, "years")
  return(age)
}

#输出回归结果
doc <- read_docx()
out <- function(model, model_name) {
  summa <- summary(model)
  su <- as_flextable(xtable(summa))
  doc <- body_add_flextable(doc, su)
  #获取模型参数
  coe <- coefficients(model)
  #获取模型参数的85%、90%、95%置信区间
  conf_int_85 <- confint(model, level = 0.85)
  conf_int_90 <- confint(model, level = 0.90)
  conf_int_95 <- confint(model, level = 0.95)
  #获取预测值和残差值
  predictions <- fitted(model)
  residuals <- residuals(model)
  #将结果存储在CSV文件中
  write.csv(coe, file = paste0(model_name, "_coe.csv"))
  write.csv(conf_int_85, file = paste0(model_name, "_conf_int_85.csv"))
  write.csv(conf_int_90, file = paste0(model_name, "_conf_int_90.csv"))
  write.csv(conf_int_95, file = paste0(model_name, "_conf_int_95.csv"))
  write.csv(predictions, file = paste0(model_name, "_predictions.csv"))
  write.csv(residuals, file = paste0(model_name, "_residuals.csv"))
  print(summa)
}

#异常值处理
t_data[which(t_data$player_name == "Rômulo"), ]$contract_expires <- "2027-01-31"

#外场球员总表
rel <- data.frame(
  player = t_data$player_name,
  tmv = t_data$player_market_value_euro,
  pos = t_data$player_position,
  age = sapply(t_data$player_dob, cal_age),
  nat = t_data$national_cap,
  inj_d = t_data$duration / 30,
  inj_g = t_data$games_missed,
  min = t_data$min / 90,
  g_a = t_data$gls + t_data$ast,
  gpc = t_data$gls * 100 / t_data$sh,
  fld = t_data$fld,
  fls = t_data$fls,
  bck = t_data$int + t_data$tkw,
  cexp = sapply(t_data$player_name, contract_exp, t_data),
  clen = sapply(t_data$player_name, contract_len, t_data),
  url = t_data$URL,
  nmt = t_data$name_in_home_country,
  row.names = NULL,
  stringsAsFactors = FALSE
)

rel[which(is.na(rel$gpc)), ]$gpc <- 0
rel[which(is.na(rel$nmt)), ]$nmt <- rel[which(is.na(rel$nmt)), ]$player

#门将总表
krel <- data.frame(
  player = k_data$player_name,
  tmv = k_data$player_market_value_euro,
  age = sapply(k_data$player_dob, cal_age),
  nat = k_data$national_cap,
  inj_d = k_data$duration / 30,
  inj_g = k_data$games_missed,
  min = k_data$min / 90,
  svp = k_data$"sv%",
  cexp = sapply(k_data$player_name, contract_exp, k_data),
  clen = sapply(k_data$player_name, contract_len, k_data),
  url = k_data$URL,
  nmt = k_data$name_in_home_country,
  row.names = NULL,
  stringsAsFactors = FALSE
)

krel[which(is.na(krel$nmt)), ]$nmt <- krel[which(is.na(krel$nmt)), ]$player

#纠正身价
load(file = "player_data.rdata")
rel$tmv <- sapply(rel$tmv, max, player_bio[which(player_bio$URL == rel$url), ]$player_valuation, na.rm = TRUE)
krel$tmv <- sapply(krel$tmv, max, player_bio[which(player_bio$URL == krel$url), ]$player_valuation, na.rm = TRUE)

#分离数据
cal <- rel[, c("tmv", "age", "nat", "inj_d", "inj_g", "min", "g_a", "gpc", "fld", "fls", "bck", "cexp", "clen")]
kcal <- krel[, c("tmv", "age", "nat", "inj_d", "inj_g", "min", "svp", "cexp", "clen")]

#描述性分析
cal_summary <- describe(cal)
kcal_summary <- describe(kcal)
write.csv(cal_summary, file = "cal_summary1.csv")
write.csv(kcal_summary, file = "kcal_summary1.csv")

#相关性分析
corma <- cor(cal)
kcorma <- cor(kcal)
write.csv(corma, file = "cor_matrix1.csv", row.names = TRUE)
write.csv(kcorma, file = "kcor_matrix1.csv", row.names = TRUE)

#身价取log
cal$tmv <- sapply(cal$tmv, log)
kcal$tmv <- sapply(kcal$tmv, log)

#锋线球员表
frel <- rel[which(rel$pos == "Centre-Forward"), ]
frel <- rbind(frel, rel[which(rel$pos == "Striker"), ])
frel <- rbind(frel, rel[which(rel$pos == "Second Striker"), ])
frel <- rbind(frel, rel[which(rel$pos == "Right Winger"), ])
frel <- rbind(frel, rel[which(rel$pos == "Left Winger"), ])

fcal <- frel[, c("tmv", "age", "nat", "inj_g", "min", "g_a", "gpc", "fls", "cexp", "clen")]
ffit <- lm(log(tmv) ~ ., data = fcal)
out(ffit, "forward-fit")

#中场球员表
mrel <- rel[which(rel$pos == "Central Midfield"), ]
mrel <- rbind(mrel, rel[which(rel$pos == "Attacking Midfield"), ])
mrel <- rbind(mrel, rel[which(rel$pos == "Left Midfield"), ])
mrel <- rbind(mrel, rel[which(rel$pos == "Right Midfield"), ])
mrel <- rbind(mrel, rel[which(rel$pos == "Midfielder"), ])

mcal <- mrel[, c("tmv", "age", "nat", "inj_g", "min", "g_a", "gpc", "fls", "cexp", "clen")]
mfit <- lm(log(tmv) ~ ., data = mcal)
out(mfit, "midfield-fit")

#后卫球员表
drel <- rel[which(rel$pos == "Defender"), ]
drel <- rbind(drel, rel[which(rel$pos == "Centre-Back"), ])

dcal <- drel[, c("tmv", "age", "nat", "inj_g", "min", "g_a", "gpc", "fls", "cexp", "clen")]
dfit <- lm(log(tmv) ~ ., data = dcal)
out(dfit, "defender-fit")

#边后卫球员表
brel <- rel[which(rel$pos == "Left-Back" | rel$pos == "Right-Back"), ]

bcal <- brel[, c("tmv", "age", "nat", "inj_g", "min", "g_a", "gpc", "fls", "cexp", "clen")]
bfit <- lm(log(tmv) ~ ., data = bcal)
out(bfit, "side-back-fit")

#后腰球员表
dmrel <- rel[which(rel$pos == "Defensive Midfield"), ]

dmcal <- dmrel[, c("tmv", "age", "nat", "inj_g", "min", "g_a", "gpc", "fls", "cexp", "clen")]
dmfit <- lm(log(tmv) ~ ., data = dmcal)
out(dmfit, "def-mid-fit")

#门将表
kcal$inj_d <- NULL
kfit <- lm(log(tmv) ~ ., data = kcal)
out(kfit, "keeper-fit")

print(doc, "summaries.docx", preview = "docx")