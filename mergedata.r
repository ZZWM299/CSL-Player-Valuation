library(worldfootballR)
library(dplyr, warn.conflicts = FALSE)

#创建一个函数，该函数接受一个名字，将其分割成姓和名，然后返回排序后的名字
sort_name <- function(name) {
  parts <- strsplit(name, " ")[[1]]
  sorted_parts <- sort(parts)
  return(paste(sorted_parts, collapse = " "))
}

#加载数据文件
load(file = "player_data.RData")
player_injury <- read.csv('all_injury.csv', header = TRUE, stringsAsFactors = FALSE)

#去重
player_bio <- unique(player_bio)
player_logs <- unique(player_logs)
keeper_logs <- unique(keeper_logs)
csl_tmv <- csl_tmv[!duplicated(csl_tmv$player_url, fromLast = TRUE), ]

#选取身价表格
tmv <- dplyr::select(csl_tmv, squad, player_name, player_position, player_dob, date_joined, player_market_value_euro, player_url)
colnames(tmv)[colnames(tmv) == "player_url"] <- "URL"

#选取简介表格
bio <- select(player_bio, name_in_home_country, current_club, joined, contract_expires, on_loan_from, contract_there_expires, last_contract_extension, URL)

#选取伤病表格
inj <- player_injury %>% group_by(player_url) %>% summarise(games_missed = sum(as.numeric(games_missed), na.rm = TRUE), duration = sum(as.numeric(gsub(" days", "", duration)), na.rm = TRUE))
colnames(inj)[colnames(inj) == "player_url"] <- "URL"

#合并德转数据
tm_data <- merge(tmv, bio, by = "URL")
tm_data <- merge(tm_data, inj, by = "URL", all.x = TRUE)

#将门将位置的数据单独处理
keeper_tm <- tm_data[tm_data$player_position == "Goalkeeper", ]
tm_data <- anti_join(tm_data, keeper_tm)

#将英文名重复的数据单独处理
tm_dup <- rbind(tm_data[duplicated(tm_data$player_name), ], tm_data[duplicated(tm_data$player_name, fromLast = TRUE), ])
log_dup <- rbind(player_logs[duplicated(player_logs$player), ], player_logs[duplicated(player_logs$player, fromLast = TRUE), ])
#将英文名重复的数据删除
tm_data <- anti_join(tm_data, tm_dup)
player_logs <- anti_join(player_logs, log_dup)

#在tm_data和player_logs数据框中创建新的列，该列包含排序后的名字
tm_data$sorted_name <- sapply(tm_data$player_name, sort_name)
player_logs$sorted_name <- sapply(player_logs$player, sort_name)
player_logs <- player_logs[!duplicated(player_logs$sorted_name), ]

#根据新的列合并数据框
t_data <- inner_join(tm_data, player_logs, by = "sorted_name")

#门将数据同样处理
keeper_tm$sorted_name <- sapply(keeper_tm$player_name, sort_name)
keeper_logs$sorted_name <- sapply(keeper_logs$player, sort_name)
k_data <- inner_join(keeper_tm, keeper_logs, by = "sorted_name")

#处理剩余数据
tm_ytm <- anti_join(tm_data, t_data)
log_ytm <- anti_join(player_logs, t_data)
#处理非英语外文名
tm_ytm$sorted_name <- sapply(tm_ytm$name_in_home_country, sort_name)
m2 <- inner_join(tm_ytm, log_ytm, by = "sorted_name")
t_data <- rbind(t_data, m2)
#仍未匹配的数据手动配对（省略过程），为ytm
tm_ytm <- anti_join(tm_ytm, m2)
log_ytm <- anti_join(log_ytm, m2)
k_data <- rbind(k_data, ytm)

#手动配对英文名相同的数据后，合并
tm_dup$sorted_name <- sapply(tm_dup$player_name, sort_name)
log_dup$sorted_name <- sapply(log_dup$player, sort_name)
dup <- inner_join(tm_dup, log_dup, by = "sorted_name")
t_data <- rbind(t_data, dup)

#删除不再需要的列
t_data$sorted_name <- NULL
k_data$sorted_name <- NULL

#保存匹配后数据508条
save(t_data, k_data, file = "final_data.RData")