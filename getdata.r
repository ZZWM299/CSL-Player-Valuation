library(worldfootballR)
library(dplyr, warn.conflicts = FALSE)

#检查收集数据项数是否达到球员总数n
cnt <- list()
chknum <- function(x) {
  cnt <- append(cnt, x)
  if (x == n) {
    cat("All players collected!\n")
    return(1)
  }else {
    cat(x, "/", n, "Not equal!\n")
    return(0)
  }
}

#第一部分:获取德转数据

#获取全联赛身价数据，存储在数据框csl_tmv中
csl_tmv <- tm_player_market_values(
  start_year=2022,
  league_url="https://www.transfermarkt.com/chinese-super-league/startseite/wettbewerb/CSL"
)
csl_tmv <- csl_tmv[!duplicated(csl_tmv$player_url, fromLast = TRUE), ]
n <- nrow(csl_tmv)
cat(n, "players' MV in total.\n")

#获取球队链接，存储在字符向量team_urls中
team_urls <- tm_league_team_urls(start_year=2022, league_url="https://www.transfermarkt.com/chinese-super-league/startseite/wettbewerb/CSL")

#获取球员链接，存储在字符向量player_urls中
i <- 0
player_urls <- vector()
while (i < length(team_urls)) {
  i <- i + 1
  team_players_urls <- tm_team_player_urls(team_urls[i])
  player_urls <- c(player_urls, team_players_urls)
}
player_urls <- unique(player_urls) #去重
chknum(length(player_urls))

#获取球员个人简介，存储在数据框player_bio中
player_bio <- tm_player_bio(player_urls)
chknum(nrow(player_bio))

#获取球员伤病数据，清理后存储在数据框player_inj中
player_injury <- tm_player_injury_history(player_urls)
player_injury <- unique(player_injury) #去重
player_inj <- player_injury %>% group_by(player_url) %>% summarise(games_missed = sum(as.numeric(games_missed), na.rm = TRUE), duration = sum(as.numeric(duration), na.rm = TRUE))

#第二部分：获取FBref数据

#获取球队链接，存储在字符向量team_fb_urls中
team_fb_urls <- fb_teams_urls("https://fbref.com/en/comps/62/2023/2023-Chinese-Super-League-Stats")

#获取球员链接，存储在字符向量player_fb_urls中
j <- 0
player_fb_urls <- vector()
while (j < length(team_fb_urls)) {
  j <- j + 1
  team_players_urls <- fb_player_urls(team_fb_urls[j])
  player_fb_urls <- c(player_fb_urls, team_players_urls)
}
#player_fb_urls <- player_fb_urls[!duplicated(player_fb_urls)]
chknum(length(player_fb_urls))

#获取球员比赛表现数据，存储在数据框player_logs中
k <- 0
player_logs <- data.frame(
  "player"=character(),
  "national_cap"=numeric(),
  "min"=numeric(),
  "gls"=numeric(),
  "ast"=numeric(),
  "sh"=numeric(),
  "fls"=numeric(),
  "fld"=numeric(),
  "tkw"=numeric(),
  "int"=numeric(),
  stringsAsFactors = FALSE
)
keeper_logs <- data.frame(
  "player"=character(),
  "national_cap"=numeric(),
  "min"=numeric(),
  "sv%"=double(),
  stringsAsFactors = FALSE
)
exc <- c(30, 97, 131, 164, 200, 233, 240, 305, 341, 374, 471, 541)
while (k < length(player_fb_urls)) {
  k <- k + 1
  if (k %in% exc) {
    next
  }
  u <- player_fb_urls[k]
  s <- fb_player_match_logs(u, "2023", "summary")
  v <- vector()
  v <- append(v, s[1, 1])
  v <- append(v, any(grepl("China", s[, 9])))
  v <- append(v, sum(s[, 13], na.rm = TRUE))
  if (any(grepl("GK", s[, 12]))) {
    s <- fb_player_match_logs(u, "2023", "keepers")
    v <- append(v, mean(s$Save_percent_Performance, na.rm = TRUE))
    keeper_logs <- rbind(keeper_logs, v, stringsAsFactors = FALSE)
  }else {
    v <- append(v, sum(s[, 14], na.rm = TRUE))
    v <- append(v, sum(s[, 15], na.rm = TRUE))
    v <- append(v, sum(s[, 18], na.rm = TRUE))
    v <- append(v, sum(s[, 22], na.rm = TRUE))
    v <- append(v, sum(s[, 23], na.rm = TRUE))
    v <- append(v, sum(s[, 26], na.rm = TRUE))
    v <- append(v, sum(s[, 27], na.rm = TRUE))
    player_logs <- rbind(player_logs, v, stringsAsFactors = FALSE)
  }
}
names(player_logs) <- c("player", "national_cap", "min",
  "gls", "ast", "sh", "fls", "fld", "tkw", "int"
)
names(keeper_logs) <- c("player", "national_cap", "min", "sv%")
player_logs <- unique(player_logs)
keeper_logs <- unique(keeper_logs)
chknum(nrow(player_logs) + nrow(keeper_logs))

cat("\n", cnt, "\n All data collected!\n")

#保存数据至文件
save(
  csl_tmv,
  player_bio, player_inj,
  player_logs, keeper_logs,
  file = "player_data.RData"
)

save(
  team_urls, team_fb_urls,
  player_urls, player_fb_urls,
  player_injury,
  file = "urls_raw.RData"
)

cat("All data saved!\n")