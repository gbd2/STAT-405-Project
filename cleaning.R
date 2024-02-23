library(data.table)
library(dplyr)

# reads in play-by-play data
pbp <- fread("play_by_play.csv")

# reads in game-by-game data
game <- fread("game.csv")

# Changes the dates to just their years
game$game_date <- as.integer((format(game$game_date, "%Y")))
game <- game[, c("game_id", "game_date")]

# Removes unnecessary columns
pbp[, c("wctimestring", "player1_team_city", "player1_team_nickname", "person1type", "player2_team_city", "player2_team_nickname", "person2type", "player3_team_city", "player3_team_nickname", "person3type", "video_available_flag")] <- NULL

# Ensures all vital columns are not empty/NA
no_missing <- complete.cases(pbp$"game_id", pbp$"eventnum", pbp$"eventmsgtype", pbp$"eventmsgactiontype",
                             pbp$"period", pbp$"wctimestring", pbp$"pctimestring", pbp$"score")
pbp <- pbp[no_missing, ]

# Clears up memory space
rm(no_missing)

# Removes duplicate rows
pbp <- unique(pbp)

# Merge with game-by-game data to get year information for each event (row)
pbp <- merge(pbp, game, by = "game_id", all.x=TRUE)
head(pbp)

# parses descriptions to get shot distance
dist <- function(str, pattern = "\\b(\\d+)'") {
  pos <- regexpr(pattern, str)
  if (pos[1] <= 0){
    return(0)
  }
  sub <- regmatches(str, pos)
  num <- as.integer(substring(sub, 1, nchar(sub)-1))
  return (num)
}

# initialize the column
pbp$shot_dist <- 0

# Runs the parser on each row, parsing different columns depending on what is empty or not
pbp <- pbp %>%
  mutate(
    shot_dist = apply(pbp[, c("homedescription", "visitordescription")], 1, function(row) {
      dist_value <- dist(row["homedescription"])
      if (is.na(dist_value) || dist_value <= 0) {
        dist_value <- dist(row["visitordescription"])
      }
      return(dist_value)
    })
  )
