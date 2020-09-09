library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)

matchstats = read.csv(file = '../charting-w-stats-Overview.csv')

matchstats = matchstats %>%
  separate(
    .,
    col = match_id,
    into = c(
      'match_date',
      'womens',
      'tourney_name',
      'round',
      'player_1',
      'player_2'
    ),
    sep = '-'
  ) %>%
  filter(., set != 'Total')

matchstats$player_1 =
  sapply(matchstats$player_1, function(x) {
    gsub(x, pattern = '_', replacement = ' ')
  })

matchstats$player_2 =
  sapply(matchstats$player_2, function(x) {
    gsub(x, pattern = '_', replacement = ' ')
  })

matchstats$tourney_name =
  sapply(matchstats$tourney_name, function(x) {
    gsub(x, pattern = '_', replacement = ' ')
  })

matchstats$tourney_name =
  sapply(matchstats$tourney_name, tolower)

matchstats$year = sapply(matchstats$match_date, function(x) {
  substr(x, 1, 4)
})

matchstats = matchstats %>%
  select(.,
         year,
         tourney_name,
         player_1,
         player_2,
         player,
         winners,
         unforced,
         set)

player1stats = matchstats %>%
  filter(., player == 1) %>%
  select(
    .,
    year,
    tourney_name,
    player_1,
    player_2,
    player_1_winners = winners,
    player_1_unforced = unforced,
    set
  )

player2stats = matchstats %>%
  filter(., player == 2) %>%
  select(
    .,
    year,
    tourney_name,
    player_1,
    player_2,
    player_2_winners = winners,
    player_2_unforced = unforced,
    set
  )

playerstats =
  inner_join(
    player1stats,
    player2stats,
    by = c('year',
           'tourney_name',
           'player_1',
           'player_2',
           'set')
  )

playerstats$set = as.numeric(playerstats$set)

#----------------------------

file_names = sapply(2012:2020,
                    function(s) {
                      paste('../wta_matches_', s, '.csv', sep = '')
                    })
files_list = lapply(file_names, function(file) {
  read.csv(file, header = TRUE)
})

files_list = lapply(files_list, function(df) {
  df %>%
    select(.,
           tourney_name,
           tourney_date,
           winner_name,
           loser_name,
           score)
})

DF = bind_rows(files_list)
DF

# style_by_year = function(s) {
#   DF = read.csv(file = paste('../wta_matches_', s, '.csv', sep = ''),
#                 header = TRUE)

DF$year = sapply(DF$tourney_date, function(x) {
  substr(x, 1, 4)
})

DF2 = DF %>%
  separate(
    .,
    score,
    sep = ' ',
    into = c('set1', 'set2', 'set3', 'set4'),
    fill = 'right'
  )
head(DF2)

DF_set1 = DF2 %>%
  select(., year,
         tourney_name,
         winner_name,
         loser_name,
         setscore = set1) %>%
  mutate(., set = 1)

DF_set2 = DF2 %>%
  select(., year,
         tourney_name,
         winner_name,
         loser_name,
         setscore = set2) %>%
  mutate(., set = 2)

DF_set3 = DF2 %>%
  select(., year,
         tourney_name,
         winner_name,
         loser_name,
         setscore = set3) %>%
  mutate(., set = 3)

DF_sets = bind_rows(DF_set1, DF_set2, DF_set3) %>%
  filter(
    .,!grepl("[a-z]|]|\\[", setscore, ignore.case = TRUE),
    setscore != '',!is.na(setscore)
  ) %>%
  mutate(.,
         setscore = sub(
           pattern = '\\(.*\\)',
           replacement = '',
           x = setscore
         )) %>%
  separate(.,
           setscore,
           into = c('games1', 'games2'),
           sep = '-') %>%
  mutate(
    .,
    games1 = as.numeric(games1),
    games2 = as.numeric(games2),
    winner_name2 = ifelse(games1 > games2,
                          yes = winner_name,
                          no = loser_name),
    loser_name2 = ifelse(games1 < games2,
                         yes = winner_name,
                         no = loser_name)
  ) %>%
  select(
    .,
    year,
    tourney_name,
    winner_name = winner_name2,
    loser_name = loser_name2,
    games1,
    games2,
    set
  )

DF_sets

DF_sets$tourney_name = tolower(x = DF_sets$tourney_name)

placeholder1 = inner_join(
  DF_sets,
  playerstats,
  by = c(
    'year',
    'tourney_name',
    'winner_name' = 'player_1',
    'loser_name' = 'player_2',
    'set'
  )
) %>%
  select(
    .,
    year,
    tourney_name,
    winner_name,
    loser_name,
    winner_winners = player_1_winners,
    winner_unforced = player_1_unforced,
    loser_winners = player_2_winners,
    loser_unforced = player_2_unforced,
    games1,
    games2
  )

placeholder2 = inner_join(
  DF_sets,
  playerstats,
  by = c(
    'year',
    'tourney_name',
    'winner_name' = 'player_2',
    'loser_name' = 'player_1',
    'set'
  )
) %>%
  select(
    .,
    year,
    tourney_name,
    winner_name,
    loser_name,
    winner_winners = player_2_winners,
    winner_unforced = player_2_unforced,
    loser_winners = player_1_winners,
    loser_unforced = player_1_unforced,
    games1,
    games2
  )

master = bind_rows(placeholder1, placeholder2) %>%
  mutate(.,
         winner_games = pmax(games1, games2),
         loser_games = pmin(games1, games2)) %>%
  select(.,-games1,-games2)

master

master_W = master %>%
  select(
    .,
    year,
    name = winner_name,
    winners = winner_winners,
    unforced = winner_unforced,
    games_won = winner_games,
    games_lost = loser_games
  )

master_L = master %>%
  select(
    .,
    year,
    name = loser_name,
    winners = loser_winners,
    unforced = loser_unforced,
    games_won = loser_games,
    games_lost = winner_games
  )

master_allsets = bind_rows(master_W, master_L)
master_allsets


master_means = master_allsets %>%
  group_by(., year, name) %>%
  summarise(
    .,
    mean_per_game = sum(winners + unforced) / sum(games_won + games_lost),
    winners_to_unforced_ratio = sum(winners) / (sum(unforced))
  )
master_means

yearly_means = master_means %>% 
  group_by(., year) %>% 
  summarise(.,
            mmpg = mean(mean_per_game),
            mwur = median(winners_to_unforced_ratio))

master_means = inner_join(master_means, yearly_means) %>%
  mutate(
    .,
    aggression = case_when(
      mean_per_game >= mmpg ~ 'aggressive',
      mean_per_game < mmpg ~ 'defensive'
    ),
    consistency =   case_when(
      winners_to_unforced_ratio >= mwur ~ 'consistent',
      winners_to_unforced_ratio < mwur ~ 'inconsistent'
    )
  )

master_means = master_means %>%
  unite(., style, aggression, consistency, sep = ', ')

style_counts =  master_means %>%
  group_by(., year) %>%
  summarise(., year, style, number = n()) %>% 
  group_by(., year, style) %>% 
  summarise(., percent = n() / mean(number))
style_counts


just_styles = master_means %>%
  select(., year, name, style)


W_UE_by_style = inner_join(master_allsets,
                           just_styles,
                           by = c('name', 'year'))
W_UE_by_style


WL_by_style =  inner_join(master,
                          just_styles,
                          by = c('winner_name' = 'name', 'year')) %>%
  rename(.,
         winner_style = style) %>%
  inner_join(., just_styles, by = c('loser_name' = 'name', 'year')) %>%
  rename(.,
         loser_style = style)
WL_by_style


style_matchups = WL_by_style %>%
  group_by(.,
           year,
           winner_style,
           loser_style) %>%
  summarise(.,
            winner_games = sum(winner_games),
            loser_games = sum(loser_games)) %>%
  filter(.,
         (winner_style != loser_style))
style_matchups


test_1 = style_matchups %>%
  select(
    year,
    style = winner_style,
    opponent = loser_style,
    p1_games = winner_games,
    p2_games = loser_games
  )

test_2 = style_matchups %>%
  select(
    year,
    style = loser_style,
    opponent = winner_style,
    p1_games = loser_games,
    p2_games = winner_games
  )

style_matchups = bind_rows(test_1, test_2) %>%
  group_by(., year, style, opponent) %>%
  summarise(., win_percent = sum(p1_games) / sum(p1_games + p2_games))
style_matchups


style_win_pcts = W_UE_by_style %>%
  group_by(., year, style) %>%
  summarise(
    .,
    games_won = sum(games_won),
    games_lost = sum(games_lost),
    win_percent = sum(games_won) / sum(games_won + games_lost)
  )
style_win_pcts


#   return(
#     list(
#       master_means, --->players.ratios.style
#       style_win_pcts, --->win.pct.by.style
#       style_counts, --->style.counts
#       style_matchups --->style.matchups
#       W_UE_by_style --->by.style
#     )
#   )
# }


for_ratios_histogram = master_means %>% 
  gather(., key = stat_type, value = ratio, mean_per_game:winners_to_unforced_ratio) %>% 
  select(year, stat_type, ratio)





year = rep(2012:2020, c(4, 4, 4, 4, 4, 4, 4, 4, 4))
style = rep(
  c(
    'aggressive, consistent',
    'aggressive, inconsistent',
    'defensive, consistent',
    'defensive, inconsistent'
  ),
  9
)



correlation_df = W_UE_by_style %>% 
  group_by(., name) %>% 
  summarise(.,
            mmpg = sum(winners + unforced) / sum(games_won + games_lost),
            wtur = sum(winners) / sum(unforced),
            mwpg = sum(winners) / sum(games_won + games_lost),
            mupg = sum(unforced) / sum(games_won + games_lost),
            win_percent = sum(games_won) / (sum(games_won + games_lost))) %>% 
  inner_join(., just_styles, by = 'name') %>%
  ungroup(.) %>% 
  group_by(style) %>%
  summarise(cor.mwpg = cor(win_percent, mwpg),
            cor.mupg = cor(win_percent, mupg),
            cor.mmpg = cor(win_percent, mmpg),
            cor.wtur = cor(win_percent, wtur)) %>% 
  gather(., corr.between, corr.coef, 2:5)


with_GS = master_means %>%
  mutate(
    .,
    GS = case_when(
      ((year == 2012) & (
        name %in% c('Serena Williams',
                    'Maria Sharapova',
                    'Victoria Azarenka')
      )) ~ 'yes',
      ((year == 2013) & (
        name %in% c('Victoria Azarenka',
                    'Serena Williams',
                    'Marion Bartoli')
      )) ~ 'yes',
      ((year == 2014) & (
        name %in% c('Na Li',
                    'Maria Sharapova',
                    'Petra Kvitova',
                    'Serena Williams')
      )) ~ 'yes',
      ((year == 2015) & (
        name %in% c('Serena Williams',
                    'Flavia Pennetta')
      )) ~ 'yes',
      ((year == 2016) & (
        name %in% c('Angelique Kerber',
                    'Garbine Muguruza',
                    'Serena Williams')
      )) ~ 'yes',
      ((year == 2017) & (
        name %in% c(
          'Serena Williams',
          'Jelena Ostapenko',
          'Garbine Muguruza',
          'Sloane Stephens'
        )
      )) ~ 'yes',
      ((year == 2018) & (
        name %in% c(
          'Caroline Wozniacki',
          'Simona Halep',
          'Angelique Kerber',
          'naomi Osaka'
        )
      )) ~ 'yes',
      ((year == 2019) & (
        name %in% c(
          'Naomi Osaka',
          'Ashleigh Barty',
          'Simona Halep',
          'Bianca Andreescu'
        )
      )) ~ 'yes',
      ((year == 2020) & (name == 'Sofia Kenin')) ~ 'yes',
      TRUE ~ 'no'
    )
  )

GS_plot = function(yr, df) {
    df = filter(df, year == yr)
    scatter.year = df %>% 
    ggplot(aes(x = mean_per_game, y = winners_to_unforced_ratio)) +
    geom_point(data = df %>% filter(., GS == 'no'),
               aes(color = style),
               size = 2) +
    scale_color_brewer(palette = "RdGy") +
    geom_point(
      data = df %>% filter(., GS == 'yes'),
      color = 'green',
      size = 4) +
    xlab('Number of winners and unforced errors per game') +
      ylab('Ratio of winners to unforced errors') +
      ggtitle('Scatter plot of aggression vs. consistency',
              subtitle = 'Points corresponding to Grand Slam winners are in green')
    return(scatter.year)
}

win_percent_plot = function(yr, df) {
  df = filter(df, year == yr)
  pcts.bar = df %>%
    ggplot(., aes(x = style, y = win_percent*100)) +
    geom_col(aes(fill = style)) +
    scale_fill_brewer(palette = 'RdGy') +
    xlab('Style') +
    ggtitle('% games won by player style') +
    ylab('Win %') +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  return(pcts.bar)
}

matchups_plot = function(yr, df) {
  df = filter(df, year == yr)
  matchups.bar = df %>%
    ggplot(., aes(x = style, y = win_percent)) +
    geom_col(aes(fill = opponent), position = 'dodge') +
    scale_fill_brewer(palette = 'RdGy')
  return(matchups.bar)
}

corr_plot = correlation_df %>% 
  ggplot(aes(x = style, y = corr.between,)) +
  geom_tile(aes(fill = corr.coef, ), colour = 'black') +
  scale_fill_gradient2(
    low = "firebrick",
    high = "blue",
    mid = "white",
    midpoint = 0)

counts_plot = style_counts %>%
  ggplot(aes(x = year, y = percent)) +
  geom_col(aes(fill = style), position = 'fill') +
  scale_fill_brewer(palette = 'RdGy')


means_plot = function(yr, df, measurement) {
  df = filter(df, year == yr, stat_type == measurement)
  consistency_histogram = df %>% 
    ggplot(aes(x = ratio)) +
    geom_histogram(color = 'black', fill = 'lavender', bins = 20)
  return(consistency_histogram)
}

GS_winners = unique(with_GS[with_GS$GS == 'yes',]$name)


GS_winners_plot = function(player) {
  winners_chart = inner_join(master_allsets, with_GS, by = c('name', 'year')) %>% 
    select(year, name, games_won, games_lost, GS) %>% 
    filter(., name == player) %>% 
    group_by(., year, name) %>% 
    summarise(., win_percent = sum(games_won)/sum(games_won + games_lost), GS) %>%
    distinct(.) %>% 
    inner_join(., just_styles, by = c('year', 'name')) %>% 
    ggplot(., aes(x = year, y = win_percent)) +
    geom_col(aes(fill = style, color = GS), size = 2) +
    scale_fill_brewer(palette = 'RdGy')
  return(winners_chart)
}
