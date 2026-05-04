cap <- tibble(read.csv("cap_data_sportrac.csv")) %>%
  mutate(team = case_match(team,
                           "WAS" ~ "WSH",
                           "VGK" ~ "VEG",
                           .default = team)
  )

leagues <- tibble(read.csv("league.csv"))
leagues$name <- c(
  "Buffalo Sabres", "Tampa Bay Lightning", "Montreal Canadiens", "Boston Bruins",
  "Ottawa Senators", "Detroit Red Wings", "Florida Panthers", "Toronto Maple Leafs",
  "Carolina Hurricanes", "Pittsburgh Penguins", "Philadelphia Flyers", "Washington Capitals",
  "Columbus Blue Jackets", "New York Islanders", "New Jersey Devils", "New York Rangers",
  "Colorado Avalanche", "Dallas Stars", "Minnesota Wild", "Utah Mammoth",
  "St. Louis Blues", "Nashville Predators", "Winnipeg Jets", "Chicago Blackhawks",
  "Vegas Golden Knights", "Edmonton Oilers", "Anaheim Ducks", "Los Angeles Kings",
  "San Jose Sharks", "Seattle Kraken", "Calgary Flames", "Vancouver Canucks", "Arizona Coyotes"
)
leagues <- leagues %>%
  relocate(name) %>%
  rename(code = team)


seasons <- c("2021_2022", "2022_2023", "2023_2024", "2024_2025", "2025_2026")

# okay. like. the utah/arizona thing is causing missing values in cap data
# cause my renaming happens before that join
# but it's just 3 rows (the three observations before ARI changed to UTA) so i'm ignoring it for now
  # yeah this is fine they suck so we dont have to worry about them too much lol
# TODO: fix it
full_data <- data.frame()
for (s in seasons) {
  season_data <- tibble(read.csv(paste0("season", s, ".csv"), header = T, skip =1)) %>%
    rename(name = if(s == "2024_2025") "X." else "X") %>%
    filter(name != "League Average") %>%
    mutate(
      name = case_match(name, c("Utah Hockey Club", "Arizona Coyotes") ~ "Utah Mammoth", .default = name),
      MadePlayoffs = ifelse(grepl("\\*", name), 1, 0),
      name = sub("\\*", "", name),
      season = s
    ) %>%
    left_join(leagues, by = "name") %>%
    relocate(c(code, division, conference), .after=name) %>%
    left_join(subset(cap[cap$season == s,], select = -c(season)), by = join_by(code == team))

  full_data <- bind_rows(full_data, season_data)
}

reg_ot <- read.csv("reg_ot.csv") %>%
  mutate(season = ifelse(season=="2022_2021", "2021_2022", season))
full_data_with_overtimes <- full_data %>%
  full_join(reg_ot, by = c("name", "season")) %>%
  na.omit() %>%
  mutate(OTW = W - RW - SOW,
         newpts = 3*RW + 2*OTW + 1*OL + 1.5*SOW + 0.5*SOL)


train <- full_data %>%
  filter(season %in% seasons[1:4])

test <- full_data %>%
  filter(season == seasons[5])

fit <- glm(MadePlayoffs ~ AvAge + SOS + division + cap_space, family = "binomial", data = train)

# does not look like it does a good job, but we have few predictors in the current model.
# what makes sense, supposing we want to calculate the odds of making playoffs before games are played?
predicted <- predict(fit, newdata = test, type = "response")
plot(abs(predicted - test$MadePlayoffs))

# honestly does not look that much worse than logistic
fit.linear <- lm(MadePlayoffs ~ AvAge + SOS + division + cap_space, data = train)
predicted.linear <- predict(fit.linear, newdata = test)
points(abs(predicted.linear - test$MadePlayoffs), col = "red")
sum(abs(predicted.linear - test$MadePlayoffs) < abs(predicted - test$MadePlayoffs)) # 13. In 13/32 cases, multiple linear regression is performing better

# TODO: ordinal regression


# things to also do: predict if playoffs based on SOS, if playoffs last year, division?
# maybe we can also rank division 1 through 4 based on accumulated points or something..?
# also want to do that 3-2-1 W-OTW-SOW thing but they dont count the number of overtime games????
# wait its on a different table my bad everyone
# ill get that in one sec

# predict points
fit2 <- glm(PTS ~ AvAge + SOS + division + cap_space, family = "poisson", data=train)
summary(fit2)
# can we think abt comparing multiple models year by year predicting making playoffs (when this data is current year)
# ploffs ~ AvAge + SOS + division + cap_space
fit3 <- glm(newpts ~ AvAge + SOS + division + cap_space, family = "poisson", data=full_data_with_overtimes)
summary(fit3) #like okay
fit4 <- glm(MadePlayoffs ~ OTW, family = "binomial", data=full_data_with_overtimes)
summary(fit4) # interesting otw odds ratio 1.309964

fit2122 <- glm(MadePlayoffs ~ AvAge + SOS + division + cap_space, family = "binomial", data=filter(full_data, season=="2021_2022"))
summary(fit2122)
#okay this sucks
# what if we did madePlayoffs last year or something?
fit2223 <- glm(MadePlayoffs ~ SOS + cap_space, family = "binomial", data=filter(full_data, season=="2022_2023"))
summary(fit2223)
# okay so this is still bad
# i do think we need to explore last season (immediate last season) behavior
# maybe i will try to wrangle coaching changes
# the nhl is the worst league on the planet. there is no organized information for this
fit2334
fit2425
fit2526
