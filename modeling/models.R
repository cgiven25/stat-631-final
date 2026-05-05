# 2025-2026 season
test <- readRDS("test.RDS")

# 2021-2025 seasons
train <- readRDS("train.RDS")

full <- rbind(train, test)
# TODO: get range of all predictors to help with interpretation


# -------------------------------
library(pROC)
showROC <- function(model, response) {
  roc(test[, response], predict(model, newdata = test, type="response"), smooth = T, plot = T)
}

# TODO:
# overtime stuff (ask Emma), currently not included in training/testing data

# initial model (wanted to choose predictors we could know before season starts)
# AvAge: higher average age performs worse? or better due to experience?
# SOS: how difficult the schedule is for the team. measure of goals above (+) or below (-) average.
log1 <- glm(MadePlayoffs ~ AvAge + SOS, data = train, family = "binomial")
summary(log1) # sig: SOS
showROC(log1, "MadePlayoffs") # area .672, not great

# maybe we can get more information.
# cap space: money left in a team's player salary budget
# ltir: long term injury reserve - salary currently tied up by injured players. lets teams surpass the salary cap by that amount
#  - requires them to predict that the player will miss >= 10 NHL games over 24 days of the season

# Expecations:
# cap space to be negatively correlated with making playoffs: their players not worth as much?
# ltir: unsure? higher means that more players are out but it lets them spend money on new players. depends who is out, probably
# division: certain divisions are just stronger.
log2 <- glm(MadePlayoffs ~ AvAge + SOS + division + cap_space + ltir, data = train, family = "binomial")
summary(log2) # sig: SOS, pacific, cap_space
showROC(log2, "MadePlayoffs") # better, area .744

# still not fantastic. with a cutoff of 0.5 (up to .8 same results):
pred.made <- predict(log2, newdata = test, type="response") > .5
sum(pred.made == test$MadePlayoffs) # 22/32 predicted correctly
# only predicted that 10 teams would make the playoffs for cutoffs in [.5, .8]
# 8 true positives, 2 false positives
# 14 true negatives, 10 (!!) false negatives

# these models are trash. why?
# many features overlap for teams that make the playoffs vs teams that don't. hard to discern
# machine learning approach maybe better? future work

# what if we try to predict points? once point rankings are figured out, playoffs are determined
loglin <- glm(PTS ~ AvAge + SOS + division + cap_space + ltir, data = train, family = "poisson")
summary(loglin)
predict.pts <- predict(loglin, newdata = test, type="response")
order <- as.integer(names(sort(predict.pts, decreasing=T)))
sorted.pts <- test[order,c("name", "division", "conference", "MadePlayoffs")]

# --- justification detour ---

# why poisson?
hist(train$PTS, prob=T)
hist(rnorm(1000, mean=mean(train$PTS), sd=sqrt(var(train$PTS))), prob=T, add=T, col=rgb(1, 0, 0, .3))
hist(rpois(1000, mean(train$PTS)), prob=T, add=T, col=rgb(0, 1, 0, .3))
# whoops this is bad don't show this lol

# our response is a nonnegative integer, so a poisson model makes some sense?
# i remembered negative binomial models: according to google they can help with overdispersion (and our data is overdispersed)
nb <- glm.nb(PTS ~ AvAge + SOS + division + cap_space + ltir, data = train)
# the Poisson model predictions are very similar. will be good to mention in the slides, though.
# there are other options but we probably don't need to keep going with that

# --- back to scheduled programming ---

# top 3 teams each division, then the next two highest in the conference
sorted.pts[sorted.pts$division == "atlantic",] # Atlantic: Lightning, Senators, Sabres
sorted.pts[sorted.pts$division == "metropolitan",] # Metro: Hurricanes, Capitals, Islanders
sorted.pts[sorted.pts$division == "central",] # Central: Avalanche, Stars, Wild
sorted.pts[sorted.pts$division == "pacific",] # Pacific: Oilers, Knights, Kings
sorted.pts[sorted.pts$conference == "eastern",] # Eastern Wildcards: Bruins, Panthers
sorted.pts[sorted.pts$conference == "western",] # Western Wildcards: Mammoth, Ducks

# real playoffs:
# Eastern Conference:
#   Atlantic: Sabres, Lightning, Canadiens
#   Metropolitan: Hurricanes, Penguins, Flyers
#   Wildcards: Bruins, Senators
# Western Conference:
#   Central: Avalanche, Stars, Wild
#   Pacific: Knights, Oilers, Ducks
#   Wildcards: Mammoth, Kings

# performance (ignoring wildcard mismatch, just whether they really made playoffs or not)
# Eastern Conference:
#   correctly predicted 5 of 8 teams.
# Western Conference:
#   correctly predicted 8 of 8 teams (and mostly in order!)


# what if we bin wins and then try to predict the bin?
# this is harder to convert into whether or not they made playoffs, but:
xtabs(~ MadePlayoffs + W.binned, data = train)
# 0/19 teams in the lowest bin made the playoffs, 6/49 in the second-lowest, 51/53 in the second highest, and 7/7 in the highest.
# predicting the top two bins correctly would already be good
library(nnet)
multinom.W <- multinom(W.binned ~ AvAge + SOS + division +  cap_space + ltir, data = train)
summary(multinom.W)
pred.wbin <- predict(multinom.W, newdata = test)
test.by.w <- cbind(test[,c("name", "W.binned", "MadePlayoffs")], predicted.bin = pred.wbin)
View(test.by.w)
# not great. no predicted teams in the highest bucket. buckets are bad, maybe?

# make buckets have equal number of observations
test.adj <- test %>%
  mutate(W.binned.adj = ntile(W, 4))
train.adj <- train %>%
  mutate(W.binned.adj = ntile(W, 4))

multinom.W.adj <- multinom(W.binned.adj ~ AvAge + SOS + division +  cap_space + ltir, data = train.adj)
summary(multinom.W.adj)
cbind(w.bin = test.adj$W.binned.adj, 
      predicted.bin = predict(multinom.W.adj, newdata = test), 
      playoffs = test.adj$MadePlayoffs)
# just by visual inspection, this sucked. worth mentioning but not including
# i'm guessing that the point values are so closely clustered that the buckets are too wide when basing them on the range of the data, or too small when breaking them into equal-sized groups.
# in the first case, we limit our training data for certain bins. in the second, the groups become too hard to discern

# seems to me like our best model is the points loglinear model where we convert to playoffs by hand.
# hard to tell without more data. 
