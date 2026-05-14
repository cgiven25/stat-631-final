library(ggplot2)

# 2025-2026 season
test <- readRDS("test.RDS")

# 2021-2025 seasons
train <- readRDS("train.RDS")

# all
all_data <- rbind(test, train)

means_data <- all_data %>%
  group_by(MadePlayoffs) %>%
  summarize(m_AvAge = mean(AvAge),
            m_SOS = mean(SOS),
            m_cap_space = mean(cap_space),
            m_ltir = mean(ltir))

means_data_season <- all_data %>%
  group_by(MadePlayoffs, season) %>%
  summarize(m_AvAge = mean(AvAge),
            m_SOS = mean(SOS),
            m_cap_space = mean(cap_space),
            m_ltir = mean(ltir))

means_data_season_division <- all_data %>%
  group_by(MadePlayoffs, season, division) %>%
  summarize(m_PTS = mean(PTS))

# averages for playoffs/not playoffs as a whole (i think years maybe more interesting)
ggplot(means_data, aes(x=MadePlayoffs, y=m_AvAge, fill=as.factor(MadePlayoffs))) + geom_col()
ggplot(means_data, aes(x=MadePlayoffs, y=m_SOS, fill=as.factor(MadePlayoffs))) + geom_col()
ggplot(means_data, aes(x=MadePlayoffs, y=m_cap_space, fill=as.factor(MadePlayoffs))) + geom_col()
ggplot(means_data, aes(x=MadePlayoffs, y=m_ltir, fill=as.factor(MadePlayoffs))) + geom_col()

# averages for playoffs/not playoffs by season
ggplot(data=means_data_season) +
  geom_col(aes(x=season, y=m_AvAge, fill=as.factor(MadePlayoffs)), position = position_dodge(1)) +
  geom_hline(yintercept=c(28.94125,28.24750), color=c("blue","red"), lty = "dashed") +
  labs(title = "Average Team Age, per Season, grouped by Make/Not Make Playoffs", x="Season", y="Average Age (Years)", fill="MadePlayoffs")

ggplot(data=means_data_season) +
  geom_col(aes(x=season, y=m_SOS, fill=as.factor(MadePlayoffs)), position = position_dodge(1)) +
  geom_hline(yintercept=c(-0.020625,0.019875), color=c("blue","red"), lty = "dashed") +
  labs(title = "Average Strength of Schedule, per Season, grouped by Make/Not Make Playoffs", x="Season", y="Average Strength of Season", fill="MadePlayoffs")

ggplot(data=means_data_season) +
  geom_col(aes(x=season, y=m_cap_space, fill=as.factor(MadePlayoffs)), position = position_dodge(1)) +
  geom_hline(yintercept=c(245307.1,5247656.7), color=c("blue","red"), lty = "dashed") +
  labs(title = "Average Cap Space, per Season, grouped by Make/Not Make Playoffs", x="Season", y="Average Cap Space (Dollars)", fill="MadePlayoffs")

ggplot(data=means_data_season) +
  geom_col(aes(x=season, y=m_ltir, fill=as.factor(MadePlayoffs)), position = position_dodge(1)) +
  geom_hline(yintercept=c(4576533,2523039), color=c("blue","red"), lty = "dashed") +
  labs(title = "Average Cap in LTIR, per Season, grouped by Make/Not Make Playoffs", x="Season", y="Average Cap Space in LTIR (Dollars)", fill="MadePlayoffs")


ggplot(data=means_data_season_division) +
  geom_col(aes(x=season, y=m_PTS, fill=as.factor(MadePlayoffs)), position=position_dodge(1)) +
  facet_grid(.~division) +
  labs(title = "Average Points in Division, per Season, grouped by Make/Not Make Playoffs", x="Season", y="Average Points", fill="MadePlayoffs") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  coord_cartesian(ylim = c(50, NA))
