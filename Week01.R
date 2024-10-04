# ベルギー人のみのデータ抽出
data <- read.csv("/Users/aoikawahara/Documents/Leuven/03_From Problem to Analysis/Assignments/Week01/ESS10.csv")
belgium_data <- subset(data, cntry == "BE")


# Ageデータ
current_year <- 2024
birth_year_data <- belgium_data$yrbrn
age_data <- current_year - birth_year_data

filtered_age_data <- subset(age_data, age_data > 0)

summary(filtered_age_data)
sd(filtered_age_data)

hist(filtered_age_data,
     main = "Histogram of age",
     xlab = "Age",
     ylab = "Frequency",
     xlim = c(0,100))


# Health statusデータ
health_data <- belgium_data$health
health_responses <- as.data.frame(table(health_data))
colnames(health_responses) <- c("Responses", "count")
health_responses$percent <- health_responses$count / sum(health_responses$count) * 100

summary(health_data)
sd(health_data)

ggplot(health_responses, aes(x = "", y = count, fill = Responses)) +
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  xlab("") +
  ylab("Health status") +
  scale_y_continuous(labels = percent) +
  scale_fill_grey(labels = c("5" = "5 - Very bad", "4" = "4 - Bad", "3" = "3 - Fair", "2" = "2 - Good", "1" = "1 - Very good")) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_fill(vjust = 0.5))


# Religiosityデータ
religiosity_data <- belgium_data$rlgdgr
filtered_religiosity_data <- subset(religiosity_data, religiosity_data < 11)

religiosity_responses <- as.data.frame(table(filtered_religiosity_data))
colnames(religiosity_responses) <- c("Responses", "count")
religiosity_responses$percent <- religiosity_responses$count / sum(religiosity_responses$count) * 100

# ggplot(religiosity_responses, aes(x = "", y = count, fill = Responses)) +
#   geom_bar(stat = "identity", position = "fill") +
#  coord_flip() +
#  xlab("") +
#  ylab("Religiosity") +
#  scale_y_continuous(labels = percent) +
#  scale_fill_grey() +
#  theme(legend.position = "bottom") +
#  guides(fill = guide_legend(reverse = TRUE))

hist(filtered_religiosity_data,
     main = "Histogram of religiosity",
     xlab = "Religiosity",
     ylab = "Frequency",
     xlim = c(0,11))

summary(filtered_religiosity_data)
sd(filtered_religiosity_data)

# 年齢とHealth statusの相関
age_health_data <- data.frame(c(age_data), c(health_data))
colnames(age_health_data) <- c("Age", "Health status")
valid_age_health_data <- filter(age_health_data, age_data > 0)

ggplot(valid_age_health_data, aes(x = factor(`Health status`) , y = Age)) +
  geom_boxplot() +
  geom_jitter(color = "#979797", size = 0.5) +
  xlab("Health status") +
  ylab("Age") +
  coord_flip()

correlation_age_health <- cor.test(valid_age_health_data$Age, valid_age_health_data$`Health status`, method = "spearman")
print(correlation_age_health)

# Religiosityと年齢の相関
religiosity_age_data <- data.frame(c(age_data), c(religiosity_data))
colnames(religiosity_age_data) <- c("Age", "Religiosity")
valid_religiosity_age_data <- filter(religiosity_age_data, age_data > 0 & religiosity_data < 11)
valid_religiosity_age_data$Religiosity <- factor(valid_religiosity_age_data$Religiosity)

ggplot(valid_religiosity_age_data, aes(x = Religiosity , y = Age)) +
  geom_boxplot() +
  geom_jitter(color = "#979797", size = 0.5) +
  xlab("Religiosity") +
  ylab("Age") +
  coord_flip()

correlation_religiosity_age <- cor.test(as.numeric(valid_religiosity_age_data$Religiosity), valid_religiosity_age_data$Age, method = "spearman")
print(correlation_religiosity_age)

# ReligiosityとHealth statusの相関
religiosity_health_data <- data.frame(c(religiosity_data), c(health_data))
colnames(religiosity_health_data) <- c("Religiosity", "Health status")
valid_religiosity_health_data <- filter(religiosity_health_data, religiosity_data < 11)

ggplot(valid_religiosity_health_data, aes(x = Religiosity, y = factor(`Health status`))) +
  geom_boxplot() +
  geom_jitter(color = "#979797", size = 0.5) +
  xlab("Religiosity") +
  ylab("Health status")

correlation_religiosity_health <- cor.test(valid_religiosity_health_data$Religiosity, valid_religiosity_health_data$`Health status`, method = "spearman")
print(correlation_religiosity_health)


# 順序ロジスティック回帰 - Ordinal Logistic Regression

df <- data.frame(c(health_data), c(religiosity_data), c(age_data))
colnames(df) <- c("Health status", "Religiosity", "Age")
valid_df <- filter(df, religiosity_data < 11 & age_data > 0)

model_olr <- clm(as.factor(`Health status`) ~ Religiosity + Age, data = valid_df)
result_olr <- summary(model_olr)
print(result_olr)


