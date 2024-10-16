#### Libraries ####
library("dplyr")
library("ggplot2")
library("parallel")
library("psych")
library("tidyr")


#### イタリア人データフレーム作成 ####
df <- read.csv("/Users/aoikawahara/Documents/Leuven/03_From Problem to Analysis/Assignments/Week01/ESS10.csv")
df <- subset(df, cntry == "IT")
df <- select(df, rlgdgr, rlgatnd, pray, imsmetn, imdfetn, impcntr, imbgeco, imueclt, imwbcnt)

##### 欠損データの削除 ####
df <- subset(df,
             df$rlgdgr < 11 &
             df$rlgatnd < 8 &
             df$pray < 8 &
             df$imsmetn < 5 &
             df$imdfetn < 5 &
             df$impcntr < 5 &
             df$imbgeco < 11 &
             df$imueclt < 11 &
             df$imwbcnt < 11)

##### Counting responses #####
count_rlgdgr <- as.data.frame(table(df$rlgdgr))
colnames(count_rlgdgr) <- c("Response", "Count")

count_rlgatnd <- as.data.frame(table(df$rlgatnd))
colnames(count_rlgatnd) <- c("Response", "Count")

count_pray <- as.data.frame(table(df$pray))
colnames(count_pray) <- c("Response", "Count")

count_imsmetn <- as.data.frame(table(df$imsmetn))
colnames(count_imsmetn) <- c("Response", "Count")

count_imdfetn <- as.data.frame(table(df$imdfetn))
colnames(count_imdfetn) <- c("Response", "Count")

count_impcntr <- as.data.frame(table(df$impcntr))
colnames(count_impcntr) <- c("Response", "Count")

count_imbgeco <- as.data.frame(table(df$imbgeco))
colnames(count_imbgeco) <- c("Response", "Count")

count_imueclt <- as.data.frame(table(df$imueclt))
colnames(count_imueclt) <- c("Response", "Count")

count_imwbcnt <- as.data.frame(table(df$imwbcnt))
colnames(count_imwbcnt) <- c("Response", "Count")

#### Data visualization - descriptives ####
ggplot(count_rlgdgr, aes(x = Response, y = Count)) +
  geom_bar(stat = "identity") +
  xlab("Response") +
  ylab("Count") +
  theme(legend.position = "bottom") +
  ggtitle("How religious do you think you are?")



action_df <- data.frame(
  Response = c("Every day", "More than once a week", "Once a week", "At least once a month", "Only on special holidays", "Less often", "Never"),
  Attend = count_rlgatnd$Count,
  Pray = count_pray$Count
)
action_df <- gather(action_df, key = "Question", value = "Count", Attend:Pray)

ggplot(action_df, aes(x = Response, y = Count, fill = Question)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Response") +
  ylab("Count") +
  scale_x_discrete(limit = c("Never", "Less often", "Only on special holidays", "At least once a month", "Once a week", "More than once a week", "Every day")) +
  theme(legend.position = "bottom") +
  ggtitle("How often do you attend religious services/pray?")



allow_df <- data.frame(
  Response = c("Many", "Some", "A few", "None"),
  SameGroup = count_imsmetn$Count,
  Different = count_imdfetn$Count,
  Poorer = count_impcntr$Count
)
allow_df <- gather(allow_df, key = "Question", value = "Count", SameGroup:Poorer)

ggplot(allow_df, aes(x = Response, y = Count, fill = Question)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Response") +
  ylab("Count") +
  theme(legend.position = "bottom") +
  scale_x_discrete(limit = c("Many", "Some", "A few", "None")) +
  ggtitle("How many allow to come and live in Italy?")



impact_df <- data.frame(
  Response = count_imbgeco$Response,
  Economy = count_imbgeco$Count,
  Culture = count_imueclt$Count,
  Country = count_imwbcnt$Count
)
impact_df <- gather(impact_df, key = "Question", value = "Count", Economy:Country)

ggplot(impact_df, aes(x = Response, y = Count, fill = Question)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Response") +
  ylab("Count") +
  theme(legend.position = "bottom") +
  ggtitle("The impact of immigration on Italy?")
#### データの反転 ####
df$rlgatnd <- 8 - df$rlgatnd
df$pray <- 8 - df$pray
df$imbgeco <- 10 - df$imbgeco
df$imueclt <- 10 - df$imueclt
df$imwbcnt <- 10 - df$imwbcnt

#### Correlation matrix - all questions ####
correlation <- cor(df, method = "spearman")
correlation

##### Religiosity - PAF実行&Score算出 ####
rlg_df <- select(df, rlgdgr, rlgatnd, pray)
fa.parallel(rlg_df, fa = "fa", fm = "pa")
rlg_paf <- fa(rlg_df,
              fm = "pa",
              nfactors = 1,
              rotate = "oblimin")
rlg_paf


df_with_rlgscores <- cbind(df, rlg_paf$scores)

ggplot(df_with_rlgscores, aes(x = PA1)) +
  geom_histogram(binwidth = 0.1) +
  xlab("Religiosity Score") +
  ylab("Count") +
  ggtitle("Histogram of Religiosity Scores (PAF)")

#### Immigrants - PAF実行&Score算出 ####
imm_df <- select(df, imsmetn, imdfetn, impcntr, imbgeco, imueclt, imwbcnt)
fa.parallel(imm_df, fa = "fa", fm = "pa")
imm_paf <- fa(imm_df,
              fm = "pa",
              nfactors = 2,
              rotate = "oblimin")
imm_paf


df_with_immscores <- cbind(df, imm_paf$scores)
df_with_immscores$combined_fs <- (0.52 * df_with_immscores$PA1) + (0.48 * df_with_immscores$PA2)

ggplot(df_with_immscores, aes(x = combined_fs)) +
  geom_histogram(binwidth = 0.1) +
  xlab("Anti-immigration Score") +
  ylab("Count") +
  ggtitle("Histogram of Anti-immigration Scores (PAF)")

#### 散布図 - ReligiosityとImmigrantsの関係 ####
scores_df <- data.frame(
  Religiosity = df_with_rlgscores$PA1,
  AntiImmigrants = df_with_immscores$combined_fs
)

ggplot(scores_df, aes(x = Religiosity, y = AntiImmigrants)) +
  geom_point() +
  xlab("Religiosity") +
  ylab("Anti-immigrants")

