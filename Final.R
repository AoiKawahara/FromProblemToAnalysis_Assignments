#### Libraries ####
library("dplyr")
library("ggplot2")
library("parallel")
library("psych")
library("tidyr")
library("rlang")
library("lavaan")
library("semPlot")
library("corrplot")


#### データフレーム作成 ####
df <- read.csv("/Users/aoikawahara/Documents/Leuven/03_From Problem to Analysis/Assignments/Week01/ESS10.csv")
df <- subset(df, cntry == "BE")
df <- select(df, agea,
             psppsgva, actrolga, psppipla,
             trstprl, trstlgl, trstplt, trstprt, trstep, trstun,
             stfgov, stfdem,
             vote)


#### 欠損データの削除 ####
df1 <- subset(df, 
             df$agea < 999 &
               df$psppsgva < 6 &
               df$actrolga < 6 &
               df$psppipla < 6 &
               df$trstprl < 11 &
               df$trstlgl< 11 &
               df$trstplt < 11 &
               df$trstprt < 11 &
               df$trstep < 11 &
               df$trstun < 11 &
               df$stfgov < 11 &
               df$stfdem < 11 &
               df$vote < 3)


#### データの反転 ####
df1$vote <- 2 - df1$vote
# 元 Yes = 1, No = 2
# 新 Yes = 1, No = 0


#### column名変更 ####
df1 <- df1 %>%
  rename(Age = agea,
         Say = psppsgva,
         Active = actrolga,
         Influence = psppipla,
         T_Parl = trstprl,
         T_Legal = trstlgl,
         T_Politicians = trstplt,
         T_Parties= trstprt,
         T_EU = trstep,
         T_UN = trstun,
         S_Gov = stfgov,
         S_Democ = stfdem,
         Vote = vote)


#### correlation matrix ####
c <- cor(df1, method = "spearman")
round(c,2)
corrplot(c, method = "color", addCoef.col = TRUE)


#### Trust 変数の作成 ####
df_trst <- select(df1, Say, Active, Influence,
                  T_Parl, T_Legal, T_Politicians, T_Parties, T_EU, T_UN,
                  S_Gov, S_Democ)

alpha(df_trst)

fa.parallel(df_trst, fa = "fa", fm = "pa")
paf_trst <- fa(df_trst,
              fm = "pa",
              nfactors = 4,
              rotate = "oblimin")
paf_trst

df_trst <- cbind(df_trst, paf_trst$scores)

head(df_trst)
df_trst$combined_fs <- (0.29 * df_trst$PA4) + (0.28 * df_trst$PA1) + (0.24 * df_trst$PA3) + (0.19 * df_trst$PA2)

ggplot(df_trst, aes(x = combined_fs)) +
  geom_histogram(binwidth = 0.1) +
  xlab("Trust Score") +
  ylab("Count") +
  ggtitle("Histogram of Trust Scores (PAF)")

# Active無しバージョン
df_trst2 <- select(df1, Say, Influence,
                  T_Parl, T_Legal, T_Politicians, T_Parties, T_EU, T_UN,
                  S_Gov, S_Democ)

alpha(df_trst2)

fa.parallel(df_trst2, fa = "fa", fm = "pa")
paf_trst2 <- fa(df_trst2,
               fm = "pa",
               nfactors = 4,
               rotate = "oblimin")
paf_trst2

# ActiveとT_ParlとT_Legal無しバージョン
df_trst3 <- select(df1, Say, Influence,
                   T_Politicians, T_Parties, T_EU, T_UN,
                   S_Gov, S_Democ)

alpha(df_trst3)

fa.parallel(df_trst3, fa = "fa", fm = "pa")
paf_trst3 <- fa(df_trst3,
                fm = "pa",
                nfactors = 4,
                rotate = "oblimin")
paf_trst3


#### モデリング用データフレーム(df2)作成 ####
df2 <- select(df1, Age, Vote)
df2 <- cbind(df2, df_trst$combined_fs)
colnames(df2) <- c("Age", "Vote", "Trust")

df2 <- df2 %>%
  mutate(Age_group = case_when(df2$Age < 40 ~ "~39",
                               df2$Age >= 40 & df2$Age < 60 ~ "40~59",
                               df2$Age >= 60 ~ "60~",))


#### Direct causal relation ####
# Binomial logistic regression model: Vote - Age
model1 <- glm(Vote ~ Age, data = df2, family = binomial(link = "logit"))
summary(model1)

df2$predl.model1 = predict.glm(model1) # logits
df2$predo.model1 = exp(df2$predl.model1) # odds
df2$predp.model1 = df2$predo.model1 / (1 + df2$predo.model1) # probabilities

ggplot(df2, aes(Age, predp.model1)) +
  geom_line() +
  ylab("Probability of Voting") +
  theme_bw()

# Binomial logistic regression model: Vote ~ Trust
model2 <- glm(Vote ~ Trust, data = df2, family = binomial(link = "logit"))
summary(model2)

df2$predl.model2 = predict.glm(model2) # logits
df2$predo.model2 = exp(df2$predl.model2) # odds
df2$predp.model2 = df2$predo.model2 / (1 + df2$predo.model2) # probabilities

ggplot(df2, aes(Trust, predp.model2)) +
  geom_line() +
  ylab("Probability of Voting") +
  theme_bw()


#### Moderated causal relation ####
model3 <- glm(Vote ~ Age * Trust, data = df2, family = binomial(link = "logit"))
summary(model3)

df2$predl.model3 = predict.glm(model3) # logits
df2$predo.model3 = exp(df2$predl.model3) # odds
df2$predp.model3 = df2$predo.model3 / (1 + df2$predo.model3) # probabilities

interaction.plot(
  x.factor = df2$Trust,                    # X軸の変数
  xlab = "Trust",
  trace.factor = df2$Age_group,            # 線で描き分ける変数
  response = df2$predp.model3,             # y軸の変数
  ylab = "Mean of Probability of Voting",
  fun = mean,                              # 平均値を図示
  type = "b",
  pch = 19,
  col = c("red", "blue", "green"))

