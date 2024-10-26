#### Libraries ####
library("dplyr")
library("ggplot2")
library("parallel")
library("psych")
library("tidyr")
library("rlang")
library("lavaan")
library("semPlot")



#### イタリア人データフレーム作成 ####
df <- read.csv("/Users/aoikawahara/Documents/Leuven/03_From Problem to Analysis/Assignments/Week01/ESS10.csv")
df <- subset(df, cntry == "IT")
df <- select(df, agea, edlveit, hinctnta, imsmetn, imdfetn, impcntr, imbgeco, imueclt, imwbcnt)



#### 欠損データの削除 ####
df <- subset(df, 
             df$agea < 999 &
               df$edlveit < 22 &
               df$hinctnta < 11 &
               df$imsmetn < 5 &
               df$imdfetn < 5 &
               df$impcntr < 5 &
               df$imbgeco < 11 &
               df$imueclt < 11 &
               df$imwbcnt < 11)



#### データの反転 ####
df$imbgeco <- 10 - df$imbgeco
df$imueclt <- 10 - df$imueclt
df$imwbcnt <- 10 - df$imwbcnt



#### correlation matrix ####
correlation <- cor(df, method = "spearman")
correlation



#### Anti_immスコア算出 ####
df_imm <- select(df, imsmetn, imdfetn, impcntr, imbgeco, imueclt, imwbcnt)

alpha(df_imm)

fa.parallel(df_imm, fa = "fa", fm = "pa")
paf_imm <- fa(df_imm,
              fm = "pa",
              nfactors = 2,
              rotate = "oblimin")
paf_imm

df_imm <- cbind(df_imm, paf_imm$scores)
df_imm$combined_fs <- (0.52 * df_imm$PA1) + (0.48 * df_imm$PA2)



#### 問題用データフレーム ####
df1 <- select(df, agea, edlveit, hinctnta)
df1 <- cbind(df1, df_imm$combined_fs)
colnames(df1) <- c("Age", "Educ", "Income", "Anti_imm")



#### Direct causal relation ####
model1 <- lm(df1$Anti_imm ~ df1$Age)
summary(model1)

model2 <- lm(df1$Educ ~ df1$Age)
summary(model2)



#### Additive model ####
model3 <- lm(df1$Anti_imm ~ df1$Age + df1$Educ)
summary(model3)



#### Moderated causal relation (調整効果) - Interaction model ####
model4 <- lm(df1$Anti_imm ~ df1$Age * df1$Educ)
summary(model4)

df1 <- df1 %>%
  mutate(Age_group = case_when(df1$Age < 40 ~ "~39",
                               df1$Age >= 40 & df1$Age < 60 ~ "40 ~ 59",
                               df1$Age >= 60 ~ "60~",))

interaction.plot(
  x.factor = df1$Educ,          # X軸の変数
  trace.factor = df1$Age_group, # 線で描き分ける変数
  response = df1$Anti_imm,      # y軸の変数
  fun = mean,                   # 平均値を図示
  type = "b",                   # 点と線で表すグラフを指定
  pch = 19,
  col = c("red", "blue", "green"))



#### Indirect causal relation (媒介効果) 1 - SEM ####
model5 <- '
  # 直接効果
  Anti_imm ~ c*Age
  
  # 間接効果
  Educ ~ a*Age
  Anti_imm ~ b*Educ
  
  # 媒介効果の計算
  ab := a*b
  total := c + (a*b)'

fit5 <- sem(model5, data = df1)
summary(fit5)

# semPaths(fit5,
#         whatLabels = "std",
#         style = "lisrel",
#         edge.color = "black",
#         layout = "tree",
#         rotation = 2,
#         edge.label.cex = 1)



#### Indirect causal relation (媒介効果) 2 - SEM ####
model6 <- '
  # 直接効果
  Anti_imm ~ c*Age
  
  # 間接効果
  Income ~ a*Age
  Anti_imm ~ b*Income
  
  # 媒介効果の計算
  ab := a*b
  total := c + (a*b)'

fit6 <- sem(model6, data = df1)
summary(fit6)



