# データフレーム作成
df <- read.csv("/Users/aoikawahara/Documents/Leuven/03_From Problem to Analysis/Assignments/Week01/ESS10.csv")
df <- subset(df, cntry == "BE")
df<- select(df, imsmetn, imdfetn, impcntr, imbgeco, imueclt, imwbcnt)

# 欠損データの削除 - 保持率96.5%
df1 <- subset(df, df$imsmetn < 5 & df$imdfetn < 5 & df$impcntr < 5 & df$imbgeco < 11 & df$imueclt < 11 & df$imwbcnt < 11)


# B40 imsmetn - Allow many/few immigrants of same race/ethnic group as majority
b40_responses <- as.data.frame(table(df1$imsmetn))
colnames(b40_responses) <- c("Response", "Count")
b40_responses$Percent <- b40_responses$Count / sum(b40_responses$Count) * 100

ggplot(b40_responses, aes(x = "", y = Count, fill = Response)) +
  geom_bar(stat = "identity", position = "fill") +
  ggtitle("Allow many/few immigrants of same race/ethnic group as majority?") +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(palette="Greys", labels = c("1" = "Allow many to come live here", "2" = "Allow some", "3" = "Allow a few", "4" = "Allow none")) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), position = position_fill(vjust = 0.5))

# B41 imdfetn - Allow many/few immigrants of different race/ethnic group from majority
b41_responses <- as.data.frame(table(df1$imdfetn))
colnames(b41_responses) <- c("Response", "Count")
b41_responses$Percent <- b41_responses$Count / sum(b41_responses$Count) * 100

ggplot(b41_responses, aes(x = "", y = Count, fill = Response)) +
  geom_bar(stat = "identity", position = "fill") +
  ggtitle("Allow many/few immigrants of different race/ethnic group from majority?") +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(palette="Greys", labels = c("1" = "Allow many to come live here", "2" = "Allow some", "3" = "Allow a few", "4" = "Allow none")) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), position = position_fill(vjust = 0.5))


# B42 impcntr – Allow many/few immigrants from poorer countries outside Europe
b42_responses <- as.data.frame(table(df1$impcntr))
colnames(b42_responses) <- c("Response", "Count")
b42_responses$Percent <- b42_responses$Count / sum(b42_responses$Count) * 100

ggplot(b42_responses, aes(x = "", y = Count, fill = Response)) +
  geom_bar(stat = "identity", position = "fill") +
  ggtitle("Allow many/few immigrants from poorer countries outside Europe?") +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(palette="Greys", labels = c("1" = "Allow many to come live here", "2" = "Allow some", "3" = "Allow a few", "4" = "Allow none")) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), position = position_fill(vjust = 0.5))


# B43 imbgeco – Immigration bad or good for country’s economy
b43_responses <- as.data.frame(table(df1$imbgeco))
colnames(b43_responses) <- c("Response", "Count")
b43_responses$Percent <- b43_responses$Count / sum(b43_responses$Count) * 100

ggplot(b43_responses, aes(x = Response, y = Count)) +
  geom_bar(stat = "identity", fill = "#979797") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), position = position_stack(vjust = 1.05)) + 
  ggtitle("Immigration bad or good for country’s economy?")


# B44 imueclt – Country’s cultural life undermined or enriched by immigrants
b44_responses <- as.data.frame(table(df1$imueclt))
colnames(b44_responses) <- c("Response", "Count")
b44_responses$Percent <- b44_responses$Count / sum(b44_responses$Count) * 100

ggplot(b44_responses, aes(x = Response, y = Count)) +
  geom_bar(stat = "identity", fill = "#979797") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), position = position_stack(vjust = 1.05)) + 
  ggtitle("Country’s cultural life undermined or enriched by immigrants?")


# B45 imwbcnt – Immigrants make country worse or better place to live
b45_responses <- as.data.frame(table(df1$imwbcnt))
colnames(b45_responses) <- c("Response", "Count")
b45_responses$Percent <- b45_responses$Count / sum(b45_responses$Count) * 100

ggplot(b45_responses, aes(x = Response, y = Count)) +
  geom_bar(stat = "identity", fill = "#979797") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), position = position_stack(vjust = 1.05)) + 
  ggtitle("Immigrants make country worse or better place to live?")


# B40-B42 How many allow to come and live
allow_df <- data.frame(
  Response = c("Many", "Some", "A few", "None"),
  B40 = b40_responses$Count,
  B41 = b41_responses$Count,
  B42 = b42_responses$Count
)
allow_df <- gather(allow_df, key = "Question", value = "Count", B40:B42)

ggplot(allow_df, aes(x = Response, y = Count, fill = Question)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Response") +
  ylab("Count") +
  theme(legend.position = "bottom") +
  scale_x_discrete(limit = c("Many", "Some", "A few", "None")) +
  scale_fill_discrete(name = "", labels = c(B40 = "the same race/ethnic group", B41 = "different race/ethnic group", B42 = "poorer countries")) +
  ggtitle("How many allow to come and live in Belgium?")


# B43-B45 The impact of immigration
impact_df <- data.frame(
  Response = b43_responses$Response,
  B43 = b43_responses$Count,
  B44 = b44_responses$Count,
  B45 = b45_responses$Count
)
impact_df <- gather(impact_df, key = "Question", value = "Count", B43:B45)

ggplot(impact_df, aes(x = Response, y = Count, fill = Question)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Response") +
  ylab("Count") +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "", labels = c(B43 = "Economy", B44 = "Culture", B45 = "Better place")) +
  ggtitle("The impact of immigration on Belgium?")


# 因子数の決定
# Correlation matrix
correlation <- cor(df1, method = "spearman")

# 固有値(eigenvalues)の算出とscree plot
eigen_df <- data.frame(
  Eigenvalue = eigen(correlation)$values,
  Factors = c("1", "2", "3", "4", "5", "6")
)

ggplot(eigen_df, aes(x = Factors, y = Eigenvalue)) +
  geom_point() +
  geom_line(group = 1) +
  xlab("Factors") +
  ylab("Eigenvalue") +
  ggtitle("Scree Plot of Eigenvalue")


# PCAによる因子分析
principal(df1, nfactors = 2, rotate = "promax")









