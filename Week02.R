# ベルギー人のみのデータ抽出
data <- read.csv("/Users/aoikawahara/Documents/Leuven/03_From Problem to Analysis/Assignments/Week01/ESS10.csv")
be_data <- subset(data, cntry == "BE")

# クリーニング済みReligiosityデータの格納
rlgsty_data <- subset(be_data$rlgdgr, be_data$rlgdgr < 11)


# B40 imsmetn - Allow many/few immigrants of same race/ethnic group as majority
b40_data <- subset(be_data$imsmetn, be_data$imsmetn < 5)
b40_responses <- as.data.frame(table(b40_data))
colnames(b40_responses) <- c("Response", "Count")
b40_responses$Percent <- b40_responses$Count / sum(b40_responses$Count) * 100

ggplot(b40_responses, aes(x = "", y = Count, fill = Response)) +
  geom_bar(stat = "identity", position = "fill") +
  ggtitle("Allow many/few immigrants of same race/ethnic group as majority?") +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(labels = c("1" = "Allow many to come live here", "2" = "Allow some", "3" = "Allow a few", "4" = "Allow none")) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), position = position_fill(vjust = 0.5))

# B41 imdfetn - Allow many/few immigrants of different race/ethnic group from majority
b41_data <- subset(be_data$imdfetn, be_data$imdfetn < 5)
b41_responses <- as.data.frame(table(b41_data))
colnames(b41_responses) <- c("Response", "Count")
b41_responses$Percent <- b41_responses$Count / sum(b41_responses$Count) * 100

ggplot(b41_responses, aes(x = "", y = Count, fill = Response)) +
  geom_bar(stat = "identity", position = "fill") +
  ggtitle("Allow many/few immigrants of different race/ethnic group from majority?") +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(labels = c("1" = "Allow many to come live here", "2" = "Allow some", "3" = "Allow a few", "4" = "Allow none")) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), position = position_fill(vjust = 0.5))


# B42 impcntr – Allow many/few immigrants from poorer countries outside Europe
b42_data <- subset(be_data$impcntr, be_data$impcntr < 5)
b42_responses <- as.data.frame(table(b42_data))
colnames(b42_responses) <- c("Response", "Count")
b42_responses$Percent <- b42_responses$Count / sum(b42_responses$Count) * 100

ggplot(b42_responses, aes(x = "", y = Count, fill = Response)) +
  geom_bar(stat = "identity", position = "fill") +
  ggtitle("Allow many/few immigrants from poorer countries outside Europe?") +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(labels = c("1" = "Allow many to come live here", "2" = "Allow some", "3" = "Allow a few", "4" = "Allow none")) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), position = position_fill(vjust = 0.5))


# B43 imbgeco – Immigration bad or good for country’s economy
b43_data <- subset(be_data$imbgeco, be_data$imbgeco < 11)
b43_responses <- as.data.frame(table(b43_data))
colnames(b43_responses) <- c("Response", "Count")
b43_responses$Percent <- b43_responses$Count / sum(b43_responses$Count) * 100

ggplot(b43_responses, aes(x = Response, y = Count)) +
  geom_bar(stat = "identity", fill = "#979797") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), position = position_stack(vjust = 1.05)) + 
  ggtitle("Immigration bad or good for country’s economy?")


# B44 imueclt – Country’s cultural life undermined or enriched by immigrants
b44_data <- subset(be_data$imueclt, be_data$imueclt < 11)
b44_responses <- as.data.frame(table(b44_data))
colnames(b44_responses) <- c("Response", "Count")
b44_responses$Percent <- b44_responses$Count / sum(b44_responses$Count) * 100

ggplot(b44_responses, aes(x = Response, y = Count)) +
  geom_bar(stat = "identity", fill = "#979797") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), position = position_stack(vjust = 1.05)) + 
  ggtitle("Country’s cultural life undermined or enriched by immigrants?")


# B45 imwbcnt – Immigrants make country worse or better place to live
b45_data <- subset(be_data$imwbcnt, be_data$imwbcnt < 11)
b45_responses <- as.data.frame(table(b45_data))
colnames(b45_responses) <- c("Response", "Count")
b45_responses$Percent <- b45_responses$Count / sum(b45_responses$Count) * 100

ggplot(b45_responses, aes(x = Response, y = Count)) +
  geom_bar(stat = "identity", fill = "#979797") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), position = position_stack(vjust = 1.05)) + 
  ggtitle("Immigrants make country worse or better place to live?")








