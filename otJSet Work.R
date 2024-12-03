library(ggplot2)
library(dplyr)

# Load the data
otjSet <- read.csv("****")
# Replace blank with NA
otjSet[otjSet == ""] <- NA

# Convert GP.WR to a numeric value
otjSet <- otjSet %>%
  mutate(GP.WR = as.numeric(gsub("%", "", GP.WR)) / 100)

  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Create bargraph comaring # Seen and Color. B is Black, U is Blue, G is Green, R is Red, W is White
  ggplot(otjSet, aes(x = Color, fill = Color)) +
    geom_bar() +
    theme_minimal() +
    labs(title = "Number of Cards per Color Combination",
         x = "Color",
         y = "Number of Cards Seen") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("B" = "black", "U" = "blue", "G" = "green", "R" = "red", "W" = "antiquewhite",
                                 "NA" = "gray", "BG" = "darkgreen", "BR" = "darkred", "RG" = "brown3",
                                 "UB" = "darkblue", "UBG" = "turquoise4", "UBR" = "darkorchid1",
                                 "UG" = "turquoise", "UR" = "blueviolet", "URG" = "brown4",
                                 "WB" = "antiquewhite4", "WBG" = "darkolivegreen3", "WBR" = "deeppink3",
                                 "WG" = "darkolivegreen1", "WR" = "deeppink1", "WRG" = "coral4",
                                 "WU" = "azure2", "WUB" = "cadetblue", "WUG" = "aquamarine3"))



splitTables <- split(otjSet, otjSet$Color)
# pull all list from split tables
BTable   <- otjSet %>% filter(Color == "B")
BGTable  <- otjSet %>% filter(Color == "BG")
BRTable  <- otjSet %>% filter(Color == "BR")
GTable   <- otjSet %>% filter(Color == "G")
RTable   <- otjSet %>% filter(Color == "R")
RGTable  <- otjSet %>% filter(Color == "RG")
UTable   <- otjSet %>% filter(Color == "U")
UBTable  <- otjSet %>% filter(Color == "UB")
UBGTable <- otjSet %>% filter(Color == "UBG")
UBRTable <- otjSet %>% filter(Color == "UBR")
UGTable  <- otjSet %>% filter(Color == "UG")
URTable  <- otjSet %>% filter(Color == "UR")
URGTable <- otjSet %>% filter(Color == "URG")
WTable   <- otjSet %>% filter(Color == "W")
WBTable  <- otjSet %>% filter(Color == "WB")
WBCTable <- otjSet %>% filter(Color == "WBC")
WGTable  <- otjSet %>% filter(Color == "WG")
WRTable  <- otjSet %>% filter(Color == "WR")
WRGTable <- otjSet %>% filter(Color == "WRG")
WUTable  <- otjSet %>% filter(Color == "WU")
WUBTable <- otjSet %>% filter(Color == "WUB")
WUGTable <- otjSet %>% filter(Color == "WUG")
NATable  <- otjSet %>% filter(is.na(Color) | Color == "NA")

# make a word cloud of the card names
library(wordcloud)

# Make x..Seen a numeric variable
otjSet$X..Seen <- as.numeric(otjSet$X..Seen)

# Create dataset of otjset that is only the top 100 valuse of X..Seen
otjSetTop100 <- head(otjSet[order(otjSet$X..Seen, decreasing = TRUE),], 100)

wordcloud(otjSetTop100$Name, otjSetTop100$X..Picked, max.words = 100, color = brewer.pal(9, "Paired"), scale = c(1.5, 0.00000000000000000000000000000000005))

# Create wordcloud for colors B, R, G, W, U, and "NA"
# Edit BTable x..Seen to be scaled down by 500
BTable$X..Seen <- BTable$X..Seen / 1000
wordcloud(BTable$Name, BTable$X..Seen, max.words = 500, colors = brewer.pal(9, "BrBG"), scale = c(3, 0.5), freq = "reversed_freq")
wordcloud(GTable$Name, GTable$X..Seen, max.words = 500, colors = brewer.pal(9, "YlGnBu"), scale = c(3, 0.5))
wordcloud(RTable$Name, RTable$X..Seen, max.words = 500, colors = brewer.pal(9, "YlOrRd"), scale = c(3, 0.5))
wordcloud(UTable$Name, UTable$X..Seen, max.words = 500, colors = brewer.pal(9, "Blues"), scale = c(3, 0.5))
wordcloud(WTable$Name, WTable$X..Seen, max.words = 500, colors = brewer.pal(9, "YlOrBr"), scale = c(3, 0.5))
wordcloud(NATable$Name, NATable$X..Seen, max.words = 500, colors = brewer.pal(9, "Dark2"), scale = c(3, 0.5))
