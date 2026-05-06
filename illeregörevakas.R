library(tidyverse)

dosya <- file.choose()
df <- read.csv(dosya, header = TRUE, skip = 1, sep = ",", stringsAsFactors = FALSE)
if(ncol(df) <= 1) { df <- read.csv(dosya, header = TRUE, skip = 1, sep = ";", stringsAsFactors = FALSE) }

colnames(df)[1] <- "Sehir"
colnames(df)[2] <- "Cinsiyet"

df_temiz <- df %>%
  mutate(Sehir = ifelse(Sehir == "" | is.na(Sehir), NA, Sehir)) %>%
  fill(Sehir, .direction = "down") %>%
  filter(Cinsiyet == "Toplam-Total" & !grepl("Türkiye", Sehir, ignore.case = TRUE) & Sehir != "") %>%
  pivot_longer(cols = 3:ncol(df), names_to = "Yil", values_to = "Vaka") %>%
  mutate(Vaka = as.numeric(gsub("[^0-9.]", "", Vaka))) %>%
  drop_na(Vaka)

ilk15 <- df_temiz %>% 
  group_by(Sehir) %>% 
  summarise(M = median(Vaka)) %>% 
  arrange(desc(M)) %>% 
  head(15) %>% 
  pull(Sehir)

ggplot(df_temiz %>% filter(Sehir %in% ilk15), aes(x = reorder(Sehir, Vaka, FUN = median), y = Vaka, fill = Sehir)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = 0.7) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, max(df_temiz$Vaka, na.rm=TRUE) + 50, by = 50)) +
  theme_minimal() +
  labs(title = "İllerin 2009-2024 Arası İntihar Vaka Dağılımı",
       x = "Şehirler", y = "Yıllık Vaka Sayısı") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text = element_text(face = "bold"))