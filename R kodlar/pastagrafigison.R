# Gerekli kütüphaneleri yükleyelim
library(ggplot2)
library(ggrepel)
library(dplyr)

# 1. Veri setini oluşturalım
data <- data.frame(
  Egitim_Durumu = c("İlkokul", "İlkogretim", "Yükseköğretim", "Ortaokul", 
                    "Okuma Yazma Bilmeyen", "Okul Bitirmeyen", "Lise", "Bilinmeyen"),
  Sayi = c(594, 342, 840, 1099, 47, 96, 1423, 19),
  Yuzde = c(13.3, 7.7, 18.8, 24.6, 1.1, 2.2, 31.9, 0.4)
)

# 2. Pasta grafiği için dilim sıralamasını ve etiket konumlarını ayarlayalım
data <- data %>%
  mutate(Egitim_Durumu = factor(Egitim_Durumu, levels = rev(Egitim_Durumu))) %>%
  arrange(desc(Egitim_Durumu)) %>%
  mutate(prop = Yuzde / sum(data$Yuzde) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.5 * prop)

# 3. Grafik Çizimi
ggplot(data, aes(x = "", y = Yuzde, fill = Egitim_Durumu)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() + # Arkaplanı temizler
  labs(title = "2024 Yılı Eğitim Durumuna Göre Dağılım",
       fill = "Eğitim Seviyesi") +
  
  # Yüzdelik etiketlerini ve çizgileri ekleyelim (ggrepel ile)
  geom_label_repel(aes(y = ypos, label = paste0("%", Yuzde)),
                   size = 4, 
                   fontface = "bold",
                   nudge_x = 0.8, 
                   show.legend = FALSE) +
  
  # Görsel özelleştirmeler
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.title = element_text(face = "bold")
  ) +
  
  # Renk paletini belirleyelim (İkinci resme yakın pastel tonlar)
  scale_fill_brewer(palette = "Pastel1")