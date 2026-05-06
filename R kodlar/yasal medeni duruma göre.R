library(tidyverse)

# TÜİK 2001-2024 arası genel toplam verileri (Bilinmeyen = 506 olarak güncellendi)
genel_dagilim <- data.frame(
  Durum = c("Evli", "Hiç Evlenmedi", "Boşandı", "Eşi Öldü", "Bilinmeyen"),
  Vaka = c(38069, 30923, 5482, 3572, 506) # Senin veri setindeki gerçek genel toplamlar
)

# Grafik Çizimi
ggplot(genel_dagilim, aes(x = reorder(Durum, -Vaka), y = Vaka, fill = Durum)) +
  geom_bar(stat = "identity", color = "white", width = 0.7) +
  # Binlik ayraçlı (noktalı) vaka sayılarını sütun üzerine ekle
  geom_text(aes(label = format(Vaka, big.mark = ".")), vjust = -0.5, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("#fc8d62", "#66c2a5", "#e78ac3", "#8da0cb", "#a6d854")) +
  theme_minimal() +
  labs(title = "2001-2024 Dönemi Yasal Medeni Duruma Göre Toplam İntihar Vakaları",
       x = "Yasal Medeni Durum", 
       y = "Toplam Vaka Sayısı") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 10, color = "black"))