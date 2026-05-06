# 1. TEMİZLİK VE KÜTÜPHANELER
rm(list = ls())
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readxl")) install.packages("readxl")

library(tidyverse)
library(readxl)

# 2. DOSYA SEÇİMİ
print("Lütfen orijinal EXCEL (.xlsx) dosyasını seçiniz:")
dosya_yolu <- file.choose()

# 3. VERİYİ OKUMA
df <- read_excel(dosya_yolu, skip = 2, col_names = FALSE)

colnames(df) <- c("Yil", "Il", "Toplam", "0-14", "15-19", "20-24", "25-29", 
                  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
                  "60-64", "65-69", "70-74", "75+")

# 4. VERİ DÜZENLEME
yas_sirasi <- c("0-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
                "70-74", "75+")

df_grafik <- df %>%
  mutate(Il = str_trim(Il)) %>%
  filter(Il == "Turkiye" & Yil >= 2021 & Yil <= 2024) %>%
  select(-Toplam, -Il) %>%
  pivot_longer(cols = -Yil, names_to = "Yas_Grubu", values_to = "Kisi_Sayisi") %>%
  mutate(
    Kisi_Sayisi = as.numeric(as.character(Kisi_Sayisi)),
    Yas_Grubu = factor(Yas_Grubu, levels = yas_sirasi),
    Yil = factor(Yil, levels = c("2021", "2022", "2023", "2024"))
  ) %>%
  filter(!is.na(Kisi_Sayisi))

# 5. GRAFİĞİ ÇİZ (FİNAL VE DÜZENLİ BAŞLIKLI)
ggplot(df_grafik, aes(x = Yas_Grubu, y = Kisi_Sayisi, fill = Yil)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  
  # Sayı etiketleri (Biraz daha küçük ve temiz)
  geom_text(aes(label = Kisi_Sayisi), 
            position = position_dodge(width = 0.9), 
            vjust = 0.5, 
            hjust = -0.3, 
            angle = 90, 
            size = 2.8) + 
  
  scale_fill_manual(values = c("2021" = "#E41A1C", "2022" = "#377EB8", 
                               "2023" = "#4DAF4A", "2024" = "#984EA3")) + 
  theme_minimal() +
  
  # Başlığı iki satıra böldük (\n kullanarak)
  labs(
    title = "Yurt Genelinde İntihar Oranları (2021-2024)",
    x = "Yaş Grupları",
    y = "Kişi Sayısı",
    fill = "Yıl"
  ) +
  
  # Grafiğin üst kısmını daha fazla genişletiyoruz (Sayılar sığsın diye)
  expand_limits(y = max(df_grafik$Kisi_Sayisi, na.rm = TRUE) * 1.3) +
  
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # Ortalanmış başlık
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "bottom"
  )