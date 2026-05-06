# 1. GEREKLİ KÜTÜPHANELER
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readxl")) install.packages("readxl")

library(tidyverse)
library(readxl)

# 2. DOSYA SEÇİMİ
print("Lütfen dosyanızı (.xlsx veya .csv) seçiniz...")
dosya_yolu <- file.choose()
uzanti <- tools::file_ext(dosya_yolu)

# 3. VERİYİ OKUMA
if (tolower(uzanti) %in% c("xlsx", "xls")) {
  df_raw <- read_excel(dosya_yolu, col_names = FALSE)
} else {
  # CSV ise virgül veya noktalı virgül ayrımını R'a bırakıyoruz
  df_raw <- read.csv(dosya_yolu, header = FALSE, sep = ",", stringsAsFactors = FALSE)
  if(ncol(df_raw) < 5) df_raw <- read.csv(dosya_yolu, header = FALSE, sep = ";", stringsAsFactors = FALSE)
}

# 4. AGRESİF VERİ TEMİZLEME
df_final <- df_raw %>%
  # Sadece ilk sütunu 2000-2024 arası yıl olan satırları tut (Başlıkları eler)
  filter(grepl("^(19|20)[0-9]{2}$", as.character(.[[1]]))) %>%
  
  # Sütunları sayıya çevir ve binlik noktaları temizle
  transmute(
    Yil = as.numeric(as.character(.[[1]])),
    Hastalik = as.numeric(gsub("[^0-9]", "", as.character(.[[4]]))),
    Aile     = as.numeric(gsub("[^0-9]", "", as.character(.[[5]]))),
    Gecim    = as.numeric(gsub("[^0-9]", "", as.character(.[[6]]))),
    Duygusal = as.numeric(gsub("[^0-9]", "", as.character(.[[8]]))),
    Egitim   = as.numeric(gsub("[^0-9]", "", as.character(.[[9]])))
  ) %>%
  
  # ZİKZAK ÇÖZÜMÜ: Her yıl için şehirleri değil, sadece Türkiye GENELİ (max) verisini al
  group_by(Yil) %>%
  summarise(across(everything(), ~max(., na.rm = TRUE))) %>%
  
  # Sıfır veya hatalı verileri temizle
  filter(!is.na(Yil)) %>%
  
  # Grafiğe uygun formata getir
  pivot_longer(cols = -Yil, names_to = "Neden", values_to = "Sayi")

# 5. PROFESYONEL GRAFİK ÇİZİMİ
if(nrow(df_final) > 0) {
  ggplot(df_final, aes(x = Yil, y = Sayi, color = Neden, group = Neden)) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 2.5) +
    # Renk paleti ve tema
    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"),
                       labels = c("Aile Gecimsizligi", "Duygusal İlişki", "Eğitim", "Gecim Zorluğu", "Hastalık")) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Yurt Geneli Intihar Nedenleri",
      x = "Yıl", y = "Kişi Sayısı", color = "Neden"
    ) +
    scale_x_continuous(breaks = seq(min(df_final$Yil), max(df_final$Yil), by = 2)) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank()
    )
} else {
  print("Hata: Veri ayıklanamadı. Lütfen dosyanın ilk sütununda yılların olduğundan emin olun.")
}