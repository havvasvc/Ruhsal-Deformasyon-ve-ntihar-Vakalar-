library(ggplot2)

# 1. Dosyayı seç (Pencere açılmazsa her zamanki gibi görev çubuğuna bak)
raw_lines <- readLines(file.choose(), warn = FALSE)

# 2. Sayıyla başlayan satırları ayıkla
data_lines <- raw_lines[grep("^[0-9]", raw_lines)]

# 3. Veriyi temizle ve ayır (Hem virgül hem noktalı virgül için)
clean_rows <- lapply(strsplit(data_lines, "[,;]"), function(x) {
  nums <- as.numeric(gsub("[^0-9.]", "", x))
  nums <- nums[!is.na(nums)]
  return(nums)
})

# 4. Tabloyu oluştur
veri_clean <- as.data.frame(do.call(rbind, clean_rows))
colnames(veri_clean) <- c("Yil", "Vaka", "Hiz")

# 5. GRAFİK (Sadece renk değişimi, boyutlar sabit)
if(nrow(veri_clean) > 0) {
  ggplot(veri_clean, aes(x = Yil, y = Vaka)) +
    geom_line(color = "gray80", linewidth = 0.5) + 
    
    # DÜZELTME: size=5 aes() dışında (sabit boyut), color=Hiz aes() içinde (değişken renk)
    geom_point(aes(color = Hiz), size = 5, alpha = 0.8) +
    
    scale_color_gradient(low = "yellow", high = "red") +
    scale_x_continuous(breaks = seq(min(veri_clean$Yil), max(veri_clean$Yil), 1)) +
    
    # Artık guides(size="none") yazmaya gerek yok çünkü boyutu değişkene bağlamadık
    labs(title = "İntihar Vakaları ve Kaba Hız Analizi (2001-2024)",
         x = "Yıl", 
         y = "Vaka Sayısı", 
         color = "Kaba İntihar Hızı") + 
    
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
} else {
  print("HATA: Veri hala okunamadı!")
}