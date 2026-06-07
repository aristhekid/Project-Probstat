#===========================================================================
# PROYEK AKHIR PROBABILITAS DAN STATISTIKA
# Nama File : script/analisis_probstat.R
# Deskripsi : Analisis Data Pembangunan Wilayah di Indonesia
#===========================================================================

# Load library yang dibutuhkan
library(dplyr)
library(ggplot2)


#==========================================================
# 1.3.1 Memahami Dataset
#==========================================================

# Membaca dataset
data <- read.csv("dataset/pembangunan_wilayah_missing_outlier.csv")

# Eksplorasi struktur dan dimensi awal data
print("--- 6 Baris Teratas Dataset ---")
head(data)

print("--- Ringkasan Dataset ---")
summary(data)

print("--- Dimensi Dataset ---")
dim(data)

print("--- Struktur Variabel Dataset ---")
str(data)

#==========================================================
# 1.3.2 Statistika Deskriptif (Kondisi Data Awal)
#==========================================================

# Memisahkan variabel numerik saja untuk kalkulasi statistik
data_num <- data %>%
  select(where(is.numeric))

print("--- Ringkasan Statistika Deskriptif Awal ---")
summary(data_num)

# Kalkulasi parameter spesifik (Wajib na.rm = TRUE karena data masih ada NA)
sapply(data_num, mean, na.rm = TRUE) #Menghitung mean

sapply(data_num, median, na.rm = TRUE) #Menghitung median

sapply(data_num, sd, na.rm = TRUE) #Menghitung standar deviasi

sapply(data_num, var, na.rm = TRUE) #Menghitung varian

sapply(data_num, quantile, na.rm = TRUE) #Menghitung kuartil

#==========================================================
# 1.3.3 Analisis Missing Value
#==========================================================

print("--- Jumlah Missing Value per Kolom ---")
colSums(is.na(data))


print("--- Menampilkan total Missing value ---")
sum(is.na(data))

print("--- Persentase Missing Value (%) ---")
colSums(is.na(data))/nrow(data)*100

# PENANGANAN: Menggunakan Complete Case Analysis (na.omit) sesuai kodingan tim
data_bersih <- na.omit(data)


print("--- Validasi Missing Value Setelah Pembersihan ---")
colSums(is.na(data_bersih))


#==========================================================
# 1.3.4 Analisis Outlier 
#==========================================================

# Ambil data numerik yang sudah bebas dari missing value
data_num_bersih <- na.omit(data_num)

# Fungsi formal deteksi jumlah outlier berdasarkan metode IQR
cek_outlier <- function(x){
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  
  lower <- Q1 - 1.5*IQR
  upper <- Q3 + 1.5*IQR
  
  sum(x < lower | x > upper)
}

print("--- Jumlah Outlier per Variabel Sebelum Penanganan ---")
sapply(data_num_bersih, cek_outlier)

# Menjelaskan variabel yang memiliki outlier.

# TAMPILAN GRAFIK: Boxplot gabungan untuk mendeteksi outlier awal
boxplot(data_num_bersih$pdrb_perkapita,
        main = "Boxplot PDRB Per Kapita")

# Membuat boxplot untuk kemiskinan untuk menganalisis lebih lanjut
boxplot(data_num_bersih$kemiskinan,
        main = "Boxplot Kemiskinan")

# Membuat boxplot untuk Pengangguran untuk menganalisis lebih lanjut
boxplot(data_num_bersih $pengangguran,
        main = "Boxplot Pengangguran")



# PENANGANAN OUTLIER: Menggunakan Metode Winsorizing agar data tidak terbuang

# 1. PDRB Per Kapita
Q1 <- quantile(data_bersih$pdrb_perkapita, 0.25)
Q3 <- quantile(data_bersih$pdrb_perkapita, 0.75)
IQR <- Q3 - Q1

lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR

data_bersih$pdrb_perkapita <-
  pmin(pmax(data_bersih$pdrb_perkapita, lower), upper)

# 2. Kemiskinan
Q1 <- quantile(data_bersih$kemiskinan, 0.25)
Q3 <- quantile(data_bersih$kemiskinan, 0.75)
IQR <- Q3 - Q1

lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR

data_bersih$kemiskinan <-
  pmin(pmax(data_bersih$kemiskinan, lower), upper)

# 3. Pengangguran
Q1 <- quantile(data_bersih$pengangguran, 0.25)
Q3 <- quantile(data_bersih$pengangguran, 0.75)
IQR <- Q3 - Q1

lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR

data_bersih$pengangguran <-
  pmin(pmax(data_bersih$pengangguran, lower), upper)


print("--- Jumlah Outlier Setelah Penanganan (Winsorizing) ---")
hitung_outlier <- function(x){
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  
  sum(x < lower | x > upper)
}

hitung_outlier(data_bersih$pdrb_perkapita)
hitung_outlier(data_bersih$kemiskinan)
hitung_outlier(data_bersih$pengangguran)


#==========================================================
# 1.3.5 Visualisasi Data 
#==========================================================

# Agregasi data rata-rata per provinsi dan tahun untuk mempermudah visualisasi
filtered_mean_data <- data_bersih %>%
  group_by(provinsi, tahun) %>%
  summarise(
    pdrb = mean(pdrb_perkapita),
    kemiskinan = mean(kemiskinan),
    pengangguran = mean(pengangguran),
    ipm = mean(ipm),
    harapan_hidup = mean(harapan_hidup),
    lama_sekolah = mean(rata_lama_sekolah),
    internet = mean(akses_internet),
    jalan_baik = mean(jalan_baik),
    air_bersih = mean(air_bersih)
  )

# Visualisasi data 1: Histogram Persentase Pengangguran per Provinsi setiap tahun
ggplot(filtered_mean_data,
       aes(x = provinsi,
           y = pengangguran,
           fill = factor(tahun))) +
  geom_col(position = "dodge") +
  labs(
    title = "Persentase pengangguran per Provinsi setiap tahun",
    x = "Provinsi",
    y = "Total",
    fill = "Tahun"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c(
    "#FDE0DD",  # sangat muda
    "#FCAE91",
    "#FB6A4A",
    "#DE2D26",
    "#A50F15"   # merah tua
  ))

# Visualisasi data 2: Boxplot Distribusi Kemiskinan per Tahun
ggplot(data_bersih,
       aes(x = factor(tahun),
           y = kemiskinan,
           fill = factor(tahun))) +
  geom_boxplot() +
  labs(
    title = "Distribusi Kemiskinan per Tahun",
    x = "Tahun",
    y = "Kemiskinan (%)"
  ) +
  theme_minimal()

# Visualisasi data 3: Line Chart PDRB per Tahun
ggplot(filtered_mean_data,
       aes(x = tahun,
           y = pdrb,
           color = provinsi,
           group = provinsi)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Tren PDRB per Kapita",
    x = "Tahun",
    y = "PDRB Per Kapita"
  ) +
  theme_minimal()

# Visualisasi data 4: Lolipop chart IPM per Provinsi berdasarkan tahun
ggplot(filtered_mean_data,
       aes(x = reorder(provinsi, ipm),
           y = ipm,
           color = factor(tahun))) +
  geom_segment(aes(xend = provinsi,
                   y = 60,
                   yend = ipm),
               alpha = 0.5) +
  geom_point(size = 4) +
  coord_flip() +
  scale_y_continuous(limits = c(60, 80)) +
  scale_color_manual(values = c(
    "2020" = "#FDE0DD",
    "2021" = "#FCAE91",
    "2022" = "#FB6A4A",
    "2023" = "#DE2D26",
    "2024" = "#A50F15"
  )) +
  labs(
    title = "IPM per Provinsi Berdasarkan Tahun",
    x = "Provinsi",
    y = "IPM",
    color = "Tahun"
  ) +
  theme_minimal()

# Visualisasi data 5: Heatmap Harapan Hidup
ggplot(filtered_mean_data,
       aes(x = factor(tahun),
           y = provinsi,
           fill = harapan_hidup)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#FDE0DD",
    high = "#A50F15"
  ) +
  labs(
    title = "Heatmap Harapan Hidup",
    x = "Tahun",
    y = "Provinsi",
    fill = "Harapan Hidup"
  ) +
  theme_minimal()



#==========================================================
# 1.3.6 Analisis Probabilitas dan Distribusi Data
# Peluang daerah memiliki air bersih lebih dari 70%
#==========================================================

print("--- Uji Normalitas (Shapiro-Wilk) Variabel Air Bersih ---")
uji_normalitas <- shapiro.test(data_bersih$air_bersih)
uji_normalitas

print("--- Menentukan apakah data normal ---")
if(uji_normalitas$p.value > 0.05){
  print("Data mendekati distribusi normal")
} else {
  print("Data tidak berdistribusi normal")
}

print("--- Analisis Probabilitas Sederhana ---")
mu <- mean(data_bersih$air_bersih)
sigma <- sd(data_bersih$air_bersih)

prob_70 <- 1 - pnorm(
  70,
  mean = mu,
  sd = sigma
)

print(paste(
  "Peluang akses air bersih > 70% =",
  round(prob_70, 4)
))


#==========================================================
# 1.3.7 Analisis Korelasi
# Kasus Hubungan IPM dan Pengangguran
#==========================================================

# Menghitung korelasi Pearson
print("--- Menghitung korerlasi antar variabel ---")
kor_ipm_pengangguran <- cor.test(
  data_bersih$ipm,
  data_bersih$pengangguran,
  method = "pearson"
)

# Menampilkan hasil
print(paste("Koefisien Korelasi (r) :", round(kor_ipm_pengangguran$estimate, 4)))
print(paste("P-Value :", round(kor_ipm_pengangguran$p.value, 4)))

# Menentukan arah hubungan
print("--- Arah hubungan positif/negatif ---")
if(kor_ipm_pengangguran$estimate > 0){
  print("Arah hubungan: Positif")
} else {
  print("Arah hubungan: Negatif")
}

# Menentukan kekuatan hubungan
print("--- Menentukan kekuatan hubungan IPM dan Pengangguran ---")
r <- abs(kor_ipm_pengangguran$estimate)

if(r < 0.20){
  print("Kekuatan hubungan: Sangat Lemah")
} else if(r < 0.40){
  print("Kekuatan hubungan: Lemah")
} else if(r < 0.60){
  print("Kekuatan hubungan: Sedang")
} else if(r < 0.80){
  print("Kekuatan hubungan: Kuat")
} else {
  print("Kekuatan hubungan: Sangat Kuat")
}
