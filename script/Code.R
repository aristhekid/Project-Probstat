#==========================================================
# 1.3.1 Memahami Dataset
#==========================================================

# Membaca dataset
data <- read.csv("pembangunan_wilayah_missing_outlier.csv")

# Menampilkan beberapa data awal
head(data)

# Melihat struktur data
str(data)

# Ringkasan data
summary(data)

# Melihat jumlah baris dan kolom
dim(data)

#==========================================================
# 1.3.2 Statistika Deskriptif
#==========================================================

library(dplyr)

# Memilih variabel numerik

data_num <- data %>%
  select(where(is.numeric))

# Statistik deskriptif umum
summary(data_num)

# Mean
sapply(data_num, mean, na.rm = TRUE)

# Median
sapply(data_num, median, na.rm = TRUE)

# Standar deviasi
sapply(data_num, sd, na.rm = TRUE)

# Varians
sapply(data_num, var, na.rm = TRUE)

# Kuartil
sapply(data_num, quantile, na.rm = TRUE)

#==========================================================
# 1.3.3 Analisis Missing Value
#==========================================================


# Menghitung jumlah missing value setiap variabel
colSums(is.na(data))


# Menampilkan total missing value
sum(is.na(data))

# Persentase missing value
colSums(is.na(data))/nrow(data)*100

# Penanganan missing value dengan Menghapus data
data_bersih <- na.omit(data)

# Memastikan tidak ada missing value lagi
colSums(is.na(data_bersih))

#==========================================================
# 1.3.4 Analisis Outlier 
#==========================================================

# Menentukan Variabel yang Memiliki Outlier
cek_outlier <- function(x){
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  
  lower <- Q1 - 1.5*IQR
  upper <- Q3 + 1.5*IQR
  
  sum(x < lower | x > upper)
}

sapply(data_num, cek_outlier)

# Menjelaskan variabel yang memiliki outlier.

# Membuat boxplot seluruh variabel numerik
boxplot(data_num,
        main = "Deteksi Outlier Variabel Numerik",
        las = 2)

# Membuat boxplot untuk PDRB per kapita untuk menganalisis lebih lanjut
boxplot(data_bersih$pdrb_perkapita,
        main = "Boxplot PDRB Per Kapita")

# Membuat boxplot untuk kemiskinan untuk menganalisis lebih lanjut
boxplot(data_bersih$kemiskinan,
        main = "Boxplot Kemiskinan")

# Membuat boxplot untuk Pengangguran untuk menganalisis lebih lanjut
boxplot(data_bersih$pengangguran,
        main = "Boxplot Pengangguran")



# Menentukan metode penanganan outlier (Winsorizing)

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


# Memastikan Outlier sudah tidak ada
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

library(ggplot2)

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

#Line Chart PDRB per Tahun
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


ggplot(filtered_mean_data,
       aes(x = tahun,
           y = internet,
           fill = provinsi)) +
  geom_area(alpha = 0.7) +
  labs(
    title = "Perkembangan Akses Internet",
    x = "Tahun",
    y = "Akses Internet (%)"
  ) +
  theme_minimal()

ggplot(filtered_mean_data,
       aes(x = reorder(provinsi, ipm),
           y = ipm)) +
  geom_segment(aes(xend = provinsi,
                   y = 0,
                   yend = ipm)) +
  geom_point(size = 4) +
  coord_flip() +
  labs(
    title = "IPM per Provinsi",
    x = "Provinsi",
    y = "IPM"
  ) +
  theme_minimal()

# Kemiskinan
ggplot(filtered_mean_data,
       aes(x = provinsi,
           y = kemiskinan,
           fill = factor(tahun))) +
  geom_col(position = "dodge") +
  labs(
    title = "Persentase kemiskinan per Provinsi setiap tahun",
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

#Pengangguran
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

# IPM
ggplot(filtered_mean_data,
       aes(x = factor(tahun),
           y = provinsi,
           fill = ipm)) +
  geom_tile() +
  labs(
    title = "Heatmap IPM",
    x = "Tahun",
    y = "Provinsi",
    fill = "IPM"
  ) +
  theme_minimal()

# Harapan hidup
ggplot(filtered_mean_data,
       aes(x = provinsi,
           y = harapan_hidup,
           fill = factor(tahun))) +
  geom_col(position = "dodge")
labs(
  title = "Usia Harapan Hidup  per Provinsi setiap tahun",
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

# Lama Sekolah
ggplot(filtered_mean_data,
       aes(x = provinsi,
           y = lama_sekolah,
           fill = factor(tahun))) +
  geom_col(position = "dodge") +
  labs(
    title = "Waktu Lama Sekolah per Provinsi setiap tahun",
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

# Internet
ggplot(filtered_mean_data,
       aes(x = provinsi,
           y = internet,
           fill = factor(tahun))) +
  geom_col(position = "dodge") +
  labs(
    title = "Persentase Internet per Provinsi setiap tahun",
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

# Jalan Baik
ggplot(filtered_mean_data,
       aes(x = provinsi,
           y = jalan_baik,
           fill = factor(tahun))) +
  geom_col(position = "dodge") +
  labs(
    title = "Persentase Jalan Baik per Provinsi setiap tahun",
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

# Air Bersih 
ggplot(filtered_mean_data,
       aes(x = provinsi,
           y = air_bersih,
           fill = factor(tahun))) +
  geom_col(position = "dodge") +
  labs(
    title = "Persentase Air Bersih per Provinsi setiap tahun",
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

#==========================================================
# 1.3.6 Analisis Probabilitas dan Distribusi Data 
#==========================================================

