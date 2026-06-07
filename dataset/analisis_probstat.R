# 1. Membaca dataset yang ada di folder dataset
data_wilayah <- read.csv("dataset/pembangunan_wilayah_missing_outlier.csv")

# 2. Mengintip 6 baris data teratas
head(data_wilayah)

# 3. Mengecek dimensi dan struktur data awal
dim(data_wilayah)
str(data_wilayah)

# ==========================================================
# 1.3.2 STATISTIKA DESKRIPTIF
# ==========================================================
# Melihat rangkuman statistik dasar (Mean, Median, Min, Max, Kuartil) semua variabel
summary(data_wilayah)

# Menghitung Standar Deviasi untuk salah satu variabel numerik (misal: IPM)
# Pakai na.rm = TRUE karena datanya masih ada yang bolong/missing value
sd(data_wilayah$ipm, na.rm = TRUE)
var(data_wilayah$ipm, na.rm = TRUE)


# ==========================================================
# 1.3.3 ANALISIS MISSING VALUE (DATA BOLONG)
# ==========================================================
# Menghitung jumlah total data yang bolong (NA) di setiap kolom
colSums(is.na(data_wilayah))