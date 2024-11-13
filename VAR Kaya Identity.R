# analisis var pada model kaya identity 

# import library
library(vars)
library(urca)
library(dplyr)
library(tidyverse)
library(readxl)
library(aTSA)

# import dataset
df <- read_excel("Data VAR.xlsx")

# menggunakan kolom yang dibutuhkan saja
str(df)
df <- df[, c('Tahun', 'Fossil CO2 Emissions', 'Population', 'Total Energy Consumption (ktoe)', 'PDB')]

# merename variabel supaya lebih ringkas
df <- df %>% rename(emisi = 'Fossil CO2 Emissions', pop = Population, energi = 'Total Energy Consumption (ktoe)', pdb = 'PDB')

# menyamakan satuan data
df$energi <- df$energi*1000

# visualisasi data awal
ggplot(df) + geom_jitter(mapping = aes(pop, emisi))
ggplot(df) + geom_jitter(mapping = aes(pop, energi))
ggplot(df) + geom_jitter(mapping = aes(pop, pdb))

# membuat persamaan kaya identity
df$pdbpcp <- df$pdb/df$pop
df$energiint <- df$energi/df$pdb
df$carbonint <- df$emisi/df$energi
df$kaya <- df$pop*df$pdbpcp*df$energiint*df$carbonint

# membuat data menjadi time series
datats <- ts(df, start = 1993)
plot(datats)

# memulai analisis var
str(datats)
rm(datavar)
datavar <- datats[, c('pop', 'pdbpcp', 'energiint', 'carbonint', 'kaya')]
str(datavar)
plot(datavar, nc=2)

# melakukan log pada data
log.datavar <- log(datavar)

# tes stationeritas menggunakan adf
adf.test(log.datavar[, 'pop']) # tidak stationer di tingkat level
adf.test(diff(log.datavar[, 'pop'])) # belum stationer di diff 1
adf.test(diff(diff(log.datavar[, 'pop']))) # belum stationer di diff 2
adf.test(diff(diff(diff(log.datavar[, 'pop'])))) # sudah stationer di diff 3
diffpop <- diff(diff(diff(log.datavar[, 'pop'])))

adf.test(datavar[, 'pdbpcp']) # belum stationer di tingkat level
adf.test(diff(datavar[, 'pdbpcp'])) # belum stationer di diff 1
adf.test(diff(diff(datavar[, 'pdbpcp']))) # sudah stationer di diff 2
diffpdbpcp <- diff(diff(datavar[, 'pdbpcp']))

adf.test(datavar[, 'energiint'])
adf.test(diff(datavar[, 'energiint']))
adf.test(diff(diff(datavar[, 'energiint']))) # sudah stationer di diff 2
diffenergi <- diff(diff(datavar[, 'energiint']))

adf.test(datavar[, 'carbonint'])
adf.test(diff(datavar[, 'carbonint']))
adf.test(diff(diff(datavar[, 'carbonint']))) # sudah stationer di diff 2
diffcarbon <- diff(diff(datavar[, 'carbonint']))

adf.test(datavar[, 'kaya'])
adf.test(diff(datavar[, 'kaya']))
adf.test(diff(diff(datavar[, 'kaya']))) # sudah stationer di diff 2
diffkaya <- diff(diff(datavar[, 'kaya']))

# membuat dataset baru yang berisi variabel yang sudah di diff
data.diff <- na.omit(ts(cbind(diffkaya, diffpop, diffpdbpcp, diffenergi, diffcarbon)))
plot(data.diff)

# mencari lag optimal
VARselect(data.diff, lag.max = 10) # lag optimal = 4

# mengestimasi var
var.model <- VAR(data.diff, p = 1, type = "none") # lag 4 tidak stabil, maka dipilih lag 1
print(var.model)
str(data.diff)
plot(data.diff)

# test kestabilan
roots(var.model) # hasil stabil pada lag 1

# analisis IRF
irf.result <- irf(var.model, impulse = "diffenergi", response = "diffkaya", n.ahead = 10)
plot(irf.result)

# forecast hasil
forecast.result <- predict(var.model, n.ahead = 10)
print(forecast.result)
plot(forecast.result)









