# install.packages("languageserver")
# install.packages("readxl")

library(readxl)

# Daten importierten
meineDaten <- read_excel("C:\\privat\\Repos\\R\\WIR110T1104_Hotelgaeste_StadtZuerich.xlsx", sheet = 4, range = cell_rows(9:380))

View(meineDaten)

# Qantile berechnen
qIn <- quantile(meineDaten$Inlandgäste)
qAus <- quantile(meineDaten$Auslandgäste)
qTot <- quantile(meineDaten$Total)

# Median berechnen
mIn <- median(meineDaten$Inlandgäste)
mAus <- median(meineDaten$Auslandgäste)
mTot <- median(meineDaten$Total)


# Mittelwerte berechnen
mIn <- mean(meineDaten$Inlandgäste)
mAus <- mean(meineDaten$Auslandgäste)
mTot <- mean(meineDaten$Total)

# Varianz
vIn <- var(meineDaten$Inlandgäste)
vAus <- var(meineDaten$Auslandgäste)
vTot <- var(meineDaten$Total)

# Standardabweichung
saIn <- sqrt(var(meineDaten$Inlandgäste))
saAus <- sqrt(var(meineDaten$Auslandgäste))
satot <- sqrt(var(meineDaten$Total))

# Modus
modus <- function(x) {
  unique_x <- unique(x)
  counts <- tabulate(match(x, unique_x))
  max_counts <- max(counts)
  modi <- unique_x[counts == max_counts]
  if (length(modi) == 1) {
    return(modi)
  } else {
    return(modi)
  }
}

modus(meineDaten)

# Boxplotting (Grafik)
boxplot(meineDaten$Inlandgäste,
        main="Inlandgäste Boxplot",
        ylab="Inlandgäste",
        xlab="Monate",
        col="orange")


#Variationskoeffizienten
cv <- (sd(meineDaten$Inlandgäste) / mean(meineDaten$Inlandgäste)) * 100
summary(cv)

# Ankünfte nur Jahr 2022
j2022 <- subset(meineDaten, Jahr == 2022)
View(j2022)
summary(j2022)

# Ausgabe
print(qIn)
