

print("Hello, World!")

x <- c(1, 2, 4, 6, 9)

y <- c("Raoul", "Louisa", 
       "Armin")

z <- as.character(x)


concentrated <- c(6,7,8,5, 9,10, 4)
mean(concentrated)

dispersed <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 61)
mean(dispersed)

sd(concentrated)
sd(dispersed)    #I calculate the standard deviation


#Data analysis public credit in Italy

#install libraries
install.packages("readxl")
install.packages("readr")

#open the libraries
library(readxl)

#open the data
library(readxl)

Pietschmann_1988 <- read_excel("C:/Users/bop33746/Dropbox/11 - Teaching/WiSe 2026-2027/QMH - Quantitative methods for historians (WiSe 26-27)/Data/Pietschmann_1988.xlsx")

#read csv file
library(readr)
Hager_1986 <- read_delim("C:/Users/bop33746/Dropbox/11 - Teaching/WiSe 2026-2027/QMH - Quantitative methods for historians (WiSe 26-27)/Data/Hager_1986.CSV",
                         show_col_types = FALSE)


Bozzi_1984 <- read_excel("C:/Users/bop33746/Dropbox/11 - Teaching/WiSe 2026-2027/QMH - Quantitative methods for historians (WiSe 26-27)/Data/Bozzi_1984.xlsx")

Riedl_1982 <- read_excel("C:/Users/bop33746/Dropbox/11 - Teaching/WiSe 2026-2027/QMH - Quantitative methods for historians (WiSe 26-27)/Data/Riedl_1982.xlsx")

#Merge data, add a year column based on the name of the dataset

library(tidyverse)

datasets <- mget(c("Riedl_1982", "Bozzi_1984", "Hager_1986", "Pietschmann_1988"))

merged <- datasets |>
  map(\(df) mutate(df, across(everything(), as.character))) |>
  list_rbind(names_to = "dataset") |>
  mutate(year   = as.integer(str_extract(dataset, "\\d{4}$")),
         author = str_remove(dataset, "_\\d{4}$")) |>
  select(-dataset) |>
  relocate(author, year) |>
  type_convert()   # re-guesses numeric columns after binding





