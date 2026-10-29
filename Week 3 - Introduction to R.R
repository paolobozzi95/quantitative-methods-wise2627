

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
#extract user
user <- Sys.getenv("USERNAME")
data_dir <- file.path("C:/Users", user, "Dropbox", "11 - Teaching", "WiSe 2026-2027", 
                      "QMH - Quantitative methods for historians (WiSe 26-27)", "Data")

setwd(data_dir)

Pietschmann_1988 <- read_excel("Pietschmann_1988.xlsx")
Hager_1986 <-       read_excel("Hager_1986.xlsx")
Bozzi_1984 <-       read_excel("Bozzi_1984.xlsx")
Riedl_1982 <-       read_excel("Riedl_1982.xlsx")


#I clean up the dataset and replace Raoul's crediti non agevolati with crediti agevolati

library(dplyr)

# crediti agevolati = total credit - crediti non agevolati
Pietschmann_1988 <- Pietschmann_1988 %>%
  mutate(`TOTALE CREDITI AGEVOLATI` = `TOTALE GENERALE` - `Crediti non agevolati`) %>%
  select(-`Crediti non agevolati`) |>
  filter(!is.na(PROVINCIA))

#remove rows with NAs


#Merge data, add a year column based on the name of the dataset
library(tidyverse)

datasets <- mget(c("Riedl_1982", "Bozzi_1984", "Hager_1986", "Pietschmann_1988"))

merged <- datasets |>
  map(\(df) mutate(df, across(everything(), as.character))) |>
  list_rbind(names_to = "dataset") |>
  mutate(year   = as.integer(str_extract(dataset, "\\d{4}$"))) |>
  mutate(credit_total = as.numeric(`TOTALE GENERALE`),
         crediti_agevolati = as.numeric(`TOTALE CREDITI AGEVOLATI`),
         provincia = as.character(PROVINCIA)) |>
  select(-dataset, -PROVINCIA, -`TOTALE GENERALE`, -`TOTALE CREDITI AGEVOLATI`) |>
  relocate(year)


#save as excel
library(openxlsx)
write.xlsx(merged, "dataset.xlsx")

