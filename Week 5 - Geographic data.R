

library(sf); library(readxl); library(dplyr)

base <- file.path(Sys.getenv("USERPROFILE"),
                  "Dropbox/11 - Teaching/WiSe 2026-2027/QMH (WiSe 26-27) - Quantitative methods for historians")

prov81 <- st_read(list.files(file.path(base, "Province_1981"), "\\.shp$", full.names = TRUE)[1])
gdp    <- read_excel(file.path(base, "Data/GDP.xlsx"))
credit <- read_excel(list.files(file.path(base, "Data"), "credit.*\\.xlsx?$",
                                ignore.case = TRUE, full.names = TRUE)[1]) |>
  mutate(province = provincia)

#Match gdp data, province data, and geographical data

library(ggplot2)

# 1. Join credit onto GDP (add `year` to `by` if the data are a panel)
econ <- gdp %>%
  left_join(credit, by = c("province", "year"))        



# 4. Join the data onto the shapefile (keep the sf object on the left)
prov_data <- prov81 %>%
  left_join(econ, by = c("DEN_PROV" = "province"))      

prov_data <- prov_data %>%
  mutate(share_subsidy = crediti_agevolati / credit_total)


# 5. Map GDP (if the data are a panel, pick one year first)
prov_data %>%
  filter(year == 1982) %>%                    
  ggplot() +
  geom_sf(aes(fill = share_subsidy), colour = "white", linewidth = 0.1) +   
 scale_fill_viridis_c(name = "GDP", na.value = "grey85") +
  labs(title = "Geo plot") +
  theme_void()
