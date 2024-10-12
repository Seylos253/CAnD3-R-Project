#setting the working directory
setwd("G:/RStudio Files")

install.packages('readr')
library (readr)

read_csv("pumf-98M0001-E-2016-individuals_F1.csv")

censusdata <- read.csv("G:/RStudio Files/pumf-98M0001-E-2016-individuals_F1.csv")

install.packages("dplyr")
library(dplyr)

install.packages('tibble')
library(tibble)

#set library dependencies
library(broom)

census_data <- censusdata |> select(PPSORT, HDGREE, AGEIMM, POB, PR, Sex)

#step one in Nicoles program is to recode male and female into binary. She suggests using the function "factor", but I prefer the mutate function of DPLYR - we will see if this causes any differences
census_data <- census_data %>%
  mutate(SexBinary = ifelse(Sex == 1, 0, 1))

#step two is to recode place of birth as follows.
census_data <- census_data %>%
  mutate(OriginRegion = factor(case_when(
    POB == 1 ~ 1,
    POB == 2 ~ 2,
    POB %in% 3:6 ~ 3,
    POB %in% 7:15 ~ 4,
    POB %in% 16:18 ~ 5,
    POB %in% 19:31 ~ 6,
    POB == 32 ~ 7,
    POB == 88 ~ NA,
    TRUE ~ NA_real_
  ),
  labels = c("Born in Canada", "Born in USA", "Born in Latin America", "Born in Europe", "Born in Africa", "born in Asia", "Born in Oceania and Elsewhere")
  ))

#step three is recoding HDGREE as Nicole outlined
census_data <- census_data %>%
  mutate(Educlevel = case_when(
    HDGREE == 1 ~ 1,
    HDGREE == 2 ~ 2, 
    HDGREE %in% 3:4 ~ 3,
    HDGREE %in% 5:8 ~ 4,
    HDGREE == 9 ~ 5,
    HDGREE %in% 10:13 ~ 6,
    HDGREE %in% c(88,99) ~ 99,
    TRUE ~ NA_real_
  ))

#adding labels to existing variable PR 
census_data <- census_data %>%
  mutate(PR = factor(PR,
                     levels = c(10, 11, 12, 13, 24, 35, 46, 47, 48, 59, 70),
                     labels = c("Newfoundland and Labrador",
                                "Prince Edward Island",
                                "Nova Scotia", 
                                "New Brunswick",
                                "Quebec",
                                "Ontario",
                                "Manitoba",
                                "Saskatchewan",
                                "Alberta",
                                "British Columbia",
                                "Northern Canada")))


#adding labels to existing variable AGEIMM
census_data <- census_data %>%
  mutate(AGEIMM = factor(AGEIMM,
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 88, 99),
                         labels = c("0 to 4 years", 
                                    "5 to 9 years",
                                    "10 to 14 years",
                                    "15 to 19 years",
                                    "20 to 24 years",
                                    "25 to 29 years",
                                    "30 to 34 years",
                                    "35 to 39 years",
                                    "40 to 44 years",
                                    "45 to 49 years",
                                    "50 to 54 years",
                                    "55 to 59 years",
                                    "60 years and older",
                                    "Not available or not applicable",
                                    "Not available or not applicable")))

#frequency table using DPLYR
crosstable1 <- census_data %>%
  count(OriginRegion, PR)
print(crosstable1)

write.csv(crosstable1, "sam_output_crosstab.csv", row.names = FALSE)

#run the linear regression
model <-  lm(Educlevel ~ OriginRegion + SexBinary + PR + AGEIMM, data = census_data)

summary (model)

regression <-  tidy(model)

write.csv(regression, "sam_output_regression.csv", row.names = T)
