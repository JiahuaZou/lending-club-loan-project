
library(dplyr)
library(tidyr)
library(ggplot2)

df <- read.csv("lending-club-loan-data/loan.csv")
head(df)

bcolnames(df)

summary(is.na(df))

too_many_na <- function(x) sum(is.na(x)) < 100000

df2 <- df %>% select_if(too_many_na)
head(df2)

summary(is.na(df2))

df2 <- df2 %>% drop_na()
summary(is.na(df2))

df2 <- df2 %>% filter(application_type == 'INDIVIDUAL')
nrow(df2)/nrow(df)

colnames(df2)

unique(df2$loan_status)

df2$loan_status <- as.character(df2$loan_status)

head(df2$loan_status, 20)

print(unique(df2$loan_status))

df2$loan_rate <- ifelse(df2$loan_status == "Current" | df2$loan_status == "Issued", 2,
                         ifelse(df2$loan_status == "Fully Paid", 1, 0))

head(df2$loan_rate, 20)

write.csv(df2, "cleaned_loan_data.csv")

sum(df2$loan_rate == 0)

sum(df2$loan_rate == 1)

sum(df2$loan_rate == 2)

nrow(df2) == sum(df2$loan_rate == 0) + sum(df2$loan_rate == 1) + sum(df2$loan_rate == 2)
