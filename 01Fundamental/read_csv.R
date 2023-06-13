setwd("C:/workspace/Private/data-science/R/01Fundamental/Rproject")

data_df <- read.csv(file = "DAT/example.csv",
                    header = TRUE,
                    sep = ",",
                    encoding = "UTF-8")
str(data_df)

head(data_df)

tail(data_df)

