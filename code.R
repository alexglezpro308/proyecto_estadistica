install.packages("ggplot2")
install.packages("GGally")
library(ggplot2)
library(GGally)

datos_teleco <- read.csv("Telco_customer_churn.csv")
head(datos_teleco)
colnames(datos_teleco)

table(datos_teleco$Internet.Service == "Fiber optic")

mean(datos_teleco$Internet.Service == "Fiber optic")

max(datos_teleco$Tenure.Months )

mean(datos_teleco$City[datos_teleco$Senior.Citizen == "Yes"] == "Los Angeles")
mean(datos_teleco$Senior.Citizen == "No")


ggpairs(datos_teleco[, c("Tenure.Months", "Internet.Service")], 
        aes(color = Internet.Service, alpha = 0.5)) +
  scale_color_manual(values = c("DSL" = "#00F5FF", "Fiber optic" = "#FF3E96", "No" = "#00FF7F")) +
  scale_fill_manual(values = c("DSL" = "#00F5FF", "Fiber optic" = "#FF3E96", "No" = "#00FF7F"))















