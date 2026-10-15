# ====================================================================
# ANÁLISIS DE ABANDONO DE CLIENTES (TELCO CHURN) - SCRIPT R
# Autores: Alejandro Gonzalez & Eduardo Vega López
# ====================================================================

# Instalación de paquetes (descomentar si es la primera vez)
# install.packages("ggplot2")
# install.packages("GGally")
# install.packages("dplyr")

# 0. Carga de librerías y datos
library(ggplot2)
library(GGally)
library(dplyr)

datos_telco <- read.csv("Telco_customer_churn.csv")

# ====================================================================
# EXPLORACIÓN INICIAL (Tu código)
# ====================================================================
head(datos_telco)
colnames(datos_telco)

# Clientes con Fibra Óptica
table(datos_telco$Internet.Service == "Fiber optic")
mean(datos_telco$Internet.Service == "Fiber optic")

# Antigüedad máxima
max(datos_telco$Tenure.Months, na.rm = TRUE)

# Proporción de Tercera Edad en Los Angeles y general
mean(datos_telco$City[datos_telco$Senior.Citizen == "Yes"] == "Los Angeles", na.rm = TRUE)
mean(datos_telco$Senior.Citizen == "No", na.rm = TRUE)

# Gráfico de relaciones (Antigüedad vs Servicio de Internet)
ggpairs(datos_telco[, c("Tenure.Months", "Internet.Service")], 
        aes(color = Internet.Service, alpha = 0.5)) +
  scale_color_manual(values = c("DSL" = "#00F5FF", "Fiber optic" = "#FF3E96", "No" = "#00FF7F")) +
  scale_fill_manual(values = c("DSL" = "#00F5FF", "Fiber optic" = "#FF3E96", "No" = "#00FF7F"))

# ====================================================================
# 1. Proporción de clientes que han abandonado la compañía
# ====================================================================
table(datos_telco$Churn.Label)
round(prop.table(table(datos_telco$Churn.Label)) * 100, 2)

ggplot(datos_telco, aes(x = Churn.Label, fill = Churn.Label)) +
  geom_bar() +
  labs(title = "Abandono de clientes", x = "Situación", y = "Número de clientes") +
  theme_minimal() +
  theme(legend.position = "none")

# ====================================================================
# 2. Abandono según el tipo de contrato
# ====================================================================
table(datos_telco$Contract, datos_telco$Churn.Label)
round(prop.table(table(datos_telco$Contract, datos_telco$Churn.Label), margin = 1) * 100, 2)

ggplot(datos_telco, aes(x = Contract, fill = Churn.Label)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Abandono según contrato", x = "Contrato", y = "Porcentaje", fill = "Abandono") +
  theme_minimal()

# ====================================================================
# 3. Diferencias en la antigüedad (Tenure)
# ====================================================================
datos_telco %>%
  group_by(Churn.Label) %>%
  summarise(
    Media_Antiguedad = mean(Tenure.Months, na.rm = TRUE),
    Mediana_Antiguedad = median(Tenure.Months, na.rm = TRUE)
  )

ggplot(datos_telco, aes(x = Churn.Label, y = Tenure.Months, fill = Churn.Label)) +
  geom_boxplot() +
  labs(title = "Antigüedad vs Abandono", x = "Abandono", y = "Antigüedad (Meses)") +
  theme_minimal() +
  theme(legend.position = "none")

# ====================================================================
# 4. Diferencias en los cargos mensuales
# ====================================================================
datos_telco %>%
  group_by(Churn.Label) %>%
  summarise(
    Media_Cargos = mean(Monthly.Charges, na.rm = TRUE),
    Mediana_Cargos = median(Monthly.Charges, na.rm = TRUE)
  )

ggplot(datos_telco, aes(x = Monthly.Charges, fill = Churn.Label)) +
  geom_density(alpha = 0.6) +
  labs(title = "Distribución de Cargos Mensuales", x = "Cargo Mensual ($)", y = "Densidad", fill = "Abandono") +
  theme_minimal()

# ====================================================================
# 5. Abandono según el servicio de Internet
# ====================================================================
table(datos_telco$Internet.Service, datos_telco$Churn.Label)
round(prop.table(table(datos_telco$Internet.Service, datos_telco$Churn.Label), margin = 1) * 100, 2)

ggplot(datos_telco, aes(x = Internet.Service, fill = Churn.Label)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Abandono por Tipo de Internet", x = "Servicio", y = "Porcentaje", fill = "Abandono") +
  theme_minimal()

# ====================================================================
# 6. Motivos de abandono más frecuentes
# ====================================================================
motivos <- datos_telco %>%
  filter(Churn.Label == "Yes" & Churn.Reason != "") %>%
  count(Churn.Reason) %>%
  arrange(desc(n)) %>%
  top_n(10, n)

ggplot(motivos, aes(x = reorder(Churn.Reason, n), y = n)) +
  geom_col(fill = "#e74c3c") +
  coord_flip() +
  labs(title = "Principales motivos de abandono", x = "Motivo", y = "Frecuencia") +
  theme_minimal()

