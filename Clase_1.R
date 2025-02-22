library(visdat)
library(skimr)
library(insuranceData)
library(ggplot2)
library(dplyr)
library(patchwork)

data("dataCar")
str(dataCar)
summary(dataCar)

#Resumen mas detallado

skim(dataCar)
head(dataCar,10) #Pr default aparecen 6, con ",10" Aparecen las 10 
dim(dataCar)#Filas x columnas

#Caracteriasticas del dataframe

glimpse(dataCar)

#Conocer el nomre de las columnas

colnames(dataCar)
names(dataCar)

#Verificar si hay datos faltantes

miss <- any(is.na(dataCar))
miss
vis_dat(dataCar)
vis_miss(dataCar)

#Reporte con DataExplore
#DataExplorer::create_report(dataCare)

#Para conocer cuantos valores existen y las reclamaciones>=1
pol <- length(dataCar$numclaims)
pol

claims <- sum(dataCar$numclaims>=1)
(claims/pol)*100 #Porcentajes de reclamos 

#Verificar top 5 vehiculos con mayor numero de reclamaciones

claims_tipo <- dataCar %>%
  group_by(veh_body) %>%
  summarise(totclaims=sum(numclaims)) %>%
  arrange(desc(totclaims))
head(claims_tipo,5)

#Numero de polizas por tipo de vehiculo

veh <- dataCar %>%
  group_by(veh_body) %>%
  summarise(tipo_veh=n()) %>%
  arrange(desc(tipo_veh))
veh

#Verifique el top 10 de vehiculo con mayor monto de reclamacion 

claims_tipo_2 <- dataCar %>%
  group_by(veh_body) %>%
  summarise(
    monto_clame = sum(claimcst0), 
    avg_exposure = mean(exposure)  
  ) %>%
  arrange(desc(monto_clame))  

#Realice un analisis completo del genero 


# Total de pólizas por genero
total_polizas <- nrow(dataCar)

# Pólizas por género
polizas_genero <- dataCar %>%
  group_by(gender) %>%
  summarise(polizas = n()) %>%
  mutate(proporcion = polizas / total_polizas * 100)

polizas_genero


# Total de reclamaciones por género
reclamaciones_genero <- dataCar %>%
  group_by(gender) %>%
  summarise(
    total_reclamaciones = sum(numclaims),
    promedio_reclamaciones = mean(numclaims)
  )

reclamaciones_genero

# Monto total reclamado por género
monto_reclamado_genero <- dataCar %>%
  group_by(gender) %>%
  summarise(
    monto_total = sum(claimcst0),
    promedio_monto = mean(claimcst0)
  )

monto_reclamado_genero

g1 <- ggplot(polizas_genero, aes(x = gender, y = polizas, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribución de Pólizas por Género", x = "Género", y = "Número de Pólizas") +
  theme_minimal()

g2 <- ggplot(reclamaciones_genero, aes(x = gender, y = total_reclamaciones, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Total de Reclamaciones por Género", x = "Género", y = "Reclamaciones Totales") +
  theme_minimal()

g3 <- ggplot(monto_reclamado_genero, aes(x = gender, y = monto_total, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Monto Total Reclamado por Género", x = "Género", y = "Monto Total Reclamado") +
  theme_minimal()

# Reclamaciones totales por género y tipo de vehículo
reclamaciones_genero_tipo <- dataCar %>%
  group_by(gender, veh_body) %>%
  summarise(
    total_reclamaciones = sum(numclaims),
    monto_total = sum(claimcst0)
  ) %>%
  arrange(desc(total_reclamaciones))

head(reclamaciones_genero_tipo, 10)

g4 <- ggplot(reclamaciones_genero_tipo, aes(x = reorder(veh_body,-total_reclamaciones), y = total_reclamaciones, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Reclamaciones por Género y Tipo de Vehículo", x = "Tipo de Vehículo", y = "Reclamaciones Totales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



graficos_combinados <- g1 + g2 + g3

print(graficos_combinados)

#Numero de sinistros por edad y genero

NO_siniestros_edad_gen <- dataCar %>%
  group_by(gender, veh_body,agecat) %>%
  summarise(
    total_reclamaciones_edad_gen = sum(numclaims),
    monto_total = sum(claimcst0)
  ) %>%
  arrange(desc(total_reclamaciones_edad_gen))

NO_siniestros_edad_gen

#Edad y monto de reclamacion

g5 <- ggplot(NO_siniestros_edad_gen, aes(x = agecat)) +
  geom_bar(aes(y = total_reclamaciones_edad_gen), stat = "identity", position = "dodge", fill = "plum") +
  labs(title = "Total de Reclamaciones por edad ", x = "Edad", y = "Reclamaciones Totales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g5

g6 <- ggplot(NO_siniestros_edad_gen, aes(x = agecat, y = total_reclamaciones_edad_gen, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~veh_body) +
  labs(
    title = "Reclamaciones por Género, Tipo de Vehículo y Categoría de Edad",
    x = "Categoría de Edad",
    y = "Reclamaciones Totales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g6

graf_edad_monto <- g5 + g6
print(graf_edad_monto)

#Grafico con vehiculo, valor del vehiculo, monto de reclam y exposicion 


# Resumen de datos por tipo de vehículo
data_summary <- dataCar %>%
  group_by(veh_body) %>%
  summarise(
    avg_veh_value = mean(veh_value),  
    total_claimcst0 = sum(claimcst0), 
    total_exposure = sum(exposure)    
  )

# Gráfico combinado: Puntos y líneas, ordenado por monto total reclamado
g7 <- ggplot(data_summary, aes(x = reorder(veh_body, -total_claimcst0))) +  
  geom_point(aes(y = avg_veh_value, size = total_exposure, color = "Valor Promedio del Vehículo"), alpha = 0.7) +
  geom_line(aes(y = avg_veh_value, group = 1, color = "Valor Promedio del Vehículo"), linetype = "dashed") +
  geom_point(aes(y = total_claimcst0, size = total_exposure, color = "Monto Total Reclamado"), alpha = 0.7) +
  geom_line(aes(y = total_claimcst0, group = 1, color = "Monto Total Reclamado"), linetype = "solid") +
  scale_size_continuous(name = "Exposición Total", range = c(3, 10)) + 
  scale_color_manual(name = "Variable", values = c("Valor Promedio del Vehículo" = "blue", "Monto Total Reclamado" = "red")) +
  labs(
    title = "Análisis por Tipo de Vehículo",
    x = "Tipo de Vehículo",
    y = "Valor Promedio / Monto Reclamado"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
g7

# Total de reclamaciones en todo el dataset
total_reclamaciones <- sum(dataCar$numclaims)

# Suma de reclamaciones para los 5 tipos principales
top_5_reclamaciones <- sum(head(claims_tipo$totclaims, 5))

# Porcentaje de reclamaciones de los 5 tipos principales
porcentaje_top_5 <- (top_5_reclamaciones / total_reclamaciones) * 100


# Suma de los montos reclamados por los 5 tipos principales
top_5_montos <- sum(head(claims_tipo_2$monto_clame, 5))

# Convertir a millones 
montos_en_millones <- top_5_montos / 1e6


