
library(readxl)  
library(dplyr) 
library(factoextra)  
library(cluster) 
library(caret)

data <- read_excel("data.xlsx")

data$`Provincia de Procedencia` <- as.factor(data$`Provincia de Procedencia`)

data <- data%>%
  mutate(across(starts_with("3.1"), ~ recode(.,"Ninguna" = 1,"Baja" = 2,"Media" = 3,"Alta" = 4,"Muy alta" = 5,.default = NA_real_),.names = "{col}")) %>%
  mutate(across(starts_with("3.2"), ~ recode(.,"Muy mala" = 1, "Mala" = 2,"Regular" = 3, "Buena" = 4,"Muy buena" = 5,.default = NA_real_),.names = "{col}")) %>%
  mutate(across(starts_with("3.3"), ~ recode(.,"Muy mala" = 1, "Mala" = 2,"Regular" = 3, "Buena" = 4,"Muy buena" = 5,.default = NA_real_),.names = "{col}")) %>%
  mutate(across(starts_with("3.4"), ~ recode(.,"Nada importante" = 1, "Poco Importante" = 2,"Neutral" = 3, "Importante" = 4,"Muy Importante" = 5,.default = NA_real_),.names = "{col}")) %>%
  mutate(across(starts_with("3.5"), ~ recode(.,"Nada importante" = 1, "Poco Importante" = 2,"Neutral" = 3, "Importante" = 4,"Muy Importante" = 5,.default = NA_real_),.names = "{col}")) %>%
  mutate(across(starts_with("3.6"), ~ recode(.,"Nada importante" = 1, "Poco Importante" = 2,"Neutral" = 3, "Importante" = 4,"Muy Importante" = 5,.default = NA_real_),.names = "{col}")) %>%
  mutate(across(starts_with("3.7"), ~ recode(.,"Nada importante" = 1, "Poco Importante" = 2,"Neutral" = 3, "Importante" = 4,"Muy Importante" = 5,.default = NA_real_),.names = "{col}")) %>%
  mutate(across(starts_with("3.8"), ~ recode(.,"si" = 1, "no" = 2,.default = NA_real_),.names = "{col}"))

data_one_hot <- dummyVars("~ .", data = data)
data_encoded <- predict(data_one_hot, newdata = data)

data_encoded <- as.data.frame(data_encoded)

datos_numericos <- dplyr::select(data, starts_with("3."))  

datos_numericos_norm <- scale(datos_numericos)

set.seed(123)
fviz_nbclust(datos_numericos_norm, kmeans, method = "wss") 

set.seed(123) 
kmeans_model <- kmeans(datos_numericos_norm, centers = 3, nstart = 20)

clusters <- kmeans_model$cluster

data_con_clusters <- cbind(data, Cluster = clusters)

fviz_cluster(kmeans_model, data = datos_numericos_norm,
             geom = "point", stand = FALSE, 
             main = "Visualización de Clusters")

kmeans_model$centers  

print(table(data_con_clusters$Cluster))

resumen_cluster <- data_con_clusters %>%
  group_by(Cluster) %>%
  summarise(
    edad_promedio = mean(edad, na.rm = TRUE),
    genero_masculino = sum(Genero == "Masculino", na.rm = TRUE),
    genero_femenino = sum(Genero == "Femenino", na.rm = TRUE),
    
  )
print(resumen_cluster)

ggplot(data_con_clusters, aes(x = `3.1`, y = `3.5`, color = as.factor(Cluster))) +
  geom_point() +
  labs(title = "Expectativas Salariales vs. Oportunidades de Empleo por Cluster",
       x = "Expectativas Salariales",
       y = "Oportunidades de Empleo",
       color = "Cluster")

