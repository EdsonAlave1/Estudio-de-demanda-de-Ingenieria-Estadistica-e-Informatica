library(openxlsx)
library(dplyr)
library(forcats)
library(psych)
cuestionario <- read.xlsx("P_piloto.xlsx")

cuestionario <- cuestionario %>%
  mutate(across(starts_with("3.1"), ~ recode(.,"Ninguna" = 1,"Baja" = 2,"Media" = 3,"Alta" = 4,"Muy Alta" = 5,.default = NA_real_),.names = "{col}")) %>%
  mutate(across(starts_with("3.2"), ~ recode(.,"Muy mala" = 1, "Mala" = 2,"Regular" = 3, "Buena" = 4,"Muy buena" = 5,.default = NA_real_),.names = "{col}")) %>%
  mutate(across(starts_with("3.3"), ~ recode(.,"Muy mala" = 1, "Mala" = 2,"Regular" = 3, "Buena" = 4,"Muy buena" = 5,.default = NA_real_),.names = "{col}")) %>%
  mutate(across(starts_with("3.4"), ~ recode(.,"Nada importante" = 1, "Poco Importante" = 2,"Neutral" = 3, "Importante" = 4,"Muy Importante" = 5,.default = NA_real_),.names = "{col}")) %>%
  mutate(across(starts_with("3.5"), ~ recode(.,"Nada importante" = 1, "Poco Importante" = 2,"Neutral" = 3, "Importante" = 4,"Muy Importante" = 5,.default = NA_real_),.names = "{col}")) %>%
  mutate(across(starts_with("3.6"), ~ recode(.,"Nada importante" = 1, "Poco Importante" = 2,"Neutral" = 3, "Importante" = 4,"Muy Importante" = 5,.default = NA_real_),.names = "{col}")) %>%
  mutate(across(starts_with("3.7"), ~ recode(.,"Nada importante" = 1, "Poco Importante" = 2,"Neutral" = 3, "Importante" = 4,"Muy Importante" = 5,.default = NA_real_),.names = "{col}")) %>%
mutate(across(starts_with("3.8"), ~ recode(.,"si" = 1, "no" = 2,.default = NA_real_),.names = "{col}"))  

preguntas <- dplyr::select(cuestionario, starts_with("3."))

alpha_result <- alpha(preguntas, check.keys = TRUE)

print(alpha_result)



