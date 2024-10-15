library(tidyverse)
data(starwars)
starwars %>% select(-name)
starwars %>% select(contains("_"))
starwars %>% select(starts_with("s"))
homeworld <- starwars %>% select(name, homeworld)
human <- starwars %>% filter(species == "Human")
starwars %>% filter(species == "Human", homeworld == "Tatooine")
starwars_nodroids <- starwars %>% filter(species != "Droid")

#PREGUNTA 1:¿Cuántos registros cumplen las condiciones finales?
#RESPUESTA: Humanos 35 observaciones y No droides 77 observaciones.

starwars %>% group_by(species) %>% tally()
starwars %>% group_by(species, gender) %>% tally()
table_gender <- starwars %>% group_by(species, gender) %>% tally()

starwars %>% group_by(species) %>% summarise(mean_height = mean(height, na.rm = T),mean_mass = mean(mass,na.rm = T))

#PREGUNTA 2: ¿Cómo calcularías la desviación estándar (sd) de esos parámetros? Recuerda consultar con ? si no sabes como usar una función o comando. Por ejemplo: ?summarise() ,?sd().
#RESPUESTA: starwars %>% group_by(species) %>% summarise(sd_height = sd(height, na.rm = T),sd_mass = sd(mass,na.rm = T))
starwars %>% group_by(species) %>% summarise(sd_height = sd(height, na.rm = T),sd_mass = sd(mass,na.rm = T))

ggplot(starwars, aes(height, mass)) + geom_point()
ggplot(starwars, aes(height, mass)) + geom_point(colour = "red")
ggplot(starwars, aes(height, mass)) + geom_point(colour = "purple", pch = 3)
ggplot(starwars, aes(height, mass)) + geom_point(colour = "red") + theme_light()

starwars_noJabba <- starwars %>% filter(name != "Jabba Desilijic Tiure")
ggplot(starwars_noJabba, aes(height, mass)) + geom_point(colour = "red") + theme_light()

toy <- read_csv("C:/Users/laura/downloads/toy.csv")
#Inspecciona el dataset, haz un resumen de la media (mean) de las variables (Peso, Altura,IMC, IAS, CCintura). Agrupando por sexo.
toy %>% 
  group_by(Sex) %>% 
  summarise(
    mean_Weight_Kg = mean(Weight_Kg, na.rm = TRUE),
    mean_Height_cm = mean(Height_cm, na.rm = TRUE),
    mean_IMC = mean(IMC, na.rm = TRUE),
    mean_Ccintura = mean(Ccintura, na.rm = TRUE)
  )
#Haz una tabla sólo con los pacientes femeninos ¿Cuántos registros cumplen las condiciones? ¿De estos cuantos tienen Sobrepeso (Overweight)?  Usa select y filter.
pacientes_femeninos <- toy %>% filter(Sex == "Women") %>% select(Sex, IMC_clas) 
# 58 pacientes son mujeres.
#Cuántas pacientes tienen sobrepeso
pacientes_sobrepeso <- pacientes_femeninos %>% filter(IMC_clas == "Overweight") 
# 9 mujeres tienen sobrepeso.
#Haz un gráfico usando ggplot relacionando el IMC (Indice de masa corporal) con el peso (Weight_Kg) de todos los pacientes.
ggplot(toy, aes(Weight_Kg, IMC)) + geom_point(color = "purple")

#Utiliza los comandos adecuados para instalar los paquetes de R ape phangorn y phytools que utilizaremos en el laboratorio de la siguiente semana. Carga las librerías y envia un print de pantalla con el output, demostrando que la instalación fue exitosa o si hubo algún problema.
install.packages("ape")
install.packages("phangorn")
install.packages("phytools")
