#importo le librerie 
require(tidyverse)
require(magrittr)
library(lubridate)
library(dplyr)
library(tidyr)
require(ggplot2)
require(gridExtra)
library(FactoMineR)
library(FactoInvestigate)
library(Factoshiny)
library(factoextra)
library(plotly)
#import dataset from text readr
library(readr)
Country_data <- read_csv("C:/Users/microsoft/Desktop/progetto4/Country-data.csv")

# Calcolo della matrice delle correlazioni
cor_matrix <- cor(Country_data[c("child_mort", "exports", "health", "imports", "income", "inflation", "life_expec", "total_fer", "gdpp")])
# La funzione cor() in R calcola la correlazione lineare tra le coppie di variabili. Il risultato è una matrice di correlazione, dove ogni cella indica la relazione di correlazione tra la coppia di variabili corrispondente.
# Una correlazione di 1 indica una correlazione positiva perfetta, 
#mentre -1 indica una correlazione negativa perfetta.
#Un valore vicino a 0 indica una debole o nessuna correlazione lineare tra le variabili.





# Grafico di dispersione tra due variabili: reddito e pil
plot_ly(Country_data, x = ~income, y = ~gdpp, text = ~country, type = "scatter", mode = "markers") %>%
  layout(title = "Scatter plot: Reddito vs Pil per capita",
         xaxis = list(title = "Reddito"),
         yaxis = list(title = "PIL per capita"),
         hovermode = "closest")

# i punti tendono ad allontanarsi in direzione diagonale da un angolo all'altro del grafico, indica una relazione tra reddito e inflazione.Correlazione positiva,reddito tende ad aumentare con un aumento del pil



# Esempio di aggregazione dei dati per paese: Reddito e Inflazione
result_data <- Country_data %>%
  group_by(country) %>%
  summarise(mean_income = mean(income), median_inflation = median(inflation))


plot_ly(result_data, x = ~mean_income, y = ~median_inflation, text = ~country, type = "scatter", mode = "markers") %>%
  layout(title = "Grafico di Reddito e Inflazione per Paese",
         xaxis = list(title = "Media del reddito per paese"),
         yaxis = list(title = "Mediana dell'inflazione"),
         hovermode = "closest")

# Esempio di aggregazione dei dati per paese:mortalità infantile e aspettativa di vita
plot_ly(data = Country_data, x = ~child_mort, y = ~life_expec, text = ~country,type = "scatter", mode = "markers") %>%
  layout(title = " Mortalità Infantile vs Aspettativa di Vita",
         xaxis = list(title = "Mortalità Infantile"),
         yaxis = list(title = "Aspettativa di Vita"),
         hovermode = "closest")
#paesi con una maggiore mortalità infantile tendono a avere un'aspettativa di vita più bassa
#grafico a dispersione che mostra la relazione tra la mortalità infantile e l'aspettativa di vita. correlazione negativa,quindi quando il valore di una variabile aumenta, il valore dell'altra variabile tende a diminuire
# PCA ---------------------------------------------------------------------
pac <- PCA(Country_data, quali.sup = 1)
pac1 <- PCA(Country_data[,2:10])
summary(pac)
summary(pac1)
#la prima variabile sintetica spiega 45% della variabilità totale dei dati. Se guardo la riga "Cumulative % var" vedo quanto ogni volta aggiungendno la variabile sintetica vedo della variabilità totale.
#In "variables" guardando cos2 vedo la correlazione tra la variabile e la dimensione.
#[le freccie vicineindicano correlazione, mentre se sono in quadranti opposti c'è correlazione negativa. 
#la freccia corta vuol dire che la correlazione non è molto alta, la variabile ha poca importanza.

#scegliere quante variabili usare utilizzando la varianza spiegata. Scelgo dove sta il gomito perchè dopo esso diminuisce sempre di meno
fviz_screeplot(pac, addlabels = TRUE, ylim = c(0, 60))

#quali sono le variabili più importanti
fviz_pca_var(pac, col.var = "contrib",col.quali.sup = "magenta",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)
#grafico delle componenti principali per gli individui (categorie) utilizzando le dimensioni 3 e 4. Il secondo mostra un grafico delle componenti principali per le variabili utilizzando le dimensioni 3 e 4. Però queste dimensioni spiegano solo il 24% 

plot(pac, choix="ind", cex=0.8,  title="Individuals PCA graph", axes=1:2)
plot(pac, choix="var", title="Variables PCA graph", axes=3:4)


#seleziona individui basati su un valore soglia del coseno al quadrato (cos2).
plot(pac, cex=0.8, habbilage="Country",select="cos2 0.6")
#seleziona individui basati sui loro contributi
plot(pac, cex=0.8,habbilage="Country", select="contrib 2")

#selezionare specifiche variabili nel grafico delle componenti principali basandosi sui contributi.
plot(pac, choix="var", select="contrib 5")#seleziono la metà

plot(pac, choix="var", select="contrib 1") #seleziono la più "importante"
# Qual è il paese con il numero di esportazioni più alto?
Country_data$country[which.max(Country_data$exports)]
#seleziono la seconda più "importante"
plot(pac, choix="var", select="contrib 2") 
#Come si posiziona Singapore a livello di aspettativa di vita dato che è la seconda variabile più importante?
ggplot(data = Country_data, aes(x = life_expec, y = country)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_point(data = subset(Country_data, country == "Singapore"), color = "red", size = 3) +
  theme_minimal() +
  labs(title = "Aspettativa di Vita per Paese",
       x = "Aspettativa di Vita",
       y = "Paese") +
  theme(legend.position = "none", axis.text.y = element_text(angle = 0, hjust = 1))#in effetti Singapore ha anche la seconda variabile più importante molto alta.

#ed a livello di mortalità infantile?
ggplot(data = Country_data, aes(x = child_mort, y = country)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_point(data = subset(Country_data, country == "Singapore"), color = "red", size = 3) +
  theme_minimal() +
  labs(title = "Mortalità Infantile per Paese",
       x = "Mortalità Infantile",
       y = "Paese") +
  theme(legend.position = "none", axis.text.y = element_text(angle = 0, hjust = 1))
#come avevamo visto dal grafico all'inizio l'aspettativa di vita e la mortalità infantile hanno una correlazione negativa. 




#seleziono individui basati sul coseno al quadrato: solo gli individui con un impatto significativo sulla varianza totale vengono inclusi nel grafico
plot(pac, cex=0.8, select="cos2 0.7", title="Country_data", cex.main=1.1, cex.axis=0.9, shadow=TRUE, auto="y")
#anche qui possiamo vedere come singapore sia molto significativa

#esportazioni stati uniti
ggplot(data = Country_data, aes(x = exports, y = country)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_point(data = subset(Country_data, country == "United States"), aes(x = exports, y = country), color = "red", size = 3) +
  theme_minimal() +
  labs(title = "Esportazioni per Paese",
       x = "Esportazioni",
       y = "Paese") +
  theme(legend.position = "none", axis.text.y = element_text(angle = 0, hjust = 1))



Investigate(pac1)
PCAshiny(pac1)

