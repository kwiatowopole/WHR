library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggstatsplot)
library(rstatix)

getwd() #sprawdzenie prawidłowej work directory

data <- read.csv("WHR2023zkontynentami.csv", sep = ";")

#badam to jak wyglądają wczytane dane
str(data) 
summary(data)  

#zmieniam nazwy kolumn na wg mnie bardziej przystępne
data <- data %>%
  rename(
    Country = Country.name,
    Happiness.Score = Ladder.score,
    Standard.Error = Standard.error.of.ladder.score,
    Upper.Whisker = upperwhisker,
    Lower.Whisker = lowerwhisker,
    Log.GDP.Per.Capita = Logged.GDP.per.capita,
    Social.Support = Social.support,
    Life.Expectancy = Healthy.life.expectancy,
    Freedom.Choices = Freedom.to.make.life.choices,
    Generosity = Generosity,
    Corruption.Perception = Perceptions.of.corruption,
    Dystopia.Score = Ladder.score.in.Dystopia,
    Explained.By.GDP = Explained.by..Log.GDP.per.capita,
    Explained.By.Social.Support = Explained.by..Social.support,
    Explained.By.Life.Expectancy = Explained.by..Healthy.life.expectancy,
    Explained.By.Freedom = Explained.by..Freedom.to.make.life.choices,
    Explained.By.Generosity = Explained.by..Generosity,
    Explained.By.Corruption = Explained.by..Perceptions.of.corruption,
    Dystopia.Residual = Dystopia...residual,
    Continent = Geographical.continent
  )
 
#charakterystyka danych



#wizualizacja na wykresach skrzypkowych
data %>%
  select(Happiness.Score, Log.GDP.Per.Capita, Social.Support, 
         Life.Expectancy, Freedom.Choices, Generosity, Corruption.Perception, Standard.Error, Upper.Whisker, Lower.Whisker) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Variable, y = Value, fill = Variable)) +  #usuwanie podzialu na grupy
  geom_violin(alpha = 0.7, color = "black") +
  facet_wrap(~Variable, scales = "free") +  #każda zmienna może mieć inne skalowanie
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  labs(title = "Rozkład zmiennych",
       x = "Zmienna",
       y = "Wartość")



#Pytanie badawcze - Kopera


#Czy istnieje związek, (a jeżeli tak to jak on wygląda) między PKB a poziomem szczęścia (czyli Log.GDP.Per.Capita a Happiness.Score)?


ggscatterstats(
  data = data,  
  x = Log.GDP.Per.Capita,  #niezależna
  y = Happiness.Score,  #zależna
  xlab = "Wsparcie socjalne",  
  ylab = "Poziom szczęścia",   
  title = "Zależność między log PKB per capita a poziomem szczęścia", 
  point.args = list(size = 2, color = "lightgreen"),  
  type = "parametric",  #regresja
  conf.level = 0.95  #poziom ufności
)


model1 <- lm(data$Happiness.Score  ~  data$Log.GDP.Per.Capita) #model regresji

#wyświetlenie wyników 
summary(model1)

#pytanie 2 (szczescie a wsparcie socjalne)


ggscatterstats(
  data = data,  
  x = Social.Support,  #niezależna
  y = Happiness.Score,  #zależna
  xlab = "PKB per capita",  
  ylab = "Poziom szczęścia",   
  title = "Zależność między PKB na osobę a poziomem szczęścia", 
  point.args = list(size = 2, color = "lightblue"),  
  type = "parametric",  #regresja
  conf.level = 0.95  #poziom ufności
)

model2 <- lm(data$Happiness.Score  ~ data$Social.Support)

summary(model2)



#dodatkowe wykresy
ExplainedPKB <- ggplot(data, aes(y = `Explained.By.GDP`)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot: Wyjaśnione przez Log PKB per capita", y = "Wkład") +
  theme_minimal()


ExplainedSocialSupport <- ggplot(data, aes(y = `Explained.By.Social.Support`)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot: Wyjaśnione przez Wsparcie społeczne", y = "Wkład") +
  theme_minimal()

print(ExplainedPKB)
print(ExplainedSocialSupport)


#pytania Amelia Krupa

# Usuwanie braków danych
spearman_data <- data %>%
  select(Log.GDP.Per.Capita, Freedom.Choices) %>%
  na.omit()

# Test Shapiro-Wilka dla Freedom.Choices
shapiro_test_support <- shapiro.test(spearman_data$Freedom.Choices)
print("Shapiro-Wilk test dla Social.Support:")
print(shapiro_test_support)

# Analiza korelacji (Spearman) dla Log.GDP.Per.Capita i Social.Support
spearman_test <- cor.test(data$Log.GDP.Per.Capita, data$Freedom.Choices, method = "spearman")
print(spearman_test)


# Wizualizacja zależności
wykres_gdp_social <- data %>%
  ggplot(aes(x = Log.GDP.Per.Capita, y = Freedom.Choices)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "darkgreen", se = TRUE) +
  labs(
    title = "Model regresji liniowej: PKB per capita a wsparcie społeczne",
    x = "Log GDP per capita",
    y = "Freedom.Choices"
  ) +
  theme_minimal()
print(wykres_gdp_social)


# Pytanie badawcze Jarosław Kwiatkowski
# Czy poziom szczęścia różni się istotnie między regionami geograficznymi?

# Wizualizacja różnic w poziomie szczęścia
# Liczba obserwacji
counts<- data%>%
  group_by(Continent) %>%
  count()
counts


ggplot(data, aes(x = Continent, y = Happiness.Score, fill = Continent)) +
  geom_boxplot( outlier.colour = "red",
                outlier.size=2) +
  labs(title = "Porównanie poziomu szczęścia między regionami",
       x = "Kontynent", y = "Poziom szczęścia") +
  scale_x_discrete(labels=paste(counts$Continent,"\n n = ", counts$n))


#łączenie danych do jednej grupy
danefiltr<-data
danefiltr$ContinentMerged1<-ifelse(danefiltr$Continent %in% c("South America", "North America"), 
                                   "America", 
                                   danefiltr$Continent)
table(danefiltr$ContinentMerged1)
danefiltr$ContinentMerged2<-ifelse(danefiltr$ContinentMerged1 %in% c("Australia and Oceania", "Asia"), 
                                   "Asia and Oceania", 
                                   danefiltr$ContinentMerged1)
table(danefiltr$ContinentMerged2)
#Liczba obserwacji
countsfiltr<- danefiltr%>%
  group_by(ContinentMerged2) %>%
  count()
countsfiltr

ggplot(danefiltr, aes(x = ContinentMerged2, y = Happiness.Score, fill = ContinentMerged2)) +
  geom_boxplot( outlier.colour = "red",
                outlier.size=2) +
  labs(title = "Porównanie poziomu szczęścia między regionami",
       x = "Kontynent", y = "Poziom szczęścia") +
  scale_x_discrete(labels=paste(countsfiltr$ContinentMerged2,"\n n = ", countsfiltr$n))
#sprawdzenie normalności rozkładu danych przez test shapiro na każdym kontynencie

results <- danefiltr %>%
  group_by(ContinentMerged2) %>%
  summarise(
    W = shapiro.test(Happiness.Score)$statistic,
    p_value = shapiro.test(Happiness.Score)$p.value
  )
print(results)
# Test statystyczny Kruskal z powodu nienormalności rozkładu danych
kruskal.test(Happiness.Score ~ ContinentMerged2, data = danefiltr)

# Dla najszerszego badania naszej hipotezy użyjmy jeden z testów post hoc - test Dunn'a   

d<-dunn_test(Happiness.Score ~ ContinentMerged2, data=danefiltr)
d

ggbetweenstats(data=danefiltr, 
               x=ContinentMerged2, 
               y=Happiness.Score, 
               type="nonparametric") +
  labs(title = "Porównanie poziomu szczęścia między regionami",
       x = "Kontynent", y = "Poziom szczęścia")