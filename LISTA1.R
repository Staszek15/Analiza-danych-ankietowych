library(vcdExtra)
library(dplyr)
library(ggplot2)
library(ggmosaic)
library(RColorBrewer)
library(devtools)
library(productplots)
data <- read.csv2('C:\\Users\\Filip\\Desktop\\ANKIETYZACJA DANYCH\\personel.csv', header = FALSE)
colnames(data) <- c('D','S','A1','A2','W1', 'W2', 'P', 'Wiek', 'Wyk')

my_data <- data.frame(data)

my_data$A1 <- factor(my_data$A1)
my_data$S <- factor(my_data$S)
#####################

View(data)
table(data['A1'])

data %>% group_by(A1) %>% count()

################

data_female <- data %>% filter(P ==  'K')

table(data_female['A1'])

data_female %>% group_by(A1) %>% count()
####################

table(data['A2'])

##################### Data Frame dla poszczegółnych grup ############################

data_female <- data %>% filter(P ==  'K')
data_female <- data_female[,!names(data_female) %in% c("P")]

data_male <- data %>% filter(P ==  'M')
data_male <- data_male[,!names(data_male) %in% c("P")]

data_supply <- data %>% filter(D ==  'Z')
data_supply <- data_supply[,!names(data_supply) %in% c("D")]

data_working <- data %>% filter(D ==  'P')
data_working <- data_working[,!names(data_working) %in% c("D")]

data_sale <- data %>% filter(D ==  'S')
data_sale <- data_sale[,!names(data_sale) %in% c("D")]

data_service <- data %>% filter(D ==  'O')
data_service <- data_service[,!names(data_service) %in% c("D")]

data_menager <- data %>% filter(S ==  1)
data_menager <- data_menager[,!names(data_menager) %in% c("S")]

data_no_menager <- data %>% filter(S ==  0)
data_no_menager <- data_no_menager[,!names(data_no_menager) %in% c("S")]

data_up_age25 <- data %>% filter(Wiek ==  1)
data_up_age25 <- data_up_age25[,!names(data_up_age25) %in% c("Wiek")]

data_up_age35 <- data %>% filter(Wiek ==  2)
data_up_age35 <- data_up_age35[,!names(data_up_age35) %in% c("Wiek")]

data_up_age50 <- data %>% filter(Wiek ==  3)
data_up_age50 <- data_up_age50[,!names(data_up_age50) %in% c("Wiek")]

data_over_age50 <- data %>% filter(Wiek ==  4)
data_over_age50 <- data_up_age50[,!names(data_up_age50) %in% c("Wiek")]

data_education_vocat <- data %>% filter(Wyk ==  1)
data_education_vocat <- data_education_vocat[,!names(data_education_vocat) %in% c("Wyk")]

data_education_seco <- data %>% filter(Wyk ==  2)
data_education_seco <- data_education_seco[,!names(data_education_seco) %in% c("Wyk")]

data_education_high <- data %>% filter(Wyk ==  3)
data_education_high <- data_education_high[,!names(data_education_high) %in% c("Wyk")]






### Zad 2 ###

##### Color #######

man <- '#495BF5'

woman <- '#F442E1'

no <- '#03E30A'

yes <- '#F11803'

o <- '#F9E301'

p <- '#7F560D'

s <- '#9C24C6'

z <- '#726E74'


### Tabela wilodzietna W1 i P ###########
structable(W1 ~ P, data)

### Tabela wilodzietna W1 i S ###########
structable(W1 ~ S, data)

### Tabela wilodzietna A1 i D ###########
structable(A1 ~ D, data)


### Zad 3####

#### W1 ##############
df_W1 <- data.frame(table(data$W1))
View(df_W1)

ggplot(df_W1, aes(x='', y = Freq, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  #scale_fill_brewer() +    #to jest spoko też
  theme_void() + 
  #scale_fill_discrete(name="Odpowiedzi")+
  scale_fill_manual(values=c("#2768AE", "#27AEA4", "#30CB51","#70EF3C"), 
                    name="Odpowiedzi")+
  ggtitle('Wykres kołowy dla ... ')



ggplot(df_W1,aes(x=Var1, y=Freq, fill = Var1)) +
  geom_bar(stat = 'identity', color="black")+#, fill="steelblue") +
  xlab("Odpowiedzi") + ylab("Frekwencja")+
  geom_text(aes(label=Freq), vjust=4, color="black", size=3.5)+
  scale_fill_manual(values=c("#2768AE", "#27AEA4", "#30CB51","#70EF3C"))+
  theme_minimal() + 
  theme(legend.position = "none") +
  ggtitle('Wykres słupkowy dla ... ')

############# W2 ###################

df_W2 <- data.frame(table(data$W2))
View(df_W2)

ggplot(df_W2, aes(x='', y = Freq, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  #scale_fill_brewer() +    #to jest spoko też
  theme_void() + 
  #scale_fill_discrete(name="Odpowiedzi")+
  scale_fill_manual(values=c("#2768AE", "#27AEA4", "#30CB51","#70EF3C"), 
                    name="Odpowiedzi")+
  ggtitle('Wykres kołowy dla ... ')



ggplot(df_W2,aes(x=Var1, y=Freq, fill = Var1)) +
  geom_bar(stat = 'identity', color="black")+#, fill="steelblue") +
  xlab("Odpowiedzi") + ylab("Frekwencja")+
  geom_text(aes(label=Freq), vjust=4, color="black", size=3.5)+
  scale_fill_manual(values=c("#2768AE", "#27AEA4", "#30CB51","#70EF3C"))+
  theme_minimal() + 
  theme(legend.position = "none") +
  ggtitle('Wykres słupkowy dla ... ')


#### Zad 4 ############

##### wzór mozaika #####

ggplot(data = my_data) +
  geom_mosaic(aes(x = product(A1,D), fill = A1)) +
  theme_mosaic() +
  scale_fill_manual(values = rev(c(brewer.pal(n = 5, name = "RdBu"))),
                    name='ocena',
                    labels = rev(c('zdecydowanie się zgadzam','zgadzam się',
                                   'trudno powiedzieć','nie zgadzam się', 'zdecydowanie się nie zgadzam'))
                    ) +
  scale_x_productlist(labels = c("Obsługi","Produkcyjni","Sprzedaży","Zaopatrzenia"))+ 
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), text = element_text(size = 15),axis.text.x = element_text(angle = -20, size = 15, vjust = 0)) +
  xlab("Dział") + ylab('') 


ggplot(data = data) +
  geom_mosaic(aes(x = product(W1), fill = D)) +
  theme_mosaic()

ggplot(data = my_data) +
  geom_mosaic(aes(x = product(S,P), fill = S),offset = 0.02) +
  theme_mosaic() +
  #scale_y_continuous("Kierownik" , breaks = c(0.5,0.95), labels = c('Nie','Tak'))+
  scale_x_productlist(labels = c("Kobieta","Mężczyzna")) +
  scale_fill_manual(values=c(yes,no),
                    name='Czy jest \n kierownikiem?',
                    labels = c('Nie','Tak')) +
  theme(axis.text.y=element_blank(),text = element_text(size = 15), axis.text.x = element_text(size = 15),
        axis.ticks.y = element_blank()) +
  ylab('') + xlab('')
  

ggplot(data = data) +
  geom_mosaic(aes(x = product(P,W1), fill = P), offset = 0.02) +
  theme_mosaic() +
  scale_fill_manual(values=c(woman, man), 
                    name='Płeć') +
  xlab("Ocena") + ylab('Płeć')

mosaicplot(P~W1, data = data)
  


#### Zad 5 #####


library(stats)

#### proste niezależne (losowanie ze zwracaniem)

wylosowanie <- sample(nrow(data),size = 0.1*nrow(data), TRUE)

wylosowanie
#### proste zależne (losowanie bez zwracaniem)

NIE_wylosowanie <- sample(nrow(data),size = 0.1*nrow(data), FALSE)

### Zad 6 #####

library(likert)
 
likert(my_data['A1'])

summary(likert(my_data['A1']))

likert.density.plot(likert(my_data['A1']))

likert.bar.plot(likert(my_data['A1']))

#### Zad 7