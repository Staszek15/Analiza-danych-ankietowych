library(vcdExtra)
library(dplyr)
library(ggplot2)
library(ggmosaic)

data <- read.csv2('C:\\Users\\Filip\\Desktop\\ANKIETYZACJA DANYCH\\personel.csv', header = TRUE)
colnames(data) <- c('D','S','A1','A2','W1', 'W2', 'P', 'Wiek', 'Wyk')
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



### Zad 2 ###

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

ggplot(data = data) +
  geom_mosaic(aes(x = product(A1), fill = D)) +
  theme_mosaic()

ggplot(data = data) +
  geom_mosaic(aes(x = product(W1), fill = D)) +
  theme_mosaic()

ggplot(data = data) +
  geom_mosaic(aes(x = product(S), fill = P)) +
  theme_mosaic()

ggplot(data = data) +
  geom_mosaic(aes(x = product(W1), fill = P), offset = 0.05) +
  theme_mosaic()

mosaicplot(P~W1, data = data)
  







