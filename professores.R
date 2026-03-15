library (tidyverse)
library(cowplot)
library(readxl)

sexpall <- c("#8bc34a", "#ffc107")
racpall <- c("#448aff", "#1565c0", "#009688", "#ff9800", "#f44336", "#ad1457")
contpall <- c('#f6bd60', "#f5cac3", "#84a59d", "#f28482")


#### Dados casos ####
setwd("C:\\Users\\Daniel\\OneDrive - FURB\\Pesquisa\\Francieli")

cont <- read_excel("dados_prof.xlsx", sheet = "contrata")
cont <- cont %>% pivot_longer(cols = c(Pública, Municipal, Estadual, Federal),
                                       names_to = "escola", values_to = "n") 
names(cont)
colnames(cont) <- c("Contrato","ano","escola","n" )

ggplot(cont, aes(fill = Contrato, y = n, x = ano)) + 
  geom_bar(position= "fill", stat="identity") + 
  scale_fill_manual(values = contpall)+ #, labels = c("Feminino", "Masculino"
  labs(x = 'Ano', y = 'Professores (%)', fill = 'Contrato')+
  scale_x_continuous(breaks=c(2015,2025))+
  theme_bw()+ theme(legend.position = "bottom")


sex <- read_excel("dados_prof.xlsx", sheet = "sexo")

sex <- sex %>% pivot_longer(cols = c(fem, mas),
                              names_to = "Sexo", values_to = "n") %>%
  mutate( Sexo = Sexo |>
  recode_values("fem" ~ "Feminino",
                "mas" ~ "Masculino"))

ggplot(sex, aes(fill = Sexo, y = n, x = ano)) + 
  geom_bar(position= "fill", stat="identity") + 
  facet_grid(.~reg,scales="free") +
  scale_fill_manual(values = sexpall)+ #, labels = c("Feminino", "Masculino"
  labs(x = 'Ano', y = 'Professores (%)', fill = 'Sexo') +
  scale_x_continuous(breaks=c(2015,2025)) +
  theme_bw()+ theme(legend.position = "bottom")



rac <- read_excel("dados_prof.xlsx", sheet = "rac")

rac <- rac %>% pivot_longer(cols = c(Branca, PP, ND),
                            names_to = "Raça", values_to = "n") %>%
  mutate( Raça = Raça |>
            recode_values("Branca" ~ "Branca",
                          "PP" ~ "Pretos e Pardos",
                          "ND" ~ "Não Declarados"))

ggplot(rac, aes(fill = Raça, y = n, x = ano)) + 
  geom_bar(position= "fill", stat="identity") + 
  facet_grid(.~reg,scales="free") +
  scale_fill_manual(values = racpall)+ #, labels = c("Feminino", "Masculino"
  labs(x = 'Região', y = 'Professores (%)', fill = 'Raça')+
  scale_x_continuous(breaks=c(2015,2025))+
  theme_bw()+ theme(legend.position = "bottom")

