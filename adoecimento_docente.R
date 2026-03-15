library(MASS)
library (tidyverse)
library(readxl)
library(GGally)
library(glmmTMB)
library(DHARMa)
library(lattice)
library(cowplot)
library(MuMIn)


sexpall <- c("#8bc34a", "#ffc107")
racpall <- c("#448aff", "#1565c0", "#009688", "#ff9800", "#f44336", "#ad1457")

#### Dados casos ####
setwd("C:\\Users\\Daniel\\OneDrive - FURB\\Pesquisa\\Francieli")

df <- read_excel("MENTBRtotal.xlsx", sheet = "MENTBRtotal")
names(df)
unique(df$ID_OCUP_DESC)
print(df$ID_OCUP_DESC)

df <- df %>% filter (NU_ANO >= 2013, ID_OCUP_DESC != c("NA", "desc")) # 
df <- df %>% select(NU_ANO, UF_NOT_DESC, CS_SEXO, CS_RACA, NÍVEL_ENSINO) #Seleciona apenas colunas de interesse

  
str(df)
summary(df) #sumario das variaveis
complete.cases(df) #quais linhas não tem NA
colSums(is.na(df)) #somatorio de NA nas colunas

#Trocar NA por 'Indefinido'
unique(df$NÍVEL_ENSINO)
df$NÍVEL_ENSINO <- replace_na(df$NÍVEL_ENSINO,"Indefinido")
unique(df$NÍVEL_ENSINO)

# Troca o Falta pelo Indefinido
df$NÍVEL_ENSINO <- replace(df$NÍVEL_ENSINO, df$NÍVEL_ENSINO == 'Falta', "Indefinido") 
unique(df$NÍVEL_ENSINO)

df$CS_RACA <- replace(df$CS_RACA, df$CS_RACA == 'NA', "Indefinido") 
unique(df$CS_RACA)

# Troca nome dos estados por sigla
unique(df$UF_NOT_DESC)

df$UF <- recode(df$UF_NOT_DESC, 'São Paulo'='SP', "Bahia"='BA', "Pernambuco"="PE","Paraíba"="PB", "Ceará"="CE",
                          "Minas Gerais"="MG", "Goiás"="GO","Rio de Janeiro"="RJ", "Rio Grande do Sul"="RS", "Mato Grosso"="MT",
                          "Pará"="PA", "Paraná"="PR", "Tocantins"="TO", "Mato Grosso do Sul"="MS", "Maranhão"="MA",
                          "Sergipe"="SE", "Santa Catarina"="SC", "Espírito Santo"="ES", "Rio Grande do Norte"="RN", 
                          "Alagoas"="AL", "Rondônia"="RO", "Amazonas"="AM", "Roraima"="RR", "Acre"="AC",
                          "Distrito Federal" = "DF", "Piauí" = "PI")
       

# Organiza os daddos por ano, estado e nível de ensino ####
unique(df$CS_RACA)

#df_res <- df %>% group_by(UF_NOT_DESC, NU_ANO, CS_SEXO, CS_RACA, NÍVEL_ENSINO) %>% 
#                  summarise(casos = n()) 

df_total <- df %>% group_by(UF, NU_ANO, NÍVEL_ENSINO) %>%  #, CS_SEXO, CS_RACA
                 summarise(casos_total = n(),
            prop_F = sum(CS_SEXO == "F") / casos_total,
            prop_M = sum(CS_SEXO == "M") / casos_total,
            casos_F = casos_total*prop_F,
            casos_M = casos_total*prop_M,
            prop_igno = sum(CS_RACA == '9') / casos_total,
            prop_indef = sum(CS_RACA == 'Indefinido') / casos_total,
            prop_pard = sum(CS_RACA == '4') / casos_total,
            prop_ama = sum(CS_RACA == '3') / casos_total,
            prop_pret = sum(CS_RACA == '2') / casos_total,
            prop_bran = sum(CS_RACA == '1') / casos_total,
            casos_igno = casos_total*prop_igno,
            casos_indef = casos_total*prop_indef,
            casos_pard = casos_total*prop_pard,
            casos_ama = casos_total*prop_ama,
            casos_pret = casos_total*prop_pret,
            casos_bran = casos_total*prop_bran) 

hist(df_total$casos_total)

write.csv(df_total, 'tabela_uf_ano_nivel.csv')

# Organiza os daddos por ano e estado ####

df_ano_uf <- df %>% group_by(UF, NU_ANO) %>%  
  summarise(casos_total = n(),
            prop_F = sum(CS_SEXO == "F") / casos_total,
            prop_M = sum(CS_SEXO == "M") / casos_total,
            casos_F = casos_total*prop_F,
            casos_M = casos_total*prop_M,
            prop_igno = sum(CS_RACA == '9') / casos_total,
            prop_indef = sum(CS_RACA == 'Indefinido') / casos_total,
            prop_pard = sum(CS_RACA == '4') / casos_total,
            prop_ama = sum(CS_RACA == '3') / casos_total,
            prop_pret = sum(CS_RACA == '2') / casos_total,
            prop_bran = sum(CS_RACA == '1') / casos_total,
            casos_igno = casos_total*prop_igno,
            casos_indef = casos_total*prop_indef,
            casos_pard = casos_total*prop_pard,
            casos_ama = casos_total*prop_ama,
            casos_pret = casos_total*prop_pret,
            casos_bran = casos_total*prop_bran) 

hist(df_ano_uf$casos_total)

write.csv(df_ano_uf, 'tabela_uf_ano.csv')

# Gráfico 

negbinom.params <- fitdistr(df_ano_uf$casos_total,"negative binomial")$estimate #, method = "SANN"
mybinwidth = 1

ggplot(df_ano_uf, aes(x = casos_total)) +
  geom_histogram(fill = "#009688", color = "black", binwidth = mybinwidth) + #bins = 60,
  stat_function(fun=function(x,size,mu) mybinwidth * nrow(df_ano_uf) * dnbinom(x,size = size, mu = mu),
                args=fitdistr(df_ano_uf$casos_total,"negative binomial")$estimate, #, method="SANN"
                xlim=c(1,60),n=60, color = '#ff9800', lwd = 1) +theme_bw() + 
  labs(y = 'Ocorrências (n)', x = 'Casos por ano por Estado (n)')

# Organiza dados apenas por ano ####


df_ano <- df %>% group_by(NU_ANO) %>% 
  summarise(casos_total = n(),
            prop_F = sum(CS_SEXO == "F") / casos_total,
            prop_M = sum(CS_SEXO == "M") / casos_total,
            casos_F = casos_total*prop_F,
            casos_M = casos_total*prop_M,
            prop_igno = sum(CS_RACA == '9') / casos_total,
            prop_indef = sum(CS_RACA == 'Indefinido') / casos_total,
            prop_pard = sum(CS_RACA == '4') / casos_total,
            prop_ama = sum(CS_RACA == '3') / casos_total,
            prop_pret = sum(CS_RACA == '2') / casos_total,
            prop_bran = sum(CS_RACA == '1') / casos_total,
            casos_igno = casos_total*prop_igno,
            casos_indef = casos_total*prop_indef,
            casos_pard = casos_total*prop_pard,
            casos_ama = casos_total*prop_ama,
            casos_pret = casos_total*prop_pret,
            casos_bran = casos_total*prop_bran)

write.csv(df_ano, 'tabela_ano.csv')

#Gráfico sexo 
names(df_ano)
df_ano_sex <- df_ano %>% dplyr::select('NU_ANO','casos_F','casos_M') %>% 
  pivot_longer(!c(NU_ANO), names_to = "sexo", values_to = "casos")

a <- ggplot(df_ano_sex, aes(fill=sexo, y=casos, x=NU_ANO)) + 
  geom_bar(position= "stack", stat="identity") + 
  scale_fill_manual(values=sexpall,labels = c("Feminino", "Masculino"))+
  labs(x = 'Ano', y = 'Casos (n)', fill = 'Sexo')+
  scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025))+
  theme_bw()+ theme(legend.position = "bottom")

b <- ggplot(df_ano_sex, aes(fill=sexo, y=casos, x=NU_ANO)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values=sexpall,labels = c("Feminino", "Masculino"))+
  labs(x = 'Ano', y = 'Casos (%)', fill = 'Sexo')+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025))+
  theme_bw()+ theme(legend.position = "bottom")

plot_grid(a,b,labels = 'AUTO')

#Gráfico raça 
names(df_ano)
df_ano_rac <- df_ano %>% dplyr::select("NU_ANO","casos_igno","casos_indef","casos_pard","casos_ama","casos_pret","casos_bran") %>% 
  pivot_longer(!c(NU_ANO), names_to = "raca", values_to = "casos")

a<-ggplot(df_ano_rac, aes(fill=raca, y=casos, x=NU_ANO)) + 
  geom_bar(position= "stack", stat="identity") + 
  scale_fill_manual(values=racpall,labels = c("Amarela", "Branca", "Ignorada", "Indefinida", "Parda", "Preta"))+
  labs(x = 'Ano', y = 'Casos (n)', fill = 'Raça')+
  scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025))+
  theme_bw()+ theme(legend.position = "bottom")

b<-ggplot(df_ano_rac, aes(fill=raca, y=casos, x=NU_ANO)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values=racpall,labels = c("Amarela", "Branca", "Ignorada", "Indefinida", "Parda", "Preta"))+
  labs(x = 'Ano', y = 'Casos (%)', fill = 'Raça')+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025))+
  theme_bw()+ theme(legend.position = "bottom")

plot_grid(a,b,labels = 'AUTO')

# Organiza dados apenas por estado ####

df_uf <- df %>% group_by(UF) %>% 
  summarise(casos_total = n(),
            prop_F = sum(CS_SEXO == "F") / casos_total,
            prop_M = sum(CS_SEXO == "M") / casos_total,
            casos_F = casos_total*prop_F,
            casos_M = casos_total*prop_M,
            prop_igno = sum(CS_RACA == '9') / casos_total,
            prop_indef = sum(CS_RACA == 'Indefinido') / casos_total,
            prop_pard = sum(CS_RACA == '4') / casos_total,
            prop_ama = sum(CS_RACA == '3') / casos_total,
            prop_pret = sum(CS_RACA == '2') / casos_total,
            prop_bran = sum(CS_RACA == '1') / casos_total,
            casos_igno = casos_total*prop_igno,
            casos_indef = casos_total*prop_indef,
            casos_pard = casos_total*prop_pard,
            casos_ama = casos_total*prop_ama,
            casos_pret = casos_total*prop_pret,
            casos_bran = casos_total*prop_bran)

reg <- c('NORTE',	'NORDESTE',	'NORTE',	'NORDESTE',	'NORDESTE','CENTRO-OESTE',
	'SUDESTE',	'CENTRO-OESTE',	'NORDESTE',	'SUDESTE', 'CENTRO-OESTE', 
	'CENTRO-OESTE',	'NORTE',	'NORDESTE',	'NORDESTE',	'NORDESTE',	'SUL',	'SUDESTE',
	'NORDESTE',	 'NORTE', 'NORTE',	'SUL',	'SUL',	'NORDESTE',	'SUDESTE',	'NORTE')


df_uf$reg <- reg
write.csv(df_uf, 'tabela_uf.csv')

#Gráfico sexo 
names(df_uf)
df_uf_sex <- df_uf %>% dplyr::select("reg","UF","casos_F","casos_M") %>% 
  pivot_longer(!c(reg,UF), names_to = "sexo", values_to = "casos")

a<-ggplot(df_uf_sex, aes(fill=sexo, y=casos, x=UF)) + 
  geom_bar(position= "stack", stat="identity") + facet_grid(.~reg,scales="free")+ 
  scale_fill_manual(values=sexpall,labels = c("Feminino", "Masculino"))+
  labs(x = 'Estado', y = 'Casos (n)', fill = 'Sexo')+
  theme_bw()+ theme(legend.position = "bottom")

b<-ggplot(df_uf_sex, aes(fill=sexo, y=casos, x=UF)) + 
  geom_bar(position="fill", stat="identity") + facet_grid(.~reg,scales="free")+ 
  scale_fill_manual(values=sexpall,labels = c("Feminino", "Masculino"))+
  labs(x = 'Estado', y = 'Casos (%)', fill = 'Sexo')+
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+ theme(legend.position = "bottom")

plot_grid(a,b,nrow = 2, labels = 'AUTO')

#Gráfico raça 
names(df_uf)
df_uf_rac <- df_uf %>% dplyr::select("reg","UF","casos_igno","casos_indef","casos_pard","casos_ama","casos_pret","casos_bran") %>% 
  pivot_longer(!c(reg,UF), names_to = "raca", values_to = "casos")

a<-ggplot(df_uf_rac, aes(fill=raca, y=casos, x=UF)) + 
  geom_bar(position= "stack", stat="identity") + facet_grid(.~reg,scales="free")+ 
  scale_fill_manual(values=racpall,labels = c("Amarela", "Branca", "Ignorada", "Indefinida", "Parda", "Preta"))+ #
  labs(x = 'Estado', y = 'Casos (n)', fill = 'Raça')+
  theme_bw()+ theme(legend.position = "bottom")


b<-ggplot(df_uf_rac, aes(fill=raca, y=casos, x=UF)) + 
  geom_bar(position="fill", stat="identity") + facet_grid(.~reg,scales="free")+ 
  scale_fill_manual(values=racpall,labels = c("Amarela", "Branca", "Ignorada", "Indefinida", "Parda", "Preta"))+
  labs(x = 'Estado', y = 'Casos (%)', fill = 'Raça')+
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+ theme(legend.position = "bottom")

plot_grid(a,b,nrow = 2, labels = 'AUTO')

# Organiza dados apenas por nivel de ensino ####

df_ne <- df %>% group_by(NÍVEL_ENSINO) %>% 
  summarise(casos_total = n(),
            prop_F = sum(CS_SEXO == "F") / casos_total,
            prop_M = sum(CS_SEXO == "M") / casos_total,
            casos_F = casos_total*prop_F,
            casos_M = casos_total*prop_M,
            prop_igno = sum(CS_RACA == '9') / casos_total,
            prop_indef = sum(CS_RACA == 'Indefinido') / casos_total,
            prop_pard = sum(CS_RACA == '4') / casos_total,
            prop_ama = sum(CS_RACA == '3') / casos_total,
            prop_pret = sum(CS_RACA == '2') / casos_total,
            prop_bran = sum(CS_RACA == '1') / casos_total,
            casos_igno = casos_total*prop_igno,
            casos_indef = casos_total*prop_indef,
            casos_pard = casos_total*prop_pard,
            casos_ama = casos_total*prop_ama,
            casos_pret = casos_total*prop_pret,
            casos_bran = casos_total*prop_bran)

write.csv(df_ne, 'tabela_nivel_ensino.csv')

#Gráfico sexo 
names(df_ne)
df_ne_sex <- df_ne %>% select("NÍVEL_ENSINO","casos_F","casos_M") %>% 
  pivot_longer(!c(NÍVEL_ENSINO), names_to = "sexo", values_to = "casos")

ggplot(df_ne_sex, aes(fill=sexo, y=casos, x=NÍVEL_ENSINO)) + 
  geom_bar(position= "stack", stat="identity") + 
  scale_fill_manual(values=sexpall,labels = c("Feminino", "Masculino"))+
  labs(x = 'Nível de ensino', y = 'Casos (n)', fill = 'Sexo')+
  theme_bw()+ coord_flip()+ theme(legend.position = "bottom")

ggplot(df_ne_sex, aes(fill=sexo, y=casos, x=NÍVEL_ENSINO)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values=sexpall,labels = c("Feminino", "Masculino"))+
  labs(x = 'Nível de ensino', y = 'Casos (%)', fill = 'Sexo')+
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+ coord_flip()+ theme(legend.position = "bottom")

#Gráfico raça 
names(df_ne)
df_ne_rac <- df_ne %>% select("NÍVEL_ENSINO","casos_igno","casos_indef","casos_pard","casos_ama","casos_pret","casos_bran") %>% 
  pivot_longer(!c(NÍVEL_ENSINO), names_to = "raca", values_to = "casos")

ggplot(df_ne_rac, aes(fill=raca, y=casos, x=NÍVEL_ENSINO)) + 
  geom_bar(position= "stack", stat="identity") + 
  scale_fill_manual(values=racpall,labels = c("Amarela", "Branca", "Ignorada", "Indefinida", "Parda", "Preta"))+ #
  labs(x = 'Nível de Ensino', y = 'Casos (n)', fill = 'Raça')+ theme_bw()+ 
  coord_flip()+ theme(legend.position = "bottom")
  #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  

ggplot(df_ne_rac, aes(fill=raca, y=casos, x=NÍVEL_ENSINO)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values=racpall,labels = c("Amarela", "Branca", "Ignorada", "Indefinida", "Parda", "Preta"))+
  labs(x = 'Nível de Ensino', y = 'Casos (%)', fill = 'Raça')+
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+ coord_flip()+ theme(legend.position = "bottom")



#### Dados socio-econômicos #### 

# Carrega a tabela excel
df_escola <- read_excel('Base de dados_dissertação.xlsx', sheet = 'Escola',col_names = T)
names(df_escola)
str(df_escola)

# Resume os dados por estado e ano
df_escola_ano_uf <- df_escola %>% group_by(UF, ANO) %>%  
  summarise(alunos_prof = mean(ALUNOS_PROFESSOR),
            profs = sum(NÚMERO_PROFESSORES),
            profs_F = sum(SEXO_FEMININO),
            profs_M = sum(SEXO_MASCULINO),
            profs_velhos = ((sum(idade_50_54)+sum(idade_55_59)+sum(idade_60))/profs),
            profs_novos = (1 - profs_velhos)
  ) #É possivel incluir outras variaveis presentes na tabela df_escola aqui


df_desenvolvimento <- read_excel('Base de dados_dissertação.xlsx', sheet = 'Indicadores de Desenvolvimento',col_names = T)
names(df_desenvolvimento)
str(df_desenvolvimento)
df_desenvolvimento <- df_desenvolvimento %>% filter(ANO > 2012) #filtra apenas 2013 pra frente

# Os NA estavam sendo entendidos como palavras, trocar para NA
df_desenvolvimento$GINI <- replace(df_desenvolvimento$GINI, df_desenvolvimento$GINI == 'NA', NA) 
df_desenvolvimento$IDH <- replace(df_desenvolvimento$IDH, df_desenvolvimento$IDH == 'NA', NA)
df_desenvolvimento$trab_domestico_fem <- replace(df_desenvolvimento$trab_domestico_fem, df_desenvolvimento$trab_domestico_fem == 'NA', NA) 
df_desenvolvimento$desemprego_fem <- replace(df_desenvolvimento$desemprego_fem, df_desenvolvimento$desemprego_fem == 'NA', NA) 
df_desenvolvimento$REMUNERAÇÃO <- replace(df_desenvolvimento$REMUNERAÇÃO, df_desenvolvimento$REMUNERAÇÃO == 'NA', NA) 

str(df_desenvolvimento) #Colunas estão sendo entendidas como palavras em vez de numero

df_desenvolvimento$GINI <- as.numeric(df_desenvolvimento$GINI)
df_desenvolvimento$IDH <- as.numeric(df_desenvolvimento$IDH)
df_desenvolvimento$trab_domestico_fem <- as.numeric(df_desenvolvimento$trab_domestico_fem)
df_desenvolvimento$desemprego_fem <- as.numeric(df_desenvolvimento$desemprego_fem)
df_desenvolvimento$REMUNERAÇÃO <- as.numeric(df_desenvolvimento$REMUNERAÇÃO)

str(df_desenvolvimento)
summary(df_desenvolvimento)
complete.cases(df_desenvolvimento)


#### Hipótese 1 - nº de casos é afetado pelo desenvolvimento do estado (GINI e IDH), renda, e carga de trabalho (alunos/professor) ####

#Para essa análise vamos usar um Modelo binomial negativo truncado a zero com exposição e efeitos aleatórios
# binomial negativo: regressão para dados de contagem (nº de casos)
# truncado a zero: não temos combinações UF|ANO com zero casos
# exposição: leva em consideração o nº total de professores no UF|ANO
# efeito aleatório: leva em consideração diferenças socio-econômicas dos UF não contempladas nas variáveis preditoras 
names(df_ano_uf)

#Seleciona apenas variaveis de interesse da tabela que tem dados por UF|ANO
df_h1 <- df_ano_uf %>% select(UF, NU_ANO, casos_total, casos_F, casos_M) %>% rename(ANO = NU_ANO) 

names(df_escola)

#Junta a tabela de casos com as tabelas de preditores da escola e desenvolvimento
df_h1 <- left_join(df_h1,df_escola_ano_uf, by = c('ANO', 'UF'))
df_h1 <- left_join(df_h1, df_desenvolvimento, by = c('ANO', 'UF'))
df_h2 <- df_h1

names(df_h1)
mean(df_h1$casos_total)
sd(df_h1$casos_total)

df_h1$GINI <- df_h1$GINI*10
df_h1$IDH <- df_h1$IDH*10

# Modelo para casos totais
model_h1 <- glmmTMB(
  casos_total ~ GINI + IDH + REMUNERAÇÃO + alunos_prof + (1 | ANO) + offset(log(profs)),
  data = df_h1,
  family = truncated_nbinom2()
)

summary(model_h1) # sumário de resultados do modelo
simulateResiduals(fittedModel = model_h1, n = 1000, plot = T) # teste se está tudo ok com o modelo


r.squaredGLMM(model_h1)


df_h1 <- df_h1 %>% select(UF, ANO, casos_total, alunos_prof, GINI, IDH, REMUNERAÇÃO) %>% drop_na()
df_h1$casos_pred <- predict(model_h1, type = "response") 


# Gráficos

a<-ggplot(data = df_h1)+
  geom_point(aes(x = GINI, y = casos_total), shape = 21, size = 4, fill = '#448aff', color = '#1565c0') +
  geom_smooth(aes(x=GINI,y=casos_total), color = '#ad1457', method = 'glm', 
              method.args = list(family = MASS::negative.binomial(theta = 1)))+
  labs(x = 'Índice de GINI', y = 'Casos (n)') + theme_bw()

b<-ggplot(data = df_h1)+
  geom_point(aes(x = IDH, y = casos_total), shape = 21, size = 4, fill = '#448aff', color = '#1565c0') +
  geom_smooth(aes(x=IDH,y=casos_total), color = '#ad1457', method = 'glm', 
              method.args = list(family = MASS::negative.binomial(theta = 1)))+
  labs(x = 'IDH', y = 'Casos (n)') + theme_bw()

c<-ggplot(data = df_h1)+
  geom_point(aes(x = alunos_prof, y = casos_total), shape = 21, size = 4, fill = '#448aff', color = '#1565c0') +
  geom_smooth(aes(x=alunos_prof,y=casos_total), color = '#ad1457', method = 'glm', 
              method.args = list(family = MASS::negative.binomial(theta = 1)))+
  labs(x = 'Alunos por professor (n)', y = 'Casos (n)') + theme_bw()

d<-ggplot(data = df_h1)+
  geom_point(aes(x = casos_total, y = casos_pred), shape = 21, size = 4, fill = '#448aff', color = '#1565c0') +
  geom_smooth(aes(x=casos_total,y=casos_pred), color = '#ad1457', method = 'lm',se = F,fullrange = T)+
  geom_abline(intercept = 0, slope = 1)+
  xlim(0,65)+ylim(0,65)+
  labs(x = 'Casos observados (n)', y = 'Casos preditos pelo modelo (n)') + theme_bw()

plot_grid(a,b,c,d,labels = 'AUTO')


#### Hipótese 2 - nº de casos feminino é afetado pela sobrecarga de trabalho domestico, desemprego e GINI ####

names(df_h2)
mean(df_h2$casos_F)
sd(df_h2$casos_F)

model_h2 <- glmmTMB(
  casos_F ~ GINI + IDH + REMUNERAÇÃO + alunos_prof + trab_domestico_fem + (1 | ANO) + offset(log(profs_F)),
  data = df_h2,
  family = nbinom2() # Existem combinações com zero casos, por isso deixaremos de usar o modelo truncado a zero
)


summary(model_h2) # sumário de resultados do modelo
simulationOutput <- simulateResiduals(fittedModel = model_h2, n = 1000, plot = T) # teste se está tudo ok com o modelo
ranef(model_h2)  # Extract random effects

confint(model_h2)

df_h2 <- df_h2 %>% select(UF, ANO, casos_F, alunos_prof, GINI, IDH, REMUNERAÇÃO, trab_domestico_fem) %>% drop_na()
df_h2$casos_pred <- predict(model_h2, type = "response") 


# Gráficos

a<-ggplot(data = df_h2)+
  geom_point(aes(x = GINI, y = casos_F), shape = 21, size = 4, fill = '#f44336', color = '#ad1457') +
  geom_smooth(aes(x=GINI,y=casos_F), color = '#1565c0', method = 'glm', 
              method.args = list(family = MASS::negative.binomial(theta = 1)))+
  labs(x = 'Índice de GINI', y = 'Casos (n)') + theme_bw()

b<-ggplot(data = df_h2)+
  geom_point(aes(x = IDH, y = casos_F), shape = 21, size = 4, fill = '#f44336', color = '#ad1457') +
  geom_smooth(aes(x=IDH,y=casos_F), color = '#1565c0', method = 'glm', 
              method.args = list(family = MASS::negative.binomial(theta = 1)))+
  labs(x = 'IDH', y = 'Casos (n)') + theme_bw()

c<-ggplot(data = df_h2)+
  geom_point(aes(x = alunos_prof, y = casos_F), shape = 21, size = 4, fill = '#f44336', color = '#ad1457') +
  geom_smooth(aes(x=alunos_prof,y=casos_F), color = '#1565c0', method = 'glm', 
              method.args = list(family = MASS::negative.binomial(theta = 1)))+
  labs(x = 'Alunos por professor (n)', y = 'Casos (n)') + theme_bw()

d<-ggplot(data = df_h2)+
  geom_point(aes(x = casos_F, y = casos_pred), shape = 21, size = 4, fill = '#f44336', color = '#ad1457') +
  geom_smooth(aes(x=casos_F,y=casos_pred), color = '#1565c0', method = 'lm',se = F,fullrange = T)+
  geom_abline(intercept = 0, slope = 1)+
  xlim(0,65)+ylim(0,65)+
  labs(x = 'Casos observados (n)', y = 'Casos preditos pelo modelo (n)') + theme_bw()

e<-ggplot(data = df_h2)+
  geom_point(aes(x = trab_domestico_fem, y = casos_F), shape = 21, size = 4, fill = '#f44336', color = '#ad1457') +
  geom_smooth(aes(x=trab_domestico_fem,y=casos_F), color = '#1565c0', method = 'glm', 
              method.args = list(family = MASS::negative.binomial(theta = 1)))+
  labs(x = 'Trabalho doméstico feminino (h/sem)', y = 'Casos (n)') + theme_bw()

plot_grid(a,b,c,d,labels = 'AUTO')




