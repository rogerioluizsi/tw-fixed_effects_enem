library(tidyverse)


notas <-read.csv("~/projects/panel/features_engineering/ALL_SCHOOLS_v2.csv")
notas<- notas%>%filter(IN_TP_ESCOLA== 'Municipal+Estadual')
notas<- notas%>%filter(!(CO_ANO == 2009  | CO_ANO == 2010))
uf <- ("~/data/geodata/uf.json") %>%
  st_read()%>%dplyr::select(UF_05, GEOCODIGO)
st_geometry(uf) <- NULL 
notas$CO_UF<- as.factor(notas$CO_UF)
analise<- left_join(notas, uf, by = c("CO_UF" = "GEOCODIGO"))
analise <- analise%>%group_by(CO_ESCOLA)%>%mutate(n = n())%>%ungroup%>%
  filter(n==9)%>%dplyr::select(-n)

unique(analise%>%group_by(CO_ESCOLA)%>%summarise(n=n())%>%pull(n))
unique(analise$CO_ANO)

features = c(
  'EDU_PAI',
  'EDU_MAE', 
  'RENDA_PERCAPITA', 
  #"TP_COR_RACA_1.0","TP_COR_RACA_2.0","TP_COR_RACA_3.0","TP_COR_RACA_4.0","TP_COR_RACA_5.0",
  'TITULACAO', 
  #'NU_CIENCIA_NATUREZA','NU_CIENCIAS_HUMANAS', 'NU_LINGUAGENS_CODIGOS', 'NU_MATEMATICA',
  #'NU_ESCOLAS', 
  #'NU_LICENCIADOS', 
  #'NU_IDADE', 
  'QT_COMP_ALUNO',
  'NU_NOTA_GERAL',
  'IN_FORM_DOCENTE'
  
  
)

average<-analise%>%select(c(features, 'CO_ESCOLA', 'UF_05', 'CO_ANO'))
average$CO_ESCOLA<-as.character(average$CO_ESCOLA)
#standardization
#pp = preProcess(average, method = c("range"))
#average <- predict(pp, average)
#average<-clip_tail(average, features)

#average<-average%>%filter(CO_ESCOLA=="35058836" | CO_ESCOLA=="23025905")

temp<-average%>%select(c(CO_ESCOLA, UF_05))%>%distinct_all()
temp$CO_ESCOLA<- as.character(temp$CO_ESCOLA)


for (i in features){
  #col<-get(i,average)
  #slope_nacional<-analise%>%group_by(CO_ANO)%>%summarise(avg=mean(get(i)))%>%pull(avg)
  #df <- data.frame( x=c(1:9),
  #                    y= slope_nacional)
  #c <- lm(y ~ x, data=df)
  #slope_nacional = c$coefficients[2]
  
  slope<-average %>% split(.$CO_ESCOLA) %>% map(function(x) {
    df <- data.frame( x=c(1:9),
                      y= x[[i]])
    c <- lm(y ~ x, data=df)
    c$coefficients[2]
  })%>%
    map_df(broom::tidy, .id = 'CO_ESCOLA')%>%select(-names)%>%
    rename(!!quo_name(i) := x)
  
 temp<-left_join(temp, slope, by = c("CO_ESCOLA" = "CO_ESCOLA"))
}


slope<-temp
temp<-average%>%select(c(CO_ESCOLA, UF_05))%>%distinct_all()

for (i in features){
  #col<-get(i,average)
  #avg_change_nacional<-analise%>%group_by(CO_ANO)%>%summarise(avg=mean(get(i)))
  #avg_change_nacional<-unique(avg_change_nacional%>%mutate(avg_remain= ((sum(avg)-first(avg))/(n()-1))-first(avg))%>%pull(avg_remain))
  
  avg_change<-average %>% split(.$CO_ESCOLA) %>% map(function(x) {
    first<-first(x[[i]])
    remain<-x[[i]][-1]
    mean(remain)-first
  })%>%
    map_df(broom::tidy, .id = 'CO_ESCOLA')%>%
  rename(!!quo_name(i) := x)
  
  temp<-left_join(temp, avg_change, by = c("CO_ESCOLA" = "CO_ESCOLA"))
}
avg_change<- temp


###correlation map
install.packages("corrplot")                     
library("corrplot")  
corrplot(cor(avg_change%>%select(-c(CO_ESCOLA, UF_05))))
         
    

##OLAP Rules for variables hghlighted in the Two-Way fixed-effects model

build_target <- function(df, var){
  var <- get(var, df)
  df<- df%>%mutate(
    var = ntile(var, 4))
  print(paste0(round(mean(df$var==4)*100, 1), '%', ' upper quartil'))
  return(df$var)
}

for (i in features){
  #col<-get(i,average)
  slope[[i]]<- build_target(slope, i)
  avg_change[[i]]<- build_target(avg_change, i)
}

#for (i in features){
  col<-get(i,slope)
  n_rule =slope%>%filter(TITULACAO>0.54)%>%summarise(n())%>%pull()
  n =slope%>%filter(TITULACAO>0.54 & NU_NOTA_GERAL==4)%>%summarise(n())%>%pull()
  n_target = slope%>%filter(NU_NOTA_GERAL ==4)%>%summarise(n())%>%pull()
  cover = round(n_rule/nrow(slope), 3)
  conf = round(n/n_target, 3)
  lift = round(conf/0.25, 3)
  #print(i)
  cat("\n",i, "\n", cover, "\n", conf, "\n",lift)
#}

#for (i in features){
  col<-get(i,avg_change)
  n_rule =avg_change%>%filter(col==1)%>%summarise(n())%>%pull()
  n =avg_change%>%filter(col==1 & NU_NOTA_GERAL==1)%>%summarise(n())%>%pull()
  n_target = avg_change%>%filter(NU_NOTA_GERAL ==1)%>%summarise(n())%>%pull()
  cover = round(n_rule/nrow(avg_change), 3)
  conf = round(n/n_target, 3)
  lift = round(conf/0.25, 3)
  #print(i)
  cat("\n",i, "\n", cover, "\n", conf, "\n",lift)
#}




average<- fastDummies::dummy_cols(temp, select_columns = "UF_05")
average<- average%>%select(-UF_05)


## Another analysis
build_target <- function(df, var){
  var <- get(var, df)
  df<- df%>%mutate(
    var = ntile(var,4))
  print(paste0(round(mean(df$var==4)*100, 1), '%', ' upper quartil'))
  return(df$var)
}

data<-avg_change%>%select(-c(CO_ESCOLA, UF_05))
data<-clip_tail(data)
pp = preProcess(data, method = c("range"))
data <- predict(pp, data)

#rl<-lm(NU_NOTA_GERAL~., data=data)
#summary(rl)

data$NU_NOTA_GERAL<-build_target(data, 'NU_NOTA_GERAL')
data$NU_NOTA_GERAL<- if_else(data$NU_NOTA_GERAL == 4, 1, 0)
##library(rpart)
#library(rpart.plot)
tree <- rpart(NU_NOTA_GERAL ~., data = data%>%select(-c(RENDA_PERCAPITA, EDU_MAE, EDU_PAI, TITULACAO, IN_FORM_DOCENTE)), method="class",control=rpart.control(maxsplit=2, cp=0.00001))
rpart.plot(tree)

###rules
n_rule =data%>%filter(EDU_PAI>0.66)%>%summarise(n())%>%pull()
n =data%>%filter((EDU_PAI>0.66) & (NU_NOTA_GERAL==1))%>%summarise(n())%>%pull()
n_target = data%>%filter(NU_NOTA_GERAL ==1)%>%summarise(n())%>%pull()
cover = round(n_rule/nrow(data), 3)
conf = round(n/n_target, 3)
lift = round(conf/0.25, 3)
#print(i)
cat(cover, "\n", conf, "\n",lift)
#}


data<-avg_change%>%select(-CO_ESCOLA)
data<- fastDummies::dummy_cols(data, select_columns = "UF_05")
data<- data%>%select(-UF_05)
data$TITULACAO<-build_target(data, 'TITULACAO')
data$TITULACAO<- if_else(data$TITULACAO == 4, 1, 0)

tree <- rpart(TITULACAO ~., data = data%>%select(-NU_NOTA_GERAL))




