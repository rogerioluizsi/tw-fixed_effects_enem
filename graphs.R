

school_features = c(
  'IN_LABORATORIO_INFORMATICA',
  'IN_LABORATORIO_CIENCIAS',
  'IN_SALA_ATENDIMENTO_ESPECIAL',
  'IN_BIBLIOTECA',
  'IN_SALA_LEITURA',
  'IN_BANHEIRO',
  'IN_BANHEIRO_PNE',
  'QT_SALAS_UTILIZADAS',
  'QT_EQUIP_TV',
  'QT_EQUIP_DVD',
  'QT_EQUIP_COPIADORA',
  'QT_EQUIP_IMPRESSORA',
  'QT_COMP_ALUNO',
  'IN_BANDA_LARGA',
  'QT_FUNCIONARIOS',
  'IN_ALIMENTACAO',
  'IN_COMUM_MEDIO_MEDIO',
  'IN_COMUM_MEDIO_INTEGRADO',
  'IN_COMUM_MEDIO_NORMAL',
  'IN_SALA_PROFESSOR',
  'IN_COZINHA',
  'IN_EQUIP_PARABOLICA',
  'IN_QUADRA_ESPORTES',
  'IN_ATIV_COMPLEMENTAR',
  'QT_MATRICULAS' 
)

teacher_features = c('TITULACAO', 'IN_FORM_DOCENTE','NU_LICENCIADOS', 
                     'NU_CIENCIA_NATUREZA','NU_CIENCIAS_HUMANAS', 'NU_LINGUAGENS_CODIGOS', 'NU_MATEMATICA', 
                     'NU_ESCOLAS', 'DIVERSIDADE')

student_features =c('RENDA_PERCAPITA',  'EDU_PAI', 'EDU_MAE', 'NU_IDADE')


non_actionable_features = c('TP_COR_RACA_0.0', 'TP_COR_RACA_1.0',
                            'TP_COR_RACA_2.0', 'TP_COR_RACA_3.0', 'TP_COR_RACA_4.0',
                            'TP_COR_RACA_5.0',  'TP_SEXO')

labels = c(EDU_MAE = "Mother's education", EDU_PAI = "Father's education", RENDA_PERCAPITA = "Income (per capita)",
           TP_COR_RACA_1.0 = "White students",TP_COR_RACA_0.0 = "Race non-declared ", IN_BIBLIOTECA = "School library", DIVERSIDADE = "Faculty work overload", 
           IN_ATIV_COMPLEMENTAR = "Complementary activity", IN_EQUIP_PARABOLICA = "Satellite Dish",
           IN_FORM_DOCENTE = "Faculty adequate training", IN_LABORATORIO_CIENCIAS = "Science lab", 
           IN_SALA_ATENDIMENTO_ESPECIAL = "Special attendence room", IN_SALA_LEITURA ="Reading room", 
           NU_ESCOLAS = "Faculty jobs", QT_COMP_ALUNO = "Student's Computer", QT_EQUIP_COPIADORA = "Copy machine",
           QT_FUNCIONARIOS = "Number of employess", TITULACAO = "Faculty education", IN_BANHEIRO="Bathroom",
           IN_BANHEIRO_PNE ="Handicap bathroom",IN_QUADRA_ESPORTES = "Sport's court", 
           QT_EQUIP_DVD = "DVD player", QT_EQUIP_IMPRESSORA = "Printer", 
           TP_COR_RACA_2.0="Black students", TP_COR_RACA_3.0 = "Brown students", 
           TP_COR_RACA_5.0 = "Indigenous students", NU_CIENCIA_NATUREZA = "Natural Science faculty",
           NU_IDADE = "Student's age", QT_EQUIP_TV = "Television", 
           NU_CIENCIAS_HUMANAS = "Humanities faculty", NU_LINGUAGENS_CODIGOS = "Languages faculty", 
           IN_BANDA_LARGA="Fast Internet", NU_LICENCIADOS="Pedagogical Training",QT_MATRICULAS ="Enrollments",
           NU_MATEMATICA = "Math faculty", TP_COR_RACA_4.0="Yellow students", TP_SEXO = "Gender",
           QT_SALAS_UTILIZADAS = "Classrooms", IN_LABORATORIO_INFORMATICA = "Computers lab", IN_ALIMENTACAO = 'School lunch',
           IN_SALA_PROFESSOR = "Faculty room", IN_COZINHA = "School Kitchen", IN_COMUM_MEDIO_MEDIO="Regular schools",
           IN_COMUM_MEDIO_INTEGRADO ="Technical schools (integrated)")
plot1<- result_models_fixed_effects%>%
  filter(tp_escola == "Municipal+Estadual")%>%group_by(term)%>%summarise(n=n(), estimate=estimate, year=year)%>% 
  filter((term!="(Intercept)"))%>%filter(!startsWith(term, "UF_05_"))%>%
  mutate(
    years_mean = ifelse(year < 2013, "2009–2012",
                    ifelse((year > 2012) & (year < 2016),"2013–2015","2016–2018")))%>%
  ggplot(aes(term, estimate))+ geom_boxplot(outlier.shape = NA) +geom_jitter(aes(colour = years_mean)) +
  #scale_colour_gradient(low = "#85a6f2", high = "#06297a")+
  scale_x_discrete(labels=labels) + geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()+theme_publish()+
  theme(axis.text.x = element_text(angle = 0, hjust =1, color = 'gray15'),
        axis.text.y = element_text(angle = 0, hjust =1, color = 'gray15'),
        legend.position="right", 
        legend.title = element_text())+xlab("Features")+ylab(expression(paste("\u03B2-coefficients")))+labs(colour="Start from")+
  theme(panel.grid.major = element_line(color = "gray", linetype = "dotted"),
        panel.grid.minor = element_blank())
ggsave(filename = "fixed_effects_all-variables.png", plot = plot1, dpi = 800,width = 8, height = 6)

plot1




  filter(tp_escola == "Municipal+Estadual")%>%group_by(term)%>%summarise(n=n(), estimate=estimate, year=year)%>% 
  filter(term!="(Intercept)")%>%select(-starts_with("UF_05")) 
  
  
  df_new <- df %>%
    mutate(new_column = ifelse(x == 1, "Condition 1",
                               ifelse(x == 2, "Condition 2", "Condition 3")))
  
fraction = notas%>%group_by(CO_ANO)%>%
    mutate(Undergraduate=sum(NU_GRADUACAO),  Master = sum(NU_MESTRADO), Specialization = sum(NU_ESPECIALIZACAO), Ph.D. = sum(NU_DOUTORADO))%>%
    select(Undergraduate, Specialization, Master, Ph.D.)%>%distinct_all()  

long<-fraction%>%pivot_longer(-CO_ANO, names_to = "degree", values_to = "count")

long <- long %>%
  group_by(CO_ANO) %>%
  mutate(proportion = count / sum(count))

long <- long %>%
       arrange(CO_ANO, degree) %>%
       group_by(degree) %>%
       mutate(growth = (proportion / lag(proportion) - 1) * 100) %>%
       ungroup()
long <- long %>%
  group_by(degree) %>%
  mutate(avg_growth = mean(growth, na.rm = TRUE)) %>%
  ungroup()

long <- long %>%
  mutate(degree_with_growth = paste0(degree, " (A.G. = ", round(avg_growth, 2), "%)"))


degree_order <- c("Undergraduate (A.G. = -2.37%)", "Specialization (A.G. = 2.89%)",
                  "Master (A.G. = 11.54%)", "Ph.D. (A.G. = 13.27%)")




plot1<-ggplot(long, aes(x = as.factor(CO_ANO), y = proportion, fill = degree_with_growth)) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(breaks = degree_order) +
  labs(x = "Years", y = "Fraction of faculty education",fill = "Education:") +
  theme(legend.position = "bottom")+theme_publish()
plot1
ggsave(filename = "education_over_time.png", plot = plot1, dpi = 800)


