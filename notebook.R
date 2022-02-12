# **Importando as bibliotecas utilizadas**


library(readxl)
library(ggplot2)
library(dbplyr)
library(tidyverse)
library(rstatix)
library(Hmisc)
library(ggpubr)
library(corrplot)
library(ggprism)


# **Importando os dados do ENEM**


df = read_xlsx('enem_ab2.xlsx')


# Distribuição das notas

hist(df$NOTA_ENEN, col = "lightblue", main = "", xlab = "Frequência", 
     ylab = "Notas", breaks = 30)
abline(v=mean(df$NOTA_ENEN), col='red',lwd=2)
legend(x='topright',legend=paste('Média = ',signif(mean(df$NOTA_ENEN))), fill='red')


# Distribuição das notas por sexo


notas_cut = cut(df$NOTA_ENEN, breaks = quantile(df$NOTA_ENEN),
                include.lowest = TRUE)

quartis_nota_sexo = table(df$TP_SEXO, notas_cut)

barplot(quartis_nota_sexo, beside = TRUE, xlab = "Notas por quartis", 
        ylab = "Frequência", col = c("lightblue", 5))

legend(x = "topright", legend = c("Feminino", "Masculino"),
       fill = c("lightblue", 5), bty = "n")


# Testes de diferença estatística entre as notas de homens e mulheres para a nota de Redação


## Separando as variáveis


nota_mulher = subset(df$NOTA_ENEN,df$TP_SEXO=='Feminino')
nota_homem = subset(df$NOTA_ENEN,df$TP_SEXO=='Masculino')


## Teste de variação


var.test(nota_homem,nota_mulher)


## Teste t


t_test=t.test(nota_homem,nota_mulher, conf.level = 0.05)
t_test


## Gráfico ilustrando a análise de variância das notas entre os sexos

df_p_val <- data.frame(
  group1 = "Feminino",
  group2 = "Masculino",
  label = signif(t_test$p.value, digits=3),
  y.position = 1000
)

ggboxplot(df, x = "TP_SEXO", y = "NU_NOTA_REDACAO",
          xlab = 'Sexo') +
  add_pvalue(df_p_val)



# Classificação das notas em Mesorregiões


meso = read_xlsx('mun_messoregiao.xlsx')
media_mun=aggregate(NOTA_ENEN~NO_MUNICIPIO_RESIDENCIA,data=df,FUN=mean)
media_mun_meso=merge(media_mun,meso,by.x='NO_MUNICIPIO_RESIDENCIA',by.y='mun')

## Distribuição das notas por mesorregião

nota_cut_m = cut(media_mun_meso$NOTA_ENEN, breaks = quantile(media_mun_meso$NOTA_ENEN),
                 include.lowest = TRUE)

quartis_nota = table(media_mun_meso$mesoregiao, nota_cut_m)

barplot(quartis_nota, beside = TRUE, col = c("lightblue", 5, 'blue'), 
        ylab = "Frequência", xlab = "Notas por quartis")

legend(x = "topleft", 
       legend = c("Agreste", "Sertão", "Leste"), 
       fill = c("lightblue", 5, 'blue'), xpd = TRUE)


# Testes de diferença estatística entre as notas das mesorregiões

## Análise de variância entre as médias das mesorregiões



res.aov=media_mun_meso%>%
  anova_test(NOTA_ENEN ~ mesoregiao)


pwc <- media_mun_meso %>%
  pairwise_t_test(NOTA_ENEN ~ mesoregiao, p.adjust.method = "bonferroni")


## Gráfico ilustrando a análise de variância


pwc <- pwc %>% add_xy_position(x = "mesoregiao")
ggboxplot(media_mun_meso, x = "mesoregiao", y = "NOTA_ENEN",
          xlab = 'Mesorregião') +
  stat_pvalue_manual(pwc, label = "p.adj", tip.length = 0, step.increase = 0.1,) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
    )

# Matriz de correlação entre as notas das 5 disciplinas do ENEM


nota_cn = aggregate(NU_NOTA_CN~NO_MUNICIPIO_RESIDENCIA,data=df,FUN=mean)
notas = merge(nota_cn,aggregate(NU_NOTA_CH~NO_MUNICIPIO_RESIDENCIA,data=df,
                                FUN=mean),by='NO_MUNICIPIO_RESIDENCIA')
notas = merge(notas,aggregate(NU_NOTA_LC~NO_MUNICIPIO_RESIDENCIA,data=df,
                              FUN=mean),by='NO_MUNICIPIO_RESIDENCIA')
notas = merge(notas,aggregate(NU_NOTA_MT~NO_MUNICIPIO_RESIDENCIA,data=df,
                              FUN=mean),by='NO_MUNICIPIO_RESIDENCIA')
notas = merge(notas,aggregate(NU_NOTA_REDACAO~NO_MUNICIPIO_RESIDENCIA,data=df,
                              FUN=mean),by='NO_MUNICIPIO_RESIDENCIA')


rownames(notas) = notas$NO_MUNICIPIO_RESIDENCIA
notas$NO_MUNICIPIO_RESIDENCIA = NULL

corrplot(cor(notas), method = 'number')


## Equação de regressão


cn_mt=select(notas,NU_NOTA_CN,NU_NOTA_MT)
plot(cn_mt,main='Notas Ciências da Natureza e Matematica')

regressao = lm(cn_mt$NU_NOTA_MT~cn_mt$NU_NOTA_CN)

abline(regressao, col='red')
grid()
summary(regressao)

## Teste de hipótese para validar a correlação

shapiro.test(regressao$residuals)
