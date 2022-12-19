---
title: "NTNB-principal_2045"
output: github_document
Autor: Allan Arthur A. Oliveira
Contato: @Econo.Arthur
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Análise do preço do título NTNB-principal até 19-12-2022

##### Carregando pacotes

```{r}
library(readr)
library(dplyr)
library(OpenImageR)
library(ggplot2)
library(plotly)
```
##### Carregando dados
fonte: <https://cdn.tesouro.gov.br/sistemas-internos/apex/producao/sistemas/sistd/2022/NTN-B_Principal_2022.xls>

```{r}
ntnb_principal <- readxl::read_excel(
  col_names = TRUE, skip = 1,
  "D:/ANALISE_MACRO/Repo/Insight_basic_V1/NTN-B_Principal_2022.xls", 
  sheet = "NTN-B Princ 150545",
  col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
ntnb_principal
ntnb_principal[,1] <- as.data.frame(as.Date(ntnb_principal$Dia, "%d/%m/%Y"))
#ntnb_principal
#class(ntnb_principal)
#summary(ntnb_principal)
#typeof(ntnb_principal)
```

##### Input dos dados da aquisição do título ipca + 2045

```{r}
# inputs
v_apl = 205.20
iA = 0.0596
iD = ((1+iA)^(1/360))-1
valortt_dc = 1080.01
qtd_tt <- 1/valortt_dc
```

##### Tratamento dos dados

```{r}
# Tratamento o nome das variaveis
ntnb_principal <- janitor::clean_names(ntnb_principal)
qtd_adq <- qtd_tt * v_apl
# nova variavel - variação do valor conforme preço de venda
var_vlortt <- qtd_adq * ntnb_principal$pu_venda_manha
ntnb_principal[,"preco_venda_variação_preço"] <- var_vlortt 
# nova variavel - variação do valor conforme preço de compra
var_valortt_c <- qtd_adq * ntnb_principal$pu_compra_manha
ntnb_principal[,"preco_compra_variação_preço"] <- var_valortt_c 
# Conversão da taxa de anual para diária
tx_dd <- ((1+ntnb_principal$taxa_venda_manha)^(1/360))-1
# nova variavel - variação do valor conforme a taxa de venda 
vfr_ivar <- ((1+tx_dd)^(0:240)) * v_apl 
# convertendo para DF e renomeando a variavel
vfr_ivar <- as.data.frame(vfr_ivar)
ntnb_principal[,"var_valor_conforme_taxa_titulo"] <- vfr_ivar
# convertendo Taxa anual p/ diária - taxa fixa
242-214
vfr_ifx <- as.data.frame(rep(0, 214))
vfr_ifx[, "valores"] <- as.data.frame(rep(0, 214))
df_prov <- as.data.frame((1+iD)^(0:27) * v_apl)
vfr_ifx <- bind_rows(vfr_ifx, df_prov)
vfr_ifx <- vfr_ifx[, -1]
vfr_ifx[, "valor_teorico_taxa_contratada"] <- as.data.frame(vfr_ifx$`(1 + iD)^(0:27) * v_apl`)
vfr_ifx <- vfr_ifx[, -1]
vfr_ifx
# convertendo para DF e renomeando a variavel
ntnb_principal[,"valor_teorico_taxa_contratada"] <- vfr_ifx$valor_teorico_taxa_contratada
##### Plotando o gráfico
```{r}
colnames(ntnb_principal)
# Criando o Plot
p1 <- ggplot(ntnb_principal, aes(x = dia, y = preco_venda_variação_preço,
                                 col = "Valor à Taxa Contratada"))+
  geom_line(aes(y = preco_venda_variação_preço, col = "Preço à marcação de
                \nmercado conforme preço 
                \nde venda diário\n"))+
  geom_line(aes(y = preco_compra_variação_preço, col = "Preço à marcação de
                \nmercado conforme preço 
                \nde compra diário\n"))+
  geom_line(aes(y = var_valor_conforme_taxa_titulo, col = "Preço à Taxa Real Diária\n"))+
  geom_line(aes(y = valor_teorico_taxa_contratada, col = "Valor Futuro da Apl no tempo\n"))+
  labs(title = "Comparação entre os Preços do IPCA+2045 (5,49%) no perído de 2022",
       subtitle = "Preço à Taxa Real x Preço à Taxa Teórica x Preço do Título à PU de Venda \n(VP = R$ 205,00" ,
       x = "Período", y = "Valor em R$", color = "Preço teórico vs Preço Real")+
  ylab("Valor")+
  xlab("Periodo")+
  scale_y_continuous(n.breaks = 10, expand = expansion(add = .6))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = expansion(mult = .05))+
  coord_cartesian(ylim = c(180, 235))+ 
  theme_bw()
# Plotando o gráfico interativo
ggplotly(p1)
```
