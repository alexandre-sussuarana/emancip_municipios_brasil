# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Criação de Municípios no Brasil:
#   Projeto de gráficos baseados na tese de Adilar Antonio Cigolini.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Documento disponível em:
  browseURL('https://repositorio.ufsc.br/xmlui/handle/123456789/92531')
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


# PACOTES ---------------------------------------------------------------------

library(dplyr)
library(gghighlight)
library(ggplot2)
library(ggthemes)
library(rvest)
library(tabulizer)
library(tidyr)
  
# DOWNLOAD DO DOCUMENTO PDF ---------------------------------------------------

  ## arquivo temporário
temp <- tempfile(pattern = 'tese_', fileext = '.pdf')

  ## download do pdf
download.file(
  url = paste0('https://repositorio.ufsc.br/xmlui/bitstream/handle/123456789/',
               '92531/268885.pdf?sequence=1&isAllowed=y'),
  destfile = temp,
  method = 'libcurl',
  mode = 'wb'
  )

  ## abrir o documento para eventuais consultas
temp |> shell.exec()

# OBTENCAO DAS TABELAS --------------------------------------------------------

    # Páginas onde se encontram as tabelas
    # tabelas 5 a 9.
paginas <- c(122, 149, 158, 164, 172, 181)

tb <- extract_tables(
  file = temp,
  pages = paginas, 
  encoding = 'UTF-8',
  output = 'data.frame',
  method = 'stream')
  ## Abrir o arquivo list das tabelas
View(tb)
rm(paginas)

  # tratamento da 1ª e 3ª tabela: celulas aglutinadas
    # inclusao das colunas que faltam:
tb[[1]] <- cbind(tb[[1]][,1], 'x2' = NA, 'x3' = NA, tb[[1]][,2:3])
tb[[3]] <- cbind(tb[[3]][,1], 'x2' = NA, 'x3' = NA, tb[[3]][,2:3])
    # transferencia dos numeros para a segunda coluna
tb[[1]][,2] <- gsub('.*\\D ', '', tb[[1]][, 1])
tb[[3]][,2] <- gsub('.*\\D ', '', tb[[3]][, 1])
    # transferencia dos numeros para a terceira coluna
tb[[1]][,3] <- gsub('.*\\s', '', tb[[1]][, 2])
tb[[3]][,3] <- gsub('.*\\s', '', tb[[3]][, 2])
    # exclusao de numeros da terceira coluna na segunda coluna
tb[[1]][,2] <- gsub('\\s.*', '', tb[[1]][, 2])
tb[[3]][,2] <- gsub('\\s.*', '', tb[[3]][, 2])
    # exclusao de numeros na primeira coluna
tb[[1]][,1] <- gsub('\\s\\d.*', '', tb[[1]][, 1])
tb[[3]][,1] <- gsub('\\s\\d.*', '', tb[[3]][, 1])
    # realinhamento das informações do RN na tabela 3.
tb[[3]][19, ] <- c('Rio Grande do Norte', t(tb[[3]][20,-1]))
    # exclusao de linhas desnecessarias
tb[[3]] <- tb[[3]][-c(20:21),]
    # retirada de linhas nao lidas como header
linhas <- c(3,4,3,4,4,4)
for (i in 1:length(linhas)) {
  tb[[i]] = tb[[i]][-c(1:linhas[i]), ]
  rm(i)
}
rm(linhas)
    # ordenação da tabela 1.
tb[[1]] <- cbind(tb[[1]][, 1:3], tb[[1]][, c(5,4)])


  # ATENÇÃO:
  # obtencao da tabela 4, referente ao periodo colonial.
    # Obs.: o extract_tables nao detecta as informacoes desta pagina, entao fi-
    # zemos uso do extract_areas. Assim que o comando abaixo for rodado, deve
    # abrir uma pagina no Viwer do seu RStudio. Selecione com o mouse a area
    # onde se encontra a tabela desejada e em seguida clique em 'Done'.

tb4 <- extract_areas(
  file = temp,
  pages = 102
  )

    # transformando em data.frame
tb4 <- tb4[[1]] |> data.frame()
    # aglutinando informacoes das colunas 1 e 2.
tb4$X2[-18] <- tb4$X1[-18]
    # retirada da coluna 1
tb4 <- tb4[, -1]
    # nomeando corretamente as colunas
names(tb4) <- c('provincia', 'municipios')
    # transformacao de character em numeric
tb4$municipios <- tb4$municipios |> as.numeric()
    # Exibir a tabela
View(tb4)

# MONTAGEM E TRATAMENTO DA TABELA UNICA ---------------------------------------
# inclusao de informacoes sobre os periodos de cada tabela
    # vetor com os períodos
periodos <- c('1822 - 1889',
              '1890 - 1930',
              '1931 - 1945',
              '1946 - 1964',
              '1965 - 1985',
              '1986 - 2008')
# nomes das colunas
nomes <- c('periodo',
           'uf',
           'mun_existentes',
           'mun_criados',
           'total',
           'variacao')
# objeto que acondiciona a tabela unificada
d <- NULL
    # loop de montagem da tabela unificada
for (i in 1:length(periodos)) {
  d0 = cbind(periodo = periodos[i],
             tb[[i]])
  names(d0)= nomes
  d = rbind(d, d0)
  rm(i, d0)
  
}

  # transformacao das colunas numericas (character -> numeric)
d$mun_existentes <- d$mun_existentes |> as.numeric()
d$mun_criados <- d$mun_criados |> as.numeric()
d$total <- d$total |> as.numeric()
d$variacao <- gsub('%', '', gsub(',', '.', d$variacao)) # substituicao de ',' por '.'.
d$variacao <- d$variacao |> as.numeric()
    # obs.: a transformação das colunas mun_existentes e total acusam NA's por
    # conter caracteres que indicam a falta de dados (--).

# padronizacao dos títulos de UF
    # retirada de acentos
d$uf <- d$uf |> stringi::stri_trans_general(id = 'Latin - ASCII')
    # criacao das colunas regiao e sigla_uf
        # tabela de siglas e regioes
t1 <- read_html(
  x = paste0('https://www.significados.com.br/siglas-dos-estados-do-brasil-',
             'e-suas-capitais/'),
  encoding = 'UTF-8') |>
  html_table(header = T) |> as.data.frame()
t1$ESTADO <- stringi::stri_trans_general(t1$ESTADO, id = 'Latin - ASCII')
        # loop de preenchimento das colunas
d$sigla_uf <- NA
d$regiao <- NA
for (i in 1:nrow(t1)) {
  d$sigla_uf[
    grep(t1$ESTADO[i], d$uf, ignore.case = T)
    ] = t1$SIGLA[i]
  d$regiao[
    grep(t1$ESTADO[i], d$uf, ignore.case = T)
  ] = t1$REGIÃO[i]
  rm(i)
}


  # Salvar a tabela unica (retire as # para utilizar)
# saveRDS(object = d, file = 'tb_mun_br.RDS')
  # Salvar o ambiente
# save.image(
  # file = paste0('ambiente_',
  #               format(Sys.time(), '%Y%b%d'),
  #               '.RData'))


# GRAFICOS --------------------------------------------------------------------

## ├─ grafico 1: total de municipios por provincia no periodo colonial --------

  # ordem das colunas a serem plotadas.
col.ord <- (nrow(tb4)-1):1

tb4|> filter(provincia != 'Total') |>
  ggplot(aes(
    x = col.ord,
    y = municipios,
    group = provincia
    ))+
  scale_x_discrete(
    limits = col.ord,
    labels = tb4$provincia[-nrow(tb4)]
  )+
  geom_col(fill = 'green3')+
  geom_text(aes(
    y = municipios,
    label = municipios
    ),
    nudge_y = -0.5)+
  labs(
    title = 'Quantidade de municípios por província\nno período colonial',
    subtitle = 'estatísticas do fim do período',
    caption = paste('Elab.: @ahras_econ, fonte: Adilar Antônio Cigoni (2009)',
                    '\nLinguagem R em RStudio IDE. 2022.')
  )+
  coord_flip()+
  theme_base()+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 18),
    plot.title.position = 'plot'
  )

## ├─ grafico 2: total de municipios ------------------------------------------

d |>
  filter(uf == 'Total') |>
  ggplot(
    aes(x = gsub(' - ', ' a\n', periodo),
        y = total,
        group = uf)
    )+
  scale_y_continuous(labels = ~./1000,
                     position = 'right')+
  geom_line(linetype = 'dotted',
            size = 1)+
  geom_point(aes(colour = 'total'),
             size = 10)+
  geom_col(aes(y = mun_criados,
               fill = 'municipios\nemancipados\nno período'),
           width = 0.25)+
  geom_text(aes(y = total,
                label = total))+
  labs(
    title = 'Quantidade de Municípios No Brasil',
    subtitle = 'soma dos então existentes e os emancipados por periodo',
    caption = paste('Elab.: @ahras_econ, fonte: Adilar Antônio Cigoni (2009)',
                    '\nLinguagem R em RStudio IDE. 2022.'),
    x = '',
    y = 'total de\nmunicipios\nno periodo\n(x1000)')+
  scale_colour_manual(values = c('green'))+
  scale_fill_manual(values = c('darkgreen'))+
  theme_clean()+
  theme(
    axis.title.y.right = element_text(
      angle = 0,
      vjust = 1,
      hjust = 0,
      size = 10),
    legend.position = c(0.25,0.9),
    legend.title = element_blank(),
    legend.background = element_blank(),
#    legend.spacing = unit(-3, 'mm'),
    legend.box = 'horizontal',
    plot.caption.position = 'plot'
  )

## ├─ grafico 3: total de municipios por regiao -------------------------------

d |>
  filter(uf != 'Total') |>
  group_by(periodo, regiao) |> 
  summarise(valor = sum(total)) |>
  ggplot(aes(
    x = gsub(' - ', ' a\n', periodo),
    y = valor, group = regiao, colour = regiao))+
  scale_y_continuous(position = 'right')+
  geom_line(size = 2)+
  gghighlight()+
  labs(
    x = '',
    y = '',
    title = 'Quantidade de Municípios por Região Brasileira',
    subtitle = 'Soma dos então existentes e os criados no período.',
    caption = paste('Elab.: @ahras_econ, fonte: Adilar Antônio Cigoni (2009)',
                    '\nLinguagem R em RStudio IDE. 2022.')
  )+
  theme_fivethirtyeight()

## ├─ grafico 4: variacao de emancipação por regiao ---------------------------

d |> filter(uf != "Total") |>
  group_by(periodo, regiao) |>
  summarise(valor = sum(mun_criados)) |>
  arrange(desc(periodo)) |> 
  ggplot(aes(x = periodo, y = valor, group = regiao, fill = regiao))+
  scale_y_continuous(labels = scales::percent,
                     sec.axis = sec_axis(~., labels = scales::percent))+
  geom_col(position = position_fill(reverse = T), width = 1, colour = 'black')+
  labs(
    x = '',
    y = '',
    title = 'Composição da Emancipação de Municípios por Região Brasileira',
    subtitle = paste0('participação na criação de novos',
                      ' municípios, por região e período.'),
    caption = paste('Elab.: @ahras_econ, fonte: Adilar Antônio Cigoni (2009)',
                    '\nLinguagem R em RStudio IDE. 2022.')
  )+
  theme_minimal()+
  coord_flip()+
  theme(
    legend.position = 'top',
    legend.title = element_blank(),
    plot.title.position = 'plot'
  )

## ├─ grafico 5: ranking de emancipação por uf --------------------------------

d |>
  filter(uf != "Total") |> 
  arrange(desc(uf)) |>
  ggplot(aes(x = uf, y = mun_criados, group = uf, fill = periodo))+
  geom_col(position = position_stack())+
  theme(
    legend.position = 'top',
    legend.title = element_blank()
  )+
  coord_flip()
