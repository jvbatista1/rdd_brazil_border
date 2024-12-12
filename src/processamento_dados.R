dropbox <- "c:/Users/victor/dropbox/DISSERTACAO"
library(tidyverse)

# Base do Sistema de Informações sobre Mortalidade (SIM)
library(basedosdados)
query <- bdplyr("basedosdados.br_ms_sim.municipio_causa")
sim <- bd_collect(query)

#### Analisando mortes violentas em 2019 #####
# Vetor de prefixos para as causas violentas
prefixos <- c("W32", "W33", "W34", "X85", "X86", "X87", "X88", "X89", "X90", 
              "X91", "X92", "X93", "X94", "X95", "X96", "X97", "X98", "X99", 
              "Y00", "Y01", "Y02", "Y03", "Y04", "Y05", "Y06", "Y07", "Y08", 
              "Y09", "Y10", "Y11", "Y17", "Y18", "Y19", "Y20", "Y21", "Y22", 
              "Y23", "Y24", "Y25", "Y26", "Y27", "Y28", "Y29", "Y30", "Y31", 
              "Y32", "Y33", "Y34", "Y35", "Y87", "Y89", "Y12", "Y13", "Y14", 
              "Y15", "Y16")

library(dplyr)
library(stringr)
# Filtrando a tabela
sim <- sim %>%
  filter(str_detect(causa_basica, paste0("^", prefixos, collapse = "|"))) |> 
  filter(ano == 2019) |> 
  count(id_municipio, name = "mortes_violentas")

df <- df |> 
  left_join(sim, by="id_municipio")

#Link base ministério segurança publica
#https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/bancovde-2019.xlsx/@@download/file
library(readxl)
banco <- read_xlsx(file.path(dropbox, "BancoVDE 2019.xlsx"), col_types = c("text", "text", "text", "date", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "numeric"))

banco <- banco |> 
  rename("nome" = "municipio",
         "sigla_uf" = "uf")

#### CRIAR BASES SEPARADAS PARA CADA EVENTO ####
# Vamos criar uma lista com as diferentes categorias de eventos
categorias_evento <- banco |> 
  distinct(evento) |> 
  pull()

# Função para salvar cada categoria em um CSV
salvar_csv <- function(evento) {
  banco|> 
    filter(evento == !!evento) |>  
    write.csv(file = paste0("evento_", evento, ".csv"), row.names = FALSE)
}

# Aplicar a função em cada categoria
library(purrr)
walk(categorias_evento, salvar_csv)
rm(banco)

#VARIÁVEIS COM abrangencia estadual
cocaina_table <- read_csv(file.path(dropbox, "evento_Apreensão de Cocaína.csv")) |> 
  count(sigla_uf, nome, wt=total_peso, name = "cocaina")
maconha_table <- read_csv(file.path(dropbox, "evento_Apreensão de Maconha.csv")) |> 
  count(sigla_uf, nome, wt=total_peso, name = "maconha")
apreensao_arma <- read_csv(file.path(dropbox, "evento_Arma de Fogo Apreendida.csv")) |> 
  count(sigla_uf, nome, wt=total, name = "armas") 
furto_veiculo <- read_csv(file.path(dropbox, "evento_Furto de veículo.csv")) |> 
  count(sigla_uf, nome, wt=total, name = "furto_vei")
roubo_banco_table <- read_csv(file.path(dropbox, "evento_Roubo a instituição financeira.csv")) |> 
  count(sigla_uf, nome, wt=total, name = "rou_banco")
roubo_carga_table <- read_csv(file.path(dropbox, "evento_Roubo de carga.csv")) |> 
  count(sigla_uf, nome, wt=total, name = "rou_carga")
roubo_veículo_table <- read_csv(file.path(dropbox, "evento_Roubo de veículo.csv")) |> 
  count(sigla_uf, nome, wt=total, name = "rou_vei")
trafico_table <- read_csv(file.path(dropbox, "evento_Tráfico de drogas.csv")) |> 
  count(sigla_uf, nome, wt=total, name = "trafico")

#VARIÁVEIS COM abrangencia municipal
feminicidio_table <- read_csv(file.path(dropbox, "evento_Feminicídio.csv")) |> 
  count(sigla_uf, nome, wt=total_vitimas, name = "feminicidio")
hom_doloso_table <- read_csv(file.path(dropbox, "evento_Homicídio doloso.csv")) |> 
  count(sigla_uf, nome, wt=total_vitimas, name = "hom_doloso")
lesao_table <- read_csv(file.path(dropbox, "evento_Lesão corporal seguida de morte.csv")) |> 
  count(sigla_uf, nome, wt=total_vitimas, name = "lesao")
mandado_table <- read_csv(file.path(dropbox, "evento_Mandado de prisão cumprido.csv")) |> 
  count(sigla_uf, nome, wt=total, name = "mandado")
transito_table <- read_csv(file.path(dropbox, "evento_Morte no trânsito ou em decorrência dele (exceto homicídio doloso).csv")) |> 
  count(sigla_uf, nome, wt=total_vitimas, name = "transito")
esclarecer_table <- read_csv(file.path(dropbox, "evento_Mortes a esclarecer (sem indício de crime).csv")) |> 
  count(sigla_uf, nome, wt=total_vitimas, name = "esclarecer")
latrocinio_table <- read_csv(file.path(dropbox, "evento_Roubo seguido de morte (latrocínio).csv")) |> 
  count(sigla_uf, nome, wt=total_vitimas, name = "latrocinio")
tentativa_homicidio_table <- read_csv(file.path(dropbox, "evento_Tentativa de homicídio.csv")) |> 
  count(sigla_uf, nome, wt=total_vitimas, name = "tentativa_hom")

#tabela final
municipal_table <- feminicidio_table |> 
  left_join(hom_doloso_table, join_by(sigla_uf, nome)) |> 
  left_join(lesao_table, join_by(sigla_uf, nome)) |> 
  left_join(mandado_table, join_by(sigla_uf, nome)) |> 
  left_join(transito_table, join_by(sigla_uf, nome)) |> 
  left_join(esclarecer_table, join_by(sigla_uf, nome)) |> 
  left_join(latrocinio_table, join_by(sigla_uf, nome)) |> 
  left_join(tentativa_homicidio_table, join_by(sigla_uf, nome))

rm(hom_doloso_table, lesao_table, mandado_table, transito_table, esclarecer_table, latrocinio_table, tentativa_homicidio_table)

estadual_table <- cocaina_table |> 
  left_join(maconha_table, join_by(sigla_uf, nome)) |> 
  left_join(apreensao_arma, join_by(sigla_uf, nome)) |> 
  left_join(furto_veiculo, join_by(sigla_uf, nome)) |> 
  left_join(roubo_banco_table, join_by(sigla_uf, nome)) |> 
  left_join(roubo_carga_table, join_by(sigla_uf, nome)) |> 
  left_join(roubo_veículo_table, join_by(sigla_uf, nome)) |> 
  left_join(trafico_table, join_by(sigla_uf, nome))

rm(maconha_table, apreensao_arma, furto_veiculo, roubo_banco_table, roubo_carga_table, roubo_veículo_table, trafico_table)

#### IMPORTANDO BASE PARA INSERIR OS CÓDIGOS IBGE ####
library("basedosdados")
# Defina o seu projeto no Google Cloud
set_billing_id("rapid-pact-400813")
# Para carregar o dado direto no R
query <- bdplyr("br_bd_diretorios_brasil.municipio")
codigos <- bd_collect(query)

# Transformar os nomes dos municípios na tabela 'codigos' para maiúsculas
codigos <- codigos %>%
  mutate(nome_upper = toupper(nome))

# Unir as tabelas com base no nome do município
municipal_table <- municipal_table %>%
  left_join(codigos, by = c("nome" = "nome_upper", "sigla_uf"))

municipal_table <- municipal_table |> 
  select(-nome, -(id_municipio_6:id_municipio_bcb), -(capital_uf:centroide)) |> 
  rename("nome"="nome.y") |> 
  relocate(id_municipio:nome, sigla_uf)

municipal_table <- municipal_table |> 
  left_join(sim, join_by(id_municipio))

#### POPULACAO ####
# Calcular em função da população
query <- bdplyr("br_ibge_populacao.municipio")
populacao <- bd_collect(query)

municipal_table <- municipal_table |> 
  left_join(populacao |>
              filter(ano == 2019) |> 
              select(id_municipio:populacao), join_by(id_municipio)) |> 
  mutate(mortes_violentas = (mortes_violentas/populacao)*100000,
         feminicidio = (feminicidio/populacao)*100000,
         hom_doloso = (hom_doloso/populacao)*100000,
         lesao = (lesao/populacao)*100000,
         mandado = (mandado/populacao)*100000,
         transito = (transito/populacao)*100000,
         esclarecer = (esclarecer/populacao)*100000, 
         latrocinio = (latrocinio/populacao)*100000,
         tentativa_hom = (tentativa_hom/populacao)*100000) |> 
  relocate(populacao, .before=feminicidio)

homicidios <- read_excel(file.path(dropbox, "Base_homicidios.xlsx"))

homicidios <- homicidios |> 
  separate(Município, c("id_municipio_6", "nome_upper"), sep = 7) |> 
  separate(id_municipio_6, c("id_municipio_6", NA), sep = -1) |> 
  rename("taxa_analfabetismo" = "Taxa_de_analfabetismo",
         "taxa_desemprego" = "Taxa_de_desemprego_16a_e+",
         "gini" = "Gini",
         "pibpc" = "PIB_per_capita",
         "taxa_renda_pobre" = "%_população_com_renda_<_1/4_SM",
         "taxa_trab_infantil" = "Taxa_de_trabalho_infantil",
         "taxa_homens_jovens" = "Porcentagem_Homens_Jovens",
         "valor_2010" = "valor-2010",
         "valor_2011" = "valor-2011",
         "valor_2012" = "valor-2012",
         "valor_2013" = "valor-2013",
         "valor_2014" = "valor-2014",
         "valor_2015" = "valor-2015",
         "valor_2016" = "valor-2016",
         "valor_2017" = "valor-2017",
         "valor_2018" = "valor-2018",
         "valor_2019" = "valor-2019"
         ) |> 
  left_join(codigos |> select(id_municipio, nome, id_municipio_6), join_by(id_municipio_6)) |> 
  relocate(id_municipio, nome) |> 
  select(-id_municipio_6, -nome_upper)

municipal_table <- municipal_table |> 
  left_join(homicidios)

#salvar
write_rds(municipal_table, file.path(dropbox, "municipal.rds"))
write_csv(estadual_table, file.path(dropbox,"estadual.csv"))
