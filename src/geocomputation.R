dropbox <- "c:/Users/victor/dropbox/DISSERTACAO"

#### Meu exemplo
library(sf)
library(dplyr)
library(ggplot2)

fronteira <- read_sf(file.path(dropbox,"Fronteira/Faixa_de_Fronteira_por_UF_2022.shp")) %>%
  st_transform("WGS84") |> 
  rename("id_uf" = "CD_UF",
         "nome_uf" = "NM_UF",
         'sigla_uf' = "SIGLA_UF",
         "nome_regiao" = "NM_REGIAO",
         "area_uf" = "AREA_KM2",
         "area_integrada" = "AREA_INT",
         "porcentagem_integrada" = "PORC_INT")

t <- fronteira
st_geometry(t) <- NULL

t |> 
  select(nome_uf, porcentagem_integrada) |> 
  arrange(desc(porcentagem_integrada)) |> 
  kableExtra::kable("latex")

#Uniformiza a faixa de fronteira como uma única região
linha_fronteira <- fronteira %>%
  mutate(pais = "BR") %>% 
  group_by(pais) %>% 
  summarise()

#Carrega o sf dos municípios da faixa de fronteira
municipios_fronteira <- read_sf(file.path(dropbox,"Municipios_Fronteira/Municipios_Faixa_Fronteira_2022.shp")) %>%
  st_transform("WGS84") |> 
  rename("id_municipio" = "CD_MUN",
         "nome" = "NM_MUN",
         "id_uf" = "CD_UF",
         "nome_uf" = "NM_UF",
         'sigla_uf' = "SIGLA_UF",
         "nome_regiao" = "NM_REGIAO",
         "area_municipio" = "AREA_TOT",
         "area_integrada" = "AREA_INT",
         "porcentagem_integrada" = "PORC_INT") |> 
  mutate(gemea = !(is.na(CID_GEMEA)), .keep = "unused", .after = "porcentagem_integrada")

#Carrega sf dos municípios brasileiros
library(geobr)
municipios <- read_municipality(year=2020, showProgress = T, simplified = T) |> 
  st_transform("WGS84") |> 
  rename("id_municipio" = "code_muni",
         "nome" = "name_muni",
         "id_uf" = "code_state",
         "nome_uf" = "name_state",
         'sigla_uf' = "abbrev_state",
         "nome_regiao" = "name_region",
         "id_regiao" = "code_region")|> 
  mutate(id_municipio = as.character(id_municipio))

# Estabelece a nova proposta de faixa de fronteira
linha_fronteira_300km <- st_buffer(linha_fronteira, dist = 150000)

# Verifica municípios que passam a pertencer à região
# Adiciona variável de intercessão
municipios$inter <- st_intersects(municipios, linha_fronteira_300km, sparse = F)

# Adiciona variável de pertencimento à fronteira original
municipios <- municipios %>%
  mutate(fronteira = ifelse(id_municipio %in% municipios_fronteira$id_municipio, 1, 0))

# Cria tratamento e controle
df <- municipios |>
  #filtra os municípios na nova faixa
  filter(inter == T) |> 
  # cria o grupo de tratamento e controle
  mutate(treated = ifelse(id_municipio %in% municipios_fronteira$id_municipio, 1, 0),
         groups = ifelse(treated == 1, "treatment", "control"),
         # cria os arcos
         arcos = case_when(sigla_uf %in% c("AP", "PA", "AM", "AC", "RR") ~ "Arco Norte",
                           sigla_uf %in% c("RO", "MS", "MT", "SP") ~ "Arco Central",
                           sigla_uf %in% c("PR", "SC", "RS") ~ "Arco Sul")) |> 
  # exclui a variável classificatória. as recém criadas a substituem
  dplyr::select(-inter, -fronteira, -treated)
  


# prepara a tabela da fronteira para mergir com a df principal (municipios)
t <- municipios_fronteira |> 
  # seleciona colunas de interesse
  select("id_municipio", "area_municipio", "area_integrada", "porcentagem_integrada", "gemea")

# remove o componente gráfico
st_geometry(t) <- NULL

# realiza o join
df <- dplyr::left_join(df, t, by = "id_municipio")

rm(t)

#########################################
#Carrega o sf dos países da América do Sul
america <- st_read(file.path(dropbox,"America/South_America.shp")) %>% 
  st_transform("WGS84")

brasil <- america %>%
  filter(COUNTRY == "Brazil")

# prepara para juntar demais países da américa do sul na base de municípios
# Remove regiões sem fronteira com o br
america <- america %>%
  filter(COUNTRY %in% c("French Guiana (France)", "Suriname", "Guyana", "Venezuela", 
                        "Colombia", "Peru", "Bolivia", "Paraguay", "Argentina", "Uruguay"))
  
# verifica interseções
a <- st_is_within_distance(df, america, dist= 5000, sparse = FALSE)

# renomeia colunas e cria variáveis dummy
a <- as.data.frame(a) |> 
  rename("Argentina" = "V1",
         "Bolivia" = "V2",
         "Colombia" = "V3",
         "French_Guiana" = "V4",
         "Guyana" = "V5",
         "Suriname" = "V6",
         "Paraguay" = "V7",
         "Peru" = "V8",
         "Uruguay" = "V9",
         "Venezuela" = "V10") |> 
  mutate(Argentina = ifelse(Argentina == T, 1, 0),
         Bolivia = ifelse(Bolivia == T, 1, 0),
         Colombia = ifelse(Colombia == T, 1, 0),
         French_Guiana = ifelse(French_Guiana == T, 1, 0),
         Guyana = ifelse(Guyana == T, 1, 0),
         Suriname = ifelse(Suriname == T, 1, 0),
         Paraguay = ifelse(Paraguay == T, 1, 0),
         Peru = ifelse(Peru == T, 1, 0),
         Uruguay = ifelse(Uruguay == T, 1, 0),
         Venezuela = ifelse(Venezuela == T, 1, 0))

df <- cbind(df, a)
rm(a)

library(units)
sede_municipios <- read_municipal_seat(year=2010, showProgress = T) %>%
  st_transform("WGS84") |> 
  rename("id_municipio" = "code_muni",
         "nome" = "name_muni",
         "id_uf" = "code_state",
         'sigla_uf' = "abbrev_state",
         "nome_regiao" = "name_region",
         "id_regiao" = "code_region",
         "ano" = "year") |> 
  mutate(id_municipio = as.character(id_municipio))

# Extrai latitude e longitude
sede_municipios <- sede_municipios %>%
  mutate(longitude = st_coordinates(.)[, 1],
         latitude = st_coordinates(.)[, 2])

t <- select(df, id_municipio)
st_geometry(t) <- NULL

sede_municipios <- sede_municipios |> 
  semi_join(t, by="id_municipio")

rm(t)

# Definir a linha da fronteira terrestre do Brasil com base nesse buffer
linhas_brasil <- st_boundary(brasil)
linhas_america <- st_boundary(america)
fronteira_terrestre <- st_intersection(linhas_brasil, linhas_america)

rm(linhas_brasil, linhas_america)

# Converter a fronteira terrestre em uma linha (apenas a borda do Brasil que faz fronteira terrestre)
fronteira_terrestre <- fronteira_terrestre|> 
  group_by(COUNTRY) |> 
  summarise()

#readr::write_rds(fronteira_terrestre, file.path(dropbox, "fronteira_terrestre.rds"))

# Calcular a distância entre as sedes municipais e a linha da fronteira terrestre
distancias_terrestres <- st_distance(sede_municipios, fronteira_terrestre)

# Adicionar a distância calculada ao dataframe de sede_municipio
sede_municipios$distancia_fronteira_terrestre <- as.numeric(distancias_terrestres)
rm(distancias_terrestres)

#### limite da faixa de fronteira. Definimos como fronteira interior o limite integral dos municípios pertencentes à FF
simpler_map <- municipios |>
  mutate(treated = ifelse(id_municipio %in% municipios_fronteira$id_municipio, 1, 0)) |> 
  rmapshaper::ms_simplify( keep = 0.01, keep_shapes = TRUE)

tratamento <- simpler_map |> 
  filter(treated == 1) |> 
  st_union() |> 
  st_boundary()

controle <- simpler_map |> 
  filter(treated == 0) |>
  filter(sigla_uf %in% c("AP", "PA", "AM", "AC", "RR","RO", "MS", "MT","PR", "SC", "RS","SP")) |> 
  st_union() |> 
  st_boundary()
  
tratamento <- st_buffer(tratamento, dist = 1)

fronteira_interior <- st_intersection(tratamento, controle)

#readr::write_rds(fronteira_interior, file.path(dropbox, "fronteira_interior.rds"))

rm(simpler_map, tratamento, controle)

# Calcular a distância entre as sedes municipais e a linha da fronteira terrestre
distancias_interior <- st_distance(sede_municipios, fronteira_interior)

# Adicionar a distância calculada ao dataframe de sede_municipio
sede_municipios$distancia_fronteira_interior <- as.numeric(distancias_interior)

t <- sede_municipios |> 
  select(id_municipio, distancia_fronteira_terrestre, distancia_fronteira_interior, latitude, longitude)
st_geometry(t) <- NULL

df <- df |> 
  left_join(t, by = "id_municipio") |> 
  #torna as distâncias da faixa de fronteira original negativas
  mutate(distancia_fronteira_interior = ifelse(groups == "control", 
                                                -distancia_fronteira_interior, 
                                                distancia_fronteira_interior))

rm(t)

##### CRIAÇÃO DAS LABELS #####
t <- df[, c("Argentina", "Bolivia", "Colombia", "French_Guiana","Guyana", "Suriname", "Paraguay", "Peru", "Uruguay", "Venezuela")]
st_geometry(t) <- NULL

df <- df |> 
  mutate(m_ff = groups == "treatment", 
         m_fronteira = rowSums(t) > 0,
         m_sedeff = distancia_fronteira_terrestre < 150000,
         m_ff = if_else(is.na(m_sedeff), FALSE, m_ff),
         m_sedeff = if_else(m_ff == FALSE, FALSE, m_sedeff),
         label = paste0(as.integer(m_ff), as.integer(m_fronteira), as.integer(m_sedeff)))
rm(t)

ggplot()+
  geom_sf(data=df, aes(fill = label), color = "black")+
  geom_sf(data = fronteira_interior, color = "red")+
  labs(fill = "Tipo de município")+
  # Tema e título
  theme_minimal()

ggsave("municipioslabel.png",height = 20, width = 20, units = "cm")

library(readr)
write_rds(df, file.path(dropbox, "dados_espaciais.rds"))
