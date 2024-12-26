dropbox <- "c:/Users/victor/dropbox/DISSERTACAO"

#### Meu exemplo
library(sf)
library(dplyr)
fronteira <- read_sf(file.path(dropbox,"Fronteira/Faixa_de_Fronteira_por_UF_2022.shp")) %>%
  st_transform("WGS84") |> 
  rename("id_uf" = "CD_UF",
         "nome_uf" = "NM_UF",
         'sigla_uf' = "SIGLA_UF",
         "nome_regiao" = "NM_REGIAO",
         "area_uf" = "AREA_KM2",
         "area_integrada" = "AREA_INT",
         "porcentagem_integrada" = "PORC_INT")
fronteira

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

municipios_fronteira

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
                           sigla_uf %in% c("RO", "MS", "MT") ~ "Arco Central",
                           sigla_uf %in% c("PR", "SC", "RS") ~ "Arco Sul",
                           sigla_uf %in% c("SP") ~ "Arco Sudeste")) |> 
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

df
rm(t)

#Carrega o sf dos países da América do Sul
america <- st_read(file.path(dropbox,"America/South_America.shp")) %>% 
  st_transform("WGS84")

# prepara para juntar demais países da américa do sul na base de municípios
# Remove regiões sem fronteira com o br
paises_fronteira <- america %>%
  filter(COUNTRY %in% c("French Guiana (France)", "Suriname", "Guyana", "Venezuela", 
                        "Colombia", "Peru", "Bolivia", "Paraguay", "Argentina", "Uruguay"))

# verifica interseções
a <- st_intersects(df, paises_fronteira, sparse = FALSE)

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

brasil <- america %>%
  filter(COUNTRY == "Brazil")

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

library(sf)
library(dplyr)

# Supondo que a base esteja carregada como um data frame
# Converte a base para um objeto sf, assumindo que GEOM contém geometria WKT
sede_municipios_sf <- st_as_sf(sede_municipios, wkt = "GEOM", crs = 4326)

# Extrai latitude e longitude
sede_municipios <- sede_municipios_sf %>%
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()  # Remove a coluna de geometria, se não for mais necessária


t <- select(df, id_municipio)
st_geometry(t) <- NULL

sede_municipios <- sede_municipios |> 
  semi_join(t, by="id_municipio")

rm(t)

# Criar um buffer de 50 metros para dentro do polígono do Brasil na fronteira com esses países
buffer_fronteira <- st_buffer(paises_fronteira, dist = 50)

# Definir a linha da fronteira terrestre do Brasil com base nesse buffer
fronteira_terrestre <- st_intersection(brasil, buffer_fronteira)

# Converter a fronteira terrestre em uma linha (apenas a borda do Brasil que faz fronteira terrestre)
fronteira_linha <- st_boundary(fronteira_terrestre) |> 
  group_by(COUNTRY) |> 
  summarise()

# Calcular a distância entre as sedes municipais e a linha da fronteira terrestre
distancias_terrestres <- st_distance(sede_municipios, fronteira_linha)

# Adicionar a distância calculada ao dataframe de sede_municipio
sede_municipios$distancia_fronteira_terrestre <- as.numeric(distancias_terrestres)

#### limite da faixa de fronteira. Definimos como fronteira interior o limite integral dos municípios pertencentes à FF
# Esse limite é irregular, pois não acompanha os 150 km da fronteira terrestre.
# Sedes municipais da atual faixa de fronteira (tratamento) tem distâncias negativas, enquanto o controle tem distâncias positivas.
faixa <- st_read(file.path(dropbox,"Municipios_Fronteira/Municipios_Faixa_Fronteira_2022.shp")) %>%
  st_transform("WGS84") |> 
  mutate(pais = "BR") |>  
  group_by(pais) |> 
  summarise()

faixa_linha <- st_boundary(faixa)

# Criar um buffer de 5 km ao redor da linha de fronteira
buffer_fronteira <- st_buffer(fronteira_linha, dist = 5000) |> 
  st_union()

# Definir a faixa de fronteira interior do Brasil como a exclusão desse buffer
faixa_interior <- st_difference(faixa_linha, buffer_fronteira)

# Calcular a distância entre as sedes municipais e a linha da fronteira terrestre
distancias_interior <- st_distance(sede_municipios, faixa_interior)

# Adicionar a distância calculada ao dataframe de sede_municipio
sede_municipios$distancias_fronteira_interior <- as.numeric(distancias_interior)

t <- sede_municipios |> 
  select(id_municipio, distancia_fronteira_terrestre, distancias_fronteira_interior)
st_geometry(t) <- NULL

df <- df |> 
  left_join(t, by = "id_municipio") |> 
  #torna as distâncias da faixa de fronteira original negativas
  mutate(distancias_fronteira_interior = ifelse(groups == "treatment", 
                                                -distancias_fronteira_interior, 
                                                distancias_fronteira_interior))


rm(t)

library(readr)
write_rds(df, file.path(dropbox, "dados_espaciais.rds"))
