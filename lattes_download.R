
library(dplyr)
library(rio)
library(pipeR)
library(purrr)
library(stringr)
library(tidygraph)
library(igraph)
library(ggraph)
source('utils.R')

options(scipen = 999)

# --------------------------------------------------
# importar os dados
# --------------------------------------------------

termos <- 'arapaima gigas|pirarucu'

# artigos
rio::import("~/OneDrive/Rworkspace/lattes_2020/lattes_tables/ArtigosPublicados.rds") %>>% 
    tibble::as_tibble() %>>%
    dplyr::select(titulo_do_artigo, ano_do_artigo, issn, doi, titulo_do_periodico_ou_revista, id) %>>% 
    dplyr::mutate(titulo_do_artigo = stringr::str_trim(stringi::stri_trans_general(tolower(titulo_do_artigo), "Latin-ASCII"))) %>>%
    dplyr::filter(grepl(termos, ignore.case = T, titulo_do_artigo)) %>>%
    (rio::export(., 'rawfiles/artigos.rds'))

# livros
rio::import("~/OneDrive/Rworkspace/lattes_2020/lattes_tables/LivrosPublicados.rds") %>>% 
    as_tibble() %>>%
    dplyr::select(titulo_do_livro, ano, isbn, doi, pais_de_publicacao, cidade_da_editora, id) %>>% 
    dplyr::mutate(titulo_do_livro = stringr::str_trim(stringi::stri_trans_general(tolower(titulo_do_livro), "Latin-ASCII"))) %>>%
    dplyr::filter(grepl(termos, ignore.case = T, titulo_do_livro)) %>>%
    (export(., 'rawfiles/livros.rds'))

# capitulos livros
rio::import("~/OneDrive/Rworkspace/lattes_2020/lattes_tables/CapitulosLivros.rds") %>>% 
    as_tibble() %>>%
    dplyr::select(titulo_do_capitulo_do_livro, ano, isbn, doi, pais_de_publicacao, cidade_da_editora, id) %>>% 
    dplyr::mutate(titulo_do_capitulo_do_livro = stringr::str_trim(stringi::stri_trans_general(tolower(titulo_do_capitulo_do_livro), "Latin-ASCII"))) %>>%
    dplyr::filter(grepl(termos, ignore.case = T, titulo_do_capitulo_do_livro)) %>>%
    (export(., 'rawfiles/capitulos_livros.rds'))

# projetos
rio::import("/mnt/raid0/Pessoal/OneDrive/Rworkspace/lattes_2020/lattes_tables/ParticipacaoProjetos.rds") %>>% 
    as_tibble() %>>%
    dplyr::select(nome_do_projeto, descricao_do_projeto, situacao, natureza, financiadores, ano_inicio, ano_fim, equipe, id) %>>% 
    dplyr::mutate(nome_do_projeto = stringr::str_trim(stringi::stri_trans_general(tolower(nome_do_projeto), "Latin-ASCII"))) %>>%
    dplyr::filter(grepl(termos, ignore.case = T, nome_do_projeto)) %>>%
    (export(., 'rawfiles/projetos.rds'))

# formacao 
rio::import('~/OneDrive/Rworkspace/lattes_2020/lattes_tables/FormacaoGraduacao.rds') %>>% 
    as_tibble() %>>%
    dplyr::select(sequencia_formacao, nivel, titulo_do_trabalho_de_conclusao_de_curso, 
                  titulo_do_trabalho_de_conclusao_de_curso_ingles, nome_do_orientador, 
                  codigo_instituicao, nome_instituicao, codigo_curso, nome_curso, 
                  codigo_area_curso, status_do_curso, ano_de_inicio, ano_de_conclusao, id) %>>%
    dplyr::rename(titulo = titulo_do_trabalho_de_conclusao_de_curso, titulo_ingles = titulo_do_trabalho_de_conclusao_de_curso_ingles) %>>%
    dplyr::mutate(titulo = stringr::str_trim(stringi::stri_trans_general(tolower(titulo), "Latin-ASCII"))) %>>%
    dplyr::filter(grepl(termos, ignore.case = T, titulo) | grepl(termos, ignore.case = T, titulo_ingles)) %>>%
    dplyr::distinct(.keep_all = T) %>>%
    (rio::export(., '~/rawfiles/formacao_graduacao.rds'))

# formacao mestrado
rio::import('~/OneDrive/Rworkspace/lattes_2020/lattes_tables/FormacaoMestrado.rds') %>>% 
    as_tibble() %>>%
    dplyr::select(codigo_instituicao, nome_instituicao, nome_curso, ano_de_obtencao_do_titulo, 
                  titulo_da_dissertacao_tese, titulo_da_dissertacao_tese_ingles, ano_de_inicio, ano_de_conclusao,
                  nome_completo_do_orientador, id) %>>%
    rename(titulo = titulo_da_dissertacao_tese, titulo_ingles = titulo_da_dissertacao_tese_ingles) %>>%
    dplyr::mutate(titulo = stringr::str_trim(stringi::stri_trans_general(tolower(titulo), "Latin-ASCII"))) %>>%
    dplyr::filter(grepl(termos, ignore.case = T, titulo) | grepl(termos, ignore.case = T, titulo_ingles)) %>>%
    distinct(.keep_all = T) %>>%
    (export(., 'rawfiles/formacao_mestrado.rds'))

# formacao doutorado
rio::import('~/OneDrive/Rworkspace/lattes_2020/lattes_tables/FormacaoDoutorado.rds') %>>% 
    as_tibble() %>>%
    dplyr::select(codigo_instituicao, nome_instituicao, nome_curso, ano_de_obtencao_do_titulo, 
                  titulo_da_dissertacao_tese, titulo_da_dissertacao_tese_ingles, ano_de_inicio, ano_de_conclusao,
                  nome_completo_do_orientador, id) %>>%
    rename(titulo = titulo_da_dissertacao_tese, titulo_ingles = titulo_da_dissertacao_tese_ingles) %>>%
    dplyr::mutate(titulo = stringr::str_trim(stringi::stri_trans_general(tolower(titulo), "Latin-ASCII"))) %>>%
    dplyr::filter(grepl(termos, ignore.case = T, titulo) | grepl(termos, ignore.case = T, titulo_ingles)) %>>%
    distinct(.keep_all = T) %>>%
    (export(., 'rawfiles/formacao_doutorado.rds'))

# orientacoes mestrado
rio::import('~/OneDrive/Rworkspace/lattes_2020/lattes_tables/OrientacoesMestrado.rds') %>>% 
    as_tibble() %>>%
    dplyr::select(id, natureza, titulo, ano, doi, nome_da_instituicao) %>>%
    dplyr::mutate(titulo = stringr::str_trim(stringi::stri_trans_general(tolower(titulo), "Latin-ASCII"))) %>>%
    dplyr::filter(grepl(termos, ignore.case = T, titulo)) %>>%
    distinct(.keep_all = T) %>>%
    (export(., 'rawfiles/orientacoes_mestrado.rds'))

# orientacoes doutorado
rio::import('~/OneDrive/Rworkspace/lattes_2020/lattes_tables/OrientacoesDoutorado.rds') %>>% 
    as_tibble() %>>%
    dplyr::select(id, natureza, titulo, ano, doi, nome_da_instituicao) %>>%
    dplyr::mutate(titulo = stringr::str_trim(stringi::stri_trans_general(tolower(titulo), "Latin-ASCII"))) %>>%
    dplyr::filter(grepl(termos, ignore.case = T, titulo)) %>>%
    distinct(.keep_all = T) %>>%
    (export(., 'rawfiles/orientacoes_doutorado.rds'))

# obter os ids
ids <- c((import('rawfiles/artigos.rds') %>>% pull(id)),
         (import('rawfiles/livros.rds') %>>% pull(id)), 
         (import('rawfiles/capitulos_livros.rds') %>>% pull(id)), 
         (import('rawfiles/projetos.rds') %>>% pull(id)), 
         (import('rawfiles/formacao_graduacao.rds') %>>% pull(id)), 
         (import('rawfiles/formacao_mestrado.rds') %>>% pull(id)), 
         (import('rawfiles/formacao_doutorado.rds') %>>% pull(id)), 
         (import('rawfiles/orientacoes_mestrado.rds') %>>% pull(id)), 
         (import('rawfiles/orientacoes_doutorado.rds') %>>% pull(id)))

ids <- ids[ids != '']
ids <- ids[ids != 'noId']
ids <- unique(ids)
# export(ids, 'rawfiles/ids.rds')
ids <- import('rawfiles/ids.rds')

# dados gerais
rio::import(paste0(caminho, 'OneDrive/Rworkspace/lattes_2020/lattes_tables/DadosGerais.rds')) %>>% 
    dplyr::filter(.data$id %in% ids) %>>% 
    (rio::export(., 'rawfiles/dados_gerais.rds')) 

rio::import('rawfiles/dados_gerais.rds') %>>%
    (. -> dados_gerais)

rio::export(dados_gerais, 'rawfiles/dados_gerais.xlsx') 

# endereço profissional
rio::import('~/OneDrive/Rworkspace/lattes_2020/lattes_tables/EnderecoProfissional.rds') %>>% 
    dplyr::filter(.data$id %in% ids) %>>% 
    dplyr::select(- ramal, - fax, - caixa_postal) %>>%
    (rio::export(., 'rawfiles/endereco_profissional.rds'))

rio::import('rawfiles/endereco_profissional.rds') ->
    endereco_profissional

rio::export(endereco_profissional, 'rawfiles/endereco_profissional.xlsx') 

# --------------------------------------------------
# exportar em excel
# --------------------------------------------------

rio::import('rawfiles/artigos.rds') |>
    tibble::tibble() ->
    artigos

# 497 artigos científicos
artigos |>
    dplyr::distinct(titulo_do_artigo, .keep_all = T) 


# ------------
rio::import('rawfiles/livros.rds') |>
    tibble::tibble() ->
    livros

# 35 livros
livros |>
    dplyr::distinct(titulo_do_livro, .keep_all = T) |>
    dplyr::count(pais_de_publicacao)

# ------------
rio::import('rawfiles/capitulos_livros.rds') |>
    tibble::tibble() ->
    capitulos_livros

# 87 capítulos de livros
capitulos_livros |>
    dplyr::distinct(titulo_do_capitulo_do_livro, .keep_all = T) |>
    dplyr::count(pais_de_publicacao)

capitulos_livros |>
    dplyr::distinct(titulo_do_capitulo_do_livro, .keep_all = T) |>
    dplyr::filter(pais_de_publicacao == 'Peru') |>
    dplyr::pull(titulo_do_capitulo_do_livro)

# ------------
rio::import('rawfiles/formacao_graduacao.rds') |>
    tibble::tibble() ->
    formacao_graduacao
# 165 monografias

# ------------
rio::import('rawfiles/formacao_mestrado.rds') |>
    tibble::tibble() ->
    formacao_mestrado
# 115 dissertações

# ------------
rio::import('rawfiles/formacao_doutorado.rds') |>
    tibble::tibble() ->
    formacao_doutorado
# 62 teses

# ------------
rio::import('rawfiles/orientacoes_mestrado.rds') |>
    tibble::tibble() ->
    orientacoes_mestrado
# 153 orientações de mestrado

# ------------
rio::import('rawfiles/orientacoes_doutorado.rds') |>
    tibble::tibble() ->
    orientacoes_doutorado
# 58 orientações de doutorado

# 568 projetos
# ------------
rio::import('rawfiles/projetos.rds') |>
    tibble::tibble() ->
    projetos

rio::import('rawfiles/endereco3.rds') |>
    tibble::as_tibble() ->
    endereco3

projetos |>
    dplyr::distinct(nome_do_projeto, .keep_all = T) |>
    dplyr::filter(nchar(id) == 16) ->
    projetos2

dataframe <- projetos2
textToMacht <- 'nome_do_projeto'
cleanText = TRUE
method = 'Levenshtein'
similarityScore = 0.9

# executar dentro fora da função
macthText(projetos2, 'nome_do_projeto', cleanText = T, similarityScore = 0.9) |>
tt |>
    dplyr::select(- nome_do_projeto) |>
    dplyr::rename(titulo = titulo_normalizado) |>
    dplyr::relocate(titulo) |>
    dplyr::select(titulo, id, idd, ano_inicio, equipe) -> 
    projetos3

dados_gerais |> 
    dplyr::select(nome_completo,id) ->
    dados_gerais3

projetos3 |>
    dplyr::select(- titulo) |> 
    tidyr::unnest(equipe) |> 
    dplyr::select(id, idd, nome_completo, ordem_de_integracao, flag_responsavel) |>
    dplyr::rename(nome_no_projeto = nome_completo) |> 
    dplyr::left_join(dados_gerais3) |>
    dplyr::relocate(id, nome_completo, idd) -> 
    projetos4

projetos4 |>
    dplyr::filter(ordem_de_integracao == 1) |>
    dplyr::distinct(.keep_all = TRUE) |>
    dplyr::group_by(id, flag_responsavel) |>
    dplyr::tally(name = 'qtde_projetos', sort = T) |>
    dplyr::ungroup() |>
    dplyr::left_join(dados_gerais |> dplyr::select(nome_completo, id)) |>
    dplyr::relocate(id, nome_completo) ->
    projetos5

projetos4 |>
    dplyr::filter(ordem_de_integracao == 1) |>
    dplyr::distinct(.keep_all = TRUE) |>
    dplyr::count(id, name = 'total_projetos', sort = T) |> 
    dplyr::filter(total_projetos >= 4) |>
    dplyr::left_join(dados_gerais |> dplyr::select(nome_completo, id)) ->
    quem_fica

projetos5 |>
    dplyr::filter(id %in% quem_fica$id) |>
    dplyr::mutate(flag_responsavel = ifelse(flag_responsavel == 'NAO', 'No', 'Yes')) |> 
    dplyr::rename(Leader = flag_responsavel ) -> 
    projetos6

projetos6 |>
    dplyr::group_by(nome_completo) |>
    dplyr::summarise(qtde_artigos = sum(qtde_projetos)) |>
    dplyr::arrange(qtde_artigos, nome_completo) |>
    dplyr::ungroup() ->
    ordem_autor_projetos

projetos6$nome_completo <- factor(projetos6$nome_completo, levels = ordem_autor_projetos$nome_completo)

ggplot2::ggplot(projetos6, aes(x = nome_completo, y = qtde_projetos, fill = Leader, label = qtde_projetos)) +  
    ggplot2::geom_bar(stat = "identity") + 
    coord_flip() +
    ggplot2::geom_text(size = 4, position = position_stack(vjust = 0.5)) +
    scale_y_continuous(name = 'Projects', breaks = seq(0, 15, by = 1), limits = c(0, 15)) +
    theme_classic() +
    xlab(NULL) +
    theme(legend.position = "bottom", axis.text.y = element_text(size = 12))

ggsave('images/projetos-autor-lattes-v2.png', width = 18, height = 12, units = c("cm"))


# --------------------------------------------------
# rede de colaboração entre estados (artigos)
# --------------------------------------------------

artigos |>
    dplyr::arrange(desc(ano_do_artigo), titulo_do_artigo) ->
    artigos2

# endereço (UF) dos autores
endereco_profissional |>
    dplyr::filter(id %in% unique(artigos2$id)) |>
    dplyr::select(nome_instituicao_empresa, pais, uf, cidade, id) |> 
    dplyr::mutate(nome_instituicao = stringr::str_trim(stringi::stri_trans_general(tolower(nome_instituicao_empresa), "Latin-ASCII"))) |>
    dplyr::mutate(uf2 = paste(pais, uf, sep = '_')) |>
    dplyr::mutate(uf2 = gsub('_$', '', uf2)) |>
    dplyr::select(nome_instituicao, pais, uf2, id) -> 
    endereco

# ------------------
# buscar endereços não encontrados
endereco |>
    dplyr::filter(uf2 == '') |>
    dplyr::pull(id) ->
    ids_nouf

# para as pessoas com endereço ausente adicionar o endereço do doutorado ou do mestrado
formacao_doutorado |>
    dplyr::filter(id %in% ids_nouf) |>
    dplyr::select(nome_instituicao, id) ->
    a1

formacao_mestrado |>
    dplyr::filter(id %in% ids_nouf) |>
    dplyr::select(nome_instituicao, id) ->
    a2

formacao_graduacao |>
    dplyr::filter(id %in% ids_nouf) |>
    dplyr::select(nome_instituicao, id) ->
    a3

rbind(a1, a2, a3) |>
    dplyr::distinct(.keep_all = T) |>
    dplyr::mutate(nome_instituicao = stringr::str_trim(stringi::stri_trans_general(tolower(nome_instituicao), "Latin-ASCII"))) ->
    formacao_universidade

# formacao_universidade |> dplyr::count(nome_instituicao, sort = T) |> print(n = Inf)

# estados e suas siglas
rio::import('rawfiles/sigla_estados.csv') %>>%
    tibble() %>>%
    (. -> uf)

# obter os enderecos das instituicoes
read.csv('rawfiles/SUP_IES_2019.CSV', sep = '|', fileEncoding = 'iso-8859-1') |>
    tibble::as_tibble() |>
    janitor::clean_names() |>
    dplyr::select(co_ies, no_ies, sg_ies, co_regiao, co_uf) |>
    dplyr::mutate(no_ies = stringr::str_trim(stringi::stri_trans_general(tolower(no_ies), "Latin-ASCII"))) |>
    dplyr::left_join(uf) |>
    dplyr::select(no_ies, sigla_uf) |>
    dplyr::rename(nome_instituicao = no_ies) -> 
    inst

formacao_universidade |>
    dplyr::left_join(inst) ->
    isnt_uf

isnt_uf |>
    dplyr::filter(is.na(sigla_uf)) |>
    dplyr::count(nome_instituicao, sort = T) |>
    dplyr::mutate(sigla_uf2 = NA) |>
    {\(acima) rio::export(acima, 'rawfiles/universidades_sem_estado.csv')}()
    
rio::import('rawfiles/universidades_sem_estado2.csv') |>
    tibble::as_tibble() |>
    dplyr::select(- .data$n) ->
    inst_uf_manual

isnt_uf |>
    dplyr::full_join(inst_uf_manual) |>
    dplyr::mutate(sigla_uf = ifelse(is.na(sigla_uf), sigla_uf2, sigla_uf)) |>
    dplyr::select(nome_instituicao2 = nome_instituicao, uf3 = sigla_uf, id) |>
    dplyr::distinct(id, .keep_all = TRUE) ->
    inst_uf_manual2 

## restaram 73 autores sem endereço
endereco |>
    dplyr::select(- pais) |> 
    dplyr::distinct(id, .keep_all = TRUE) |>
    dplyr::left_join(inst_uf_manual2) |>
    dplyr::mutate(nome_instituicao = ifelse(nome_instituicao == '', nome_instituicao2, nome_instituicao)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == '', uf3, uf2)) |>
    dplyr::select(- nome_instituicao2, - uf3) |>
    dplyr::filter(!is.na(uf2)) -> 
    endereco2

endereco2 |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'DF', 'Brasil_DF', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'AM', 'Brasil_AM', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'PA', 'Brasil_PA', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'MG', 'Brasil_MG', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'PE', 'Brasil_PE', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'RJ', 'Brasil_RJ', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'RO', 'Brasil_RO', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'SP', 'Brasil_SP', uf2)) ->
    endereco2
    
rio::export(endereco2, 'rawfiles/endereco2.xlsx')

dados_gerais |>
    dplyr::select(nome_completo, id) ->
    dados_gerais2

artigos2 |> 
    dplyr::left_join(endereco2) |>
    dplyr::left_join(dados_gerais2) ->
    artigos3

# normalizar títulos
artigos3 <- macthText(dataframe = artigos3, textToMacht = 'titulo_do_artigo', cleanText = T, similarityScore = 0.9)

artigos3 |> 
    dplyr::group_indices(titulo_normalizado) ->  
    pmk

artigos3 |> 
    dplyr::mutate(pmk = pmk) |> 
    dplyr::arrange(pmk) ->
    artigos3

# artigos3 %>>% count(pmk)
# artigos3 %>>% glimpse()

# gerando edges
artigos3 |> 
    dplyr::select(pmk, uf2) |> 
    dplyr::arrange(pmk) |>
    dplyr::filter(!is.na(uf2)) ->
    temp

split(temp, f = temp$pmk) |>
    purrr::map(~ .x |> dplyr::pull(uf2)) |>
    purrr::map(function(x) {expand.grid.unique(x, x, include.equals = F)}) |>
    dplyr::bind_rows() |>
    tibble::as_tibble() ->
    ide

# gerando vertices
c(ide$V1, ide$V2) |>
    unique() |>
    {\(x) tibble::tibble(name = x)}() ->
    idv

artigos3 |> 
    dplyr::select(pmk, uf2) |> 
    dplyr::filter(!is.na(uf2)) |>
    dplyr::group_by(pmk) |> 
    dplyr::distinct(uf2, .keep_all = TRUE) |>
    dplyr::ungroup() |>
    dplyr::group_by(uf2) |> 
    dplyr::count(uf2, name = 'qtde_artigos') |> 
    dplyr::rename(name = uf2) |> 
    dplyr::ungroup() |>
    dplyr::arrange(desc(qtde_artigos)) ->
    qtde_artigos

igraph::graph.data.frame(ide, directed = FALSE, vertices = idv) |>
    tidygraph::as_tbl_graph() -> 
    netocuf

netocuf |>
    tidygraph::activate(nodes) |>
    dplyr::left_join(qtde_artigos) |>
    dplyr::mutate(estado = name) -> 
    netocuf

uf |>
    dplyr::mutate(regiao_id = stringr::str_sub(co_uf, 1, 1)) |>
    dplyr::select(estado = sigla_uf, regiao, regiao_id) ->
    bb

V(netocuf)$estado <- gsub('Brasil_', '', V(netocuf)$estado)

netocuf |>
    tidygraph::activate(nodes) |>
    dplyr::left_join(bb) |>
    dplyr::mutate(regiao = ifelse(is.na(regiao), 'Exterior', regiao)) |>
    dplyr::mutate(regiao_id = ifelse(is.na(regiao_id), 6, regiao_id)) ->
    netocuf

V(netocuf)$english <- V(netocuf)$name
V(netocuf)$english <- gsub('Brasil_', 'Brazil_', V(netocuf)$english)
V(netocuf)$english <- gsub('Inglaterra', 'England', V(netocuf)$english)
V(netocuf)$english <- gsub('Estados Unidos', 'United States', V(netocuf)$english)
V(netocuf)$english <- gsub('Itália', 'Italy', V(netocuf)$english)
V(netocuf)$english <- gsub('Noruega', 'Norway', V(netocuf)$english)
V(netocuf)$english <- gsub('Canadá', 'Canada', V(netocuf)$english)
V(netocuf)$english <- gsub('Alemanha', 'Germany', V(netocuf)$english)

V(netocuf)$id <- V(netocuf)$english

# exportar para o vosviewer
write_graph(netocuf, file = 'networks/netocuf.net', format = c("pajek"))
writePajek(V(netocuf)$qtde_artigos, 'networks/netocuf_qtde.vec')
writePajek(V(netocuf)$regiao_id, 'networks/netocuf_regiao.clu')

netocuf |>
    tidygraph::activate(nodes) |>
    dplyr::mutate(degree = degree(netocuf),
                  closeness = closeness(netocuf),
                  betweenness = betweenness(netocuf)) |>
    tibble::as_tibble() ->
    netocuf_atributos

# ggraph(netocuf, layout = 'kk') + 
#     geom_edge_link(aes(size = weight), show.legend = FALSE) + 
#     geom_node_point(aes(size = qtde_artigos), show.legend = FALSE) + 
#     geom_node_text(aes(label = name)) +
#     theme_graph(foreground = 'steelblue', fg_text_colour = 'white')

# --------------------------------------------------
# sjr scimago - qualidade da pesquisa
# --------------------------------------------------

readr::read_csv2('rawfiles/scimagojr_2019_filtrado.csv') |>
    janitor::clean_names() |>
    dplyr::arrange(desc(sjr)) |>
    tidyr::separate_rows(issn, sep = ',') |>
    dplyr::mutate(issn = stringr::str_trim(issn)) |>
    dplyr::distinct(issn, .keep_all = TRUE) ->
    sjr

# para os artigos publicados em revistas sem sjr foi atribuído a pontuação de 20
artigos3 |>
    dplyr::left_join(sjr %>>% dplyr::select(issn, sjr)) |>
    dplyr::mutate(sjr = ifelse(is.na(sjr), 20, sjr)) ->
    artigos4

artigos4 |>
    dplyr::select(pmk, uf2, sjr) |> 
    dplyr::filter(!is.na(uf2)) |> 
    dplyr::group_by(pmk, uf2) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::group_by(uf2) |>
    dplyr::summarise(sjr = sum(sjr), qtde_artigos = n()) |>
    dplyr::mutate(sjr_medio = sjr / qtde_artigos) |>
    dplyr::arrange(desc(qtde_artigos)) |>
    dplyr::select(- qtde_artigos) |> 
    dplyr::rename(name = uf2) -> 
    sjr_lattes

netocuf_atributos |>
    dplyr::arrange(desc(qtde_artigos)) |>
    dplyr::left_join(sjr_lattes) |>
    dplyr::select(english, qtde_artigos, sjr, sjr_medio, degree, closeness, betweenness) ->
    netocuf_atributos2

netocuf_atributos2 |>
    dplyr::filter(grepl('Brazil_*', ignore.case = T, english)) ->
    netocuf_brazil

sum(netocuf_brazil$qtde_artigos)
sum(netocuf_brazil$sjr)
sum(netocuf_brazil$sjr)/478

rio::export(netocuf_atributos2, 'rawfiles/netocuf_atributos2.xlsx') 

# --------------------------------------------------
# taxa de crescimento lattes
# --------------------------------------------------

artigos3 |>
    dplyr::mutate(ano = as.numeric(ano_do_artigo)) |>
    dplyr::select(ano, pmk) |>
    dplyr::group_by(ano) |>
    dplyr::summarise(Papers_Lattes = n()) |>
    dplyr::arrange(ano) |> 
    dplyr::filter(ano %in% c(2000:2020)) %>>% 
    dplyr::mutate(trend_Lattes = 1:n(), lnp_Lattes = log(Papers_Lattes)) -> 
    d_lattes

m1 <- lm(lnp_Lattes ~ trend_Lattes, data = d_lattes)
# summary(m1)

beta0 <- m1$coefficients[[1]]
beta1 <- m1$coefficients[[2]]

# modelo não linear
# 2000 é o primeiro ano da série
m2 <- nls(Papers_Lattes ~ b0 * exp(b1 * (ano - 2000)), start = list(b0 = beta0, b1 = beta1), data = d_lattes)
# summary(m2)

# publications estimado
d_lattes$predicted <- 17.62763 * exp(0.07794 * (d_lattes$ano - 2000))

# taxa de crescimento anual é de 
# (exp(0.07794) - 1) * 100

# o período necessário para dobrar o tempo é 
# log(2) / 0.07794

d_lattes |> 
    dplyr::rename(Year = ano, Papers = Papers_Lattes) |> 
    dplyr::mutate(Predicted = round(predicted, 0)) |> 
    dplyr::filter(Year >= 2000) ->
    d2

p <- ggplot2::ggplot(d2, aes(x = Year, y = Papers)) + 
    ggplot2::geom_line(aes(x = Year, y = Papers, colour = "Papers Lattes")) + 
    ggplot2::geom_point(aes(y = Papers, color = 'Papers Lattes')) + 
    ggplot2::geom_line(aes(y = Predicted, color = 'Predicted Lattes'), linetype = 'longdash') + 
    ggplot2::geom_point(aes(y = Predicted, color = 'Predicted Lattes')) + 
    scale_x_continuous(name = 'Year', breaks = seq(2000, 2020, by = 2), limits = c(2000, 2020)) +
    scale_y_continuous(name = 'Papers', breaks = seq(0, 115, by = 20), limits = c(0, 115)) +
    scale_color_manual(name = "Publications", values = c("Predicted Lattes" = "red", "Papers Lattes" = "black")) +
    theme_bw() +
    ylab('Papers') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "bottom")

p + ggplot2::annotate("text", x = 2008, y = 100, size = 3, label = 'Predicted Y_p = 17.62 * e ^ 0.07 (year - 2000)', parse = F) 

ggsave('images/taxa-crescimento-lattes.png', width = 10, height = 10, units = c("cm"))

# --------------------------------------------------
# projetos por região ao longo do tempo
# --------------------------------------------------

projetos3 |>
    tidyr::unnest(equipe) |> 
    dplyr::select(id, idd, titulo, nome_completo, ano_inicio, ordem_de_integracao, flag_responsavel) |>
    dplyr::rename(nome_no_projeto = nome_completo) |> 
    dplyr::left_join(dados_gerais |> dplyr::select(nome_completo, id)) |>
    dplyr::relocate(id, nome_completo, idd , titulo) -> 
    projetos4

projetos4 |> 
    dplyr::mutate(ano_inicio = as.numeric(ano_inicio)) |> 
    dplyr::filter(ordem_de_integracao == 1) |> 
    dplyr::distinct(.keep_all = TRUE) |>
    dplyr::select(- nome_no_projeto, - ordem_de_integracao, - idd, - flag_responsavel) -> 
    projetos5 

projetos5 |>
    dplyr::count(ano_inicio, name = 'qtde_projetos', sort = F) |>
    dplyr::filter(ano_inicio %in% 2000:2020) ->
    projetos_ano

ggplot2::ggplot(projetos_ano, aes(x = ano_inicio, y = qtde_projetos)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(name = 'Year', breaks = seq(2000, 2020, by = 2), limits = c(2000, 2020)) +
    scale_y_continuous(name = 'Projects', breaks = seq(0, 60, by = 5), limits = c(0, 60)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave('images/projetos-anos-lattes.png', width = 14, height = 10, units = c("cm"))

## endereço do líder do projeto
projetos4 |> 
    dplyr::mutate(ano_inicio = as.numeric(ano_inicio)) |> 
    dplyr::filter(ordem_de_integracao == 1) |>
    dplyr::filter(flag_responsavel == 'SIM') |>
    dplyr::distinct(id, .keep_all = TRUE) |>
    dplyr::pull(id) ->
    id_lider_projeto

## manter apenas lideres com mestrado ou doutorado
rio::import('/mnt/raid0/Pessoal/Documents/Rworkspace/lattes_2020/lattes_tables/FormacaoMestrado.rds') |> 
    tibble::as_tibble() ->
    mestrado

rio::import('/mnt/raid0/Pessoal/Documents/Rworkspace/lattes_2020/lattes_tables/FormacaoDoutorado.rds') |> 
    tibble::as_tibble() ->
    doutorado

doutorado |>
    dplyr::filter(id %in% id_lider_projeto) |>
    dplyr::pull(id) ->
    lideres_doutores

mestrado |>
    dplyr::filter(id %in% id_lider_projeto) |>
    dplyr::pull(id) ->
    lideres_mestres

c(lideres_mestres, lideres_doutores) |>
    unique() ->
    lideres_mestres_doutores

projetos5 |>
    dplyr::filter(id %in% lideres_mestres_doutores) ->
    projetos6

# endereço (UF) dos autores de projetos
endereco_profissional |>
    dplyr::filter(id %in% unique(id_lider_projeto)) |>
    dplyr::select(nome_instituicao_empresa, pais, uf, cidade, id) |> 
    dplyr::mutate(nome_instituicao = stringr::str_trim(stringi::stri_trans_general(tolower(nome_instituicao_empresa), "Latin-ASCII"))) |>
    dplyr::mutate(uf2 = paste(pais, uf, sep = '_')) |>
    dplyr::mutate(uf2 = gsub('_$', '', uf2)) |>
    dplyr::select(nome_instituicao, pais, uf2, id) -> 
    endereco 

# ------------------
# buscar endereços não encontrados
endereco |>
    dplyr::filter(uf2 == '') |>
    dplyr::pull(id) ->
    ids_nouf

# para as pessoas com endereço ausente adicionar o endereço do doutorado ou do mestrado
formacao_doutorado |>
    dplyr::filter(id %in% ids_nouf) |>
    dplyr::select(nome_instituicao, id) ->
    a1

formacao_mestrado |>
    dplyr::filter(id %in% ids_nouf) |>
    dplyr::select(nome_instituicao, id) ->
    a2

formacao_graduacao |>
    dplyr::filter(id %in% ids_nouf) |>
    dplyr::select(nome_instituicao, id) ->
    a3

rbind(a1, a2, a3) |>
    dplyr::distinct(.keep_all = T) |>
    dplyr::mutate(nome_instituicao = stringr::str_trim(stringi::stri_trans_general(tolower(nome_instituicao), "Latin-ASCII"))) ->
    formacao_universidade

# formacao_universidade |> dplyr::count(nome_instituicao, sort = T) |> print(n = Inf)

# estados e suas siglas
rio::import('rawfiles/sigla_estados.csv') |>
    tibble() ->
    uf

# obter os enderecos das instituicoes
read.csv('rawfiles/SUP_IES_2019.CSV', sep = '|', fileEncoding = 'iso-8859-1') |>
    tibble::as_tibble() |>
    janitor::clean_names() |>
    dplyr::select(co_ies, no_ies, sg_ies, co_regiao, co_uf) |>
    dplyr::mutate(no_ies = stringr::str_trim(stringi::stri_trans_general(tolower(no_ies), "Latin-ASCII"))) |>
    dplyr::left_join(uf) |>
    dplyr::select(no_ies, sigla_uf) |>
    dplyr::rename(nome_instituicao = no_ies) -> 
    inst

formacao_universidade |>
    dplyr::left_join(inst) ->
    isnt_uf

isnt_uf |>
    dplyr::filter(is.na(sigla_uf)) |>
    dplyr::count(nome_instituicao, sort = T) |>
    dplyr::mutate(sigla_uf2 = NA) |>
    {\(acima) rio::export(acima, 'rawfiles/universidades_sem_estado_projetos.csv')}()
    
rio::import('rawfiles/universidades_sem_estado2_projetos.csv') |>
    tibble::as_tibble() |>
    dplyr::select(- .data$n) ->
    inst_uf_manual

isnt_uf |>
    dplyr::full_join(inst_uf_manual) |>
    dplyr::mutate(sigla_uf = ifelse(is.na(sigla_uf), sigla_uf2, sigla_uf)) |>
    dplyr::select(nome_instituicao2 = nome_instituicao, uf3 = sigla_uf, id) |>
    dplyr::distinct(id, .keep_all = TRUE) ->
    inst_uf_manual2 

## restaram X autores sem endereço
endereco |>
    dplyr::select(- pais) |> 
    dplyr::distinct(id, .keep_all = TRUE) |>
    dplyr::left_join(inst_uf_manual2) |>
    dplyr::mutate(nome_instituicao = ifelse(nome_instituicao == '', nome_instituicao2, nome_instituicao)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == '', uf3, uf2)) |>
    dplyr::select(- nome_instituicao2, - uf3) |>
    dplyr::filter(!is.na(uf2)) -> 
    endereco2

endereco2 |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'AM', 'Brasil_AM', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'PA', 'Brasil_PA', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'RO', 'Brasil_RO', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'MA', 'Brasil_MA', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'SP', 'Brasil_SP', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'AC', 'Brasil_AC', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'BA', 'Brasil_BA', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'DF', 'Brasil_DF', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'GO', 'Brasil_GO', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'MG', 'Brasil_MG', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'MT', 'Brasil_MT', uf2)) |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'TO', 'Brasil_TO', uf2)) ->
    endereco2
 
rio::export(endereco2, 'rawfiles/endereco2_projetos.xlsx')

projetos5 |>
    dplyr::left_join(endereco |> dplyr::select(id, uf2)) |>
    dplyr::filter(uf2 == '') |>
    dplyr::distinct(id, .keep_all = TRUE) |>
    dplyr::mutate(link = paste0('http://lattes.cnpq.br/', id)) |> 
    dplyr::select(id, link, uf2) |>
    {\(acima) rio::export(acima, 'rawfiles/lideres_projetos_sem_estado.xlsx')}()

# espaço de testes

projetos3 |>
    tidyr::unnest(equipe) |> 
    dplyr::select(id, idd, titulo, nome_completo, ano_inicio, ordem_de_integracao, flag_responsavel) |>
    dplyr::rename(nome_no_projeto = nome_completo) |> 
    dplyr::left_join(dados_gerais |> dplyr::select(nome_completo, id)) |>
    dplyr::left_join(endereco2) |>
    dplyr::filter(ordem_de_integracao == 1) |> 
    dplyr::filter(flag_responsavel == 'SIM') |> 
    dplyr::distinct(.keep_all = TRUE) |>
    dplyr::select(- nome_no_projeto, - flag_responsavel, - ordem_de_integracao) |>
    dplyr::filter(id %in% lideres_mestres_doutores) ->
    projetos_lideres_mestres_doutores

projetos_lideres_mestres_doutores |> 
    dplyr::count(uf2, sort = T) |>
    print(n = Inf)

projetos_lideres_mestres_doutores |>
    dplyr::filter(!is.na(uf2)) |>
    dplyr::filter(grepl('Brasil_*', ignore.case = T, uf2)) ->
    projetos_lideres_mestres_doutores_brasil

projetos_lideres_mestres_doutores_brasil |> 
    dplyr::count(uf2, sort = T) |>
    print(n = Inf)

# construir o mapa
# renv::install('ipeaGIT/geobr')
# renv::install('ggspatial')
# devtools::install_github("ipeaGIT/geobr", subdir = "r-package")

library(sf) 
library(geobr) 
library(ggspatial) 

uf <- geobr::read_state()

projetos_lideres_mestres_doutores_brasil |>
    dplyr::group_by(uf2) |>
    dplyr::count(uf2, name = 'total_projetos', sort = T) |>
    dplyr::ungroup() |>
    dplyr::mutate(abbrev_state = gsub('Brasil_', '', uf2)) |>
    dplyr::right_join(uf) -> 
    uf2

uf2[ is.na(uf2$total_projetos), 'total_projetos'] <- 0

class(uf2)
summary(uf2$total_projetos)

ggplot() +
    geom_sf(data = uf2, aes(fill = total_projetos, geometry = geom), color = "Black", size = .15) +
    geom_sf_label(data = uf2, aes(label = abbrev_state, , geometry = geom), label.padding = unit(1, "mm")) +
    scale_fill_distiller(limits = c(0, 60), name = "Projects", palette = 2, direction = 1) + 
    theme(legend.position = c(.25, .3), 
          axis.title.x = element_blank(),  
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.title.y = element_blank(),  
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(),
          legend.key = element_rect(colour = "white"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.margin = unit(0, "lines"),
          plot.background = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) 

ggsave('images/lattes-projetos-mapa.png', width = 16, height = 16, units = c("cm"))




# --------------------------------------------------
# google citations
# --------------------------------------------------

library(rvest)
library(purrr)

artigos |>
    dplyr::distinct(titulo_do_artigo, .keep_all = T) |>
    dplyr::select(titulo_do_artigo, doi) |>
    dplyr::mutate(search = ifelse(doi == '', titulo_do_artigo, doi)) -> 
    cc

head(cc$search)

dd <- paste0('https://scholar.google.com.br/scholar?hl=en&as_sdt=0%2C5&q=', cc$search, '&btnG=')

simple <- read_html(dd[[1]])


