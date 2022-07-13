
# Buscar endereços para todos os autores

# Análise custosa.

rio::import('rawfiles/artigos.rds') |>
    tibble::tibble() ->
    artigos

rio::import('rawfiles/livros.rds') |>
    tibble::tibble() ->
    livros

rio::import('rawfiles/capitulos_livros.rds') |>
    tibble::tibble() ->
    capitulos_livros

rio::import('rawfiles/projetos.rds') |>
    tibble::tibble() ->
    projetos

rio::import('rawfiles/formacao_graduacao.rds') |>
    tibble::tibble() ->
    monografias

rio::import('rawfiles/formacao_mestrado.rds') |>
    tibble::tibble() ->
    mestrado

rio::import('rawfiles/formacao_doutorado.rds') |>
    tibble::tibble() ->
    doutorado

ids <- unique(c(artigos$id, livros$id, capitulos_livros$id, projetos$id, monografias$id, mestrado$id, doutorado$id))

# ------------------
# opter endereço
rio::import('rawfiles/endereco_profissional.rds') |>
    dplyr::filter(id %in% ids) |>
    dplyr::select(nome_instituicao_empresa, pais, uf, cidade, id) |> 
    dplyr::mutate(nome_instituicao = stringr::str_trim(stringi::stri_trans_general(tolower(nome_instituicao_empresa), "Latin-ASCII"))) |>
    dplyr::mutate(uf2 = paste(pais, uf, sep = '_')) |>
    dplyr::mutate(uf2 = gsub('_$', '', uf2)) |>
    dplyr::select(nome_instituicao, pais, uf2, id) -> 
    endereco

# ------------------
# buscar endereços não encontrados em formacao
endereco |>
    dplyr::filter(uf2 == '') |>
    dplyr::pull(id) ->
    ids_nouf

rio::import('~/OneDrive/Rworkspace/lattes_2020/lattes_tables/FormacaoGraduacao.rds') |> 
    dplyr::filter(id %in% ids_nouf) |>
    dplyr::select(codigo_instituicao, nome_instituicao, ano = ano_de_inicio, id) |>
    dplyr::mutate(grau = 'graduacao') ->
    formacao_graduacao

rio::import('~/OneDrive/Rworkspace/lattes_2020/lattes_tables/FormacaoMestrado.rds') |> 
    dplyr::filter(id %in% ids_nouf) |>
    dplyr::select(nome_instituicao, codigo_instituicao, ano = ano_de_inicio, id) |>
    dplyr::mutate(grau = 'mestrado') ->
    formacao_mestrado

rio::import('~/OneDrive/Rworkspace/lattes_2020/lattes_tables/FormacaoDoutorado.rds') |> 
    dplyr::filter(id %in% ids_nouf) |>
    dplyr::select(nome_instituicao, codigo_instituicao, ano = ano_de_inicio, id) |>
    dplyr::mutate(grau = 'doutorado') ->
    formacao_doutorado

rbind(formacao_doutorado, formacao_mestrado, formacao_graduacao) |>
    dplyr::filter(codigo_instituicao != '') |> 
    dplyr::group_by(id) |>
    dplyr::summarise(nome_instituicao = nome_instituicao, 
                     codigo_instituicao = codigo_instituicao, 
                     grau = grau,
                     ano = max(ano, na.rm = T)) |>
    dplyr::distinct(.keep_all = T) |>
    dplyr::mutate(grau_level = ifelse(grau == 'doutorado', 1, ifelse(grau == 'mestrado', 2, 3))) |>
    dplyr::arrange(1) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(nome_instituicao = stringr::str_trim(stringi::stri_trans_general(tolower(nome_instituicao), "Latin-ASCII"))) ->
    formacao_universidade

# estados e suas siglas
import('rawfiles/sigla_estados.csv') |>
    tibble::as_tibble() ->
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
    dplyr::left_join(inst) |>
    dplyr::filter(is.na(sigla_uf)) |>
    dplyr::distinct(nome_instituicao, .keep_all = T) |>
    dplyr::select(codigo_instituicao, nome_instituicao, sigla_uf) ->
    universidade_nouf

rio::export(universidade_nouf, 'rawfiles/formacao_universidade_nouf.xlsx')

rio::import('rawfiles/formacao_universidade_nouf_v2.xlsx') |>
    tibble::as_tibble() |>
    dplyr::select(nome_instituicao, uf2 = sigla_uf) ->
    inst2

inst |>
    dplyr::mutate(uf2 = ifelse(nchar(sigla_uf) == 2, paste0('Brasil_', sigla_uf), sigla_uf)) |>
    dplyr::select(- sigla_uf) |>
    dplyr::bind_rows(inst2) |>
    dplyr::distinct(.keep_all = T) ->
    inst3

formacao_universidade |>
    left_join(inst3) |>
    dplyr::select(id, nome_instituicao, uf2) ->
    endereco2 

endereco |>
    dplyr::filter(uf2 != '') |> 
    dplyr::select(id, nome_instituicao, uf2) |>
    dplyr::bind_rows(endereco2) |>
    dplyr::distinct(.keep_all = T) ->
    endereco3

endereco3 |>
    dplyr::mutate(uf2 = ifelse(uf2 == 'Estados Unidos', 'EUA', uf2)) ->
    endereco3

rio::export(endereco3, 'rawfiles/endereco3.rds')

