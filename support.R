
# excelente tutorial
#
# https://www.rstudio.com/resources/webinars/tidy-evaluation-is-one-of-the-major-feature-of-the-latest-versions-of-dplyr-and-tidyr/

# para pacote caiporar

library(rlang)

get_keywords_tfidf <- function(data, groups, keywords, sep = ';',  n_keywords = 15) {

    group <- rlang::enquo(groups)
    DE <- rlang::enquo(keywords)

    data |>
        tibble::as_tibble() |>
        dplyr::rename(group = !!group, DE = !!DE) |> 
        dplyr::filter(!is.na(.data$group)) %>>% 
        dplyr::filter(!is.na(.data$DE)) %>>% 
        dplyr::select(.data$group, .data$DE) |>
        tidyr::separate_rows(.data$DE, sep = sep) |>
        dplyr::mutate(DE = str_trim(.data$DE)) |>
        dplyr::group_by(.data$group, .data$DE) |> 
        dplyr::tally(sort = T) |> 
        dplyr::ungroup() |>
        dplyr::arrange(.data$group, desc(.data$n)) |> 
        dplyr::mutate(DE = str_trim(.data$DE)) ->
        grupoDEfreq

    grupoDEfreq |> 
        dplyr::group_by(.data$group) |> 
        dplyr::arrange(.data$group, desc(n)) |> 
        dplyr::top_n(n_keywords) |> 
        dplyr::filter(.data$n > 1) |> 
        dplyr::mutate(keywords_freq = paste0(.data$DE, ' (', n, ')')) |> 
        dplyr::select(-.data$n) |> 
        dplyr::ungroup() ->
        keywords_freq

    grupoDEfreq |>
        dplyr::group_by(.data$group) |>
        dplyr::summarise(total = sum(.data$n)) ->
        total_DE

    left_join(grupoDEfreq, total_DE) |>
        tidytext::bind_tf_idf(.data$DE, .data$group, .data$n) ->
        tfidf

    tfidf |>
        dplyr::arrange(.data$group, desc(.data$tf_idf)) |>
        dplyr::group_by(.data$group) |> 
        dplyr::top_n(n_keywords) |> 
        dplyr::filter(.data$n > 1) |> 
        dplyr::mutate(keywords_tfidf = paste0(.data$DE, ' (', n, ')')) |> 
        dplyr::select(-.data$n) |> 
        dplyr::ungroup() |>
        dplyr::select(.data$group, .data$keywords_tfidf) ->
        tfidf_freq

    keywords_freq |> 
        dplyr::group_by(.data$group) |> 
        dplyr::summarise(keywords_freq = paste(.data$keywords_freq, collapse = ', ')) -> 
        keywords_freq2

    tfidf_freq |> 
        dplyr::group_by(.data$group) |> 
        dplyr::summarise(keywords_tfidf = paste(.data$keywords_tfidf, collapse = ', ')) -> 
        tfidf_freq2

    dplyr::full_join(keywords_freq2, tfidf_freq2) 

}

net3 |>
    tidygraph::as_tbl_graph() |>
    tidygraph::activate(nodes) |>
    get_keywords_tfidf(group, DE, sep = ';', n_keywords = 20) 

get_keywords_tfidf(net3, group, DE, sep = ';', n_keywords = 20) 



# ------------------------------
### rlang tidyverse style
# ------------------------------

# env-variables are “programming” variables that live in an environment. They are usually created with <-.

var_summary <- function(data, var) {
  data %>%
    summarise(n = n(), min = min({{ var }}), max = max({{ var }}))
}

mtcars %>% 
  group_by(cyl) %>% 
  var_summary(mpg)

mtcars %>% 
  group_by(cyl) %>% 
  var_summary('mpg')

# data-variables are “statistical” variables that live in a data frame. They usually come from data files (e.g. .csv, .xls), or are created manipulating existing variables.

for (var in names(mtcars)) {
  mtcars %>% count(.data[[var]]) %>% print()
}


for (var in names(mtcars)) {
  mtcars |> count(.data[[var]]) |> print()
}

soma_var2 <- function(df, var) {
    df |>
    summarise(soma = sum({{ var }}))
}

soma_var2(mtcars, cyl)
soma_var2(mtcars, 'cyl')


soma_var3 <- function(df, var) {
    df |>
    summarise(soma = sum(across({{ var }})))
}

soma_var3(mtcars, cyl)
soma_var3(mtcars, 'cyl')


var_summary <- function(data, var) {
  data |>
    summarise(n = n(), min = min({{ var }}), max = max({{ var }}))
}

var_summary(mtcars, cyl) 
var_summary(mtcars, 'cyl') 

summarise_mean <- function(data, vars) {
  data %>% summarise(n = n(), across({{ vars }}, mean))
}

mtcars %>% 
  group_by(cyl) %>% 
  summarise_mean(where(is.numeric))

vars <- c("mpg", "vs")
mtcars %>% select(all_of(vars))
mtcars %>% select(!all_of(vars))


# ------------------------------
### análise dos artigos
# ------------------------------

convert2df(file = 'bibs/scopus_2022-11-12.bib', dbsource = "scopus", format = "bibtex") |>
  tibble::as_tibble() |>
  dplyr::mutate(TI = stringr::str_trim(stringi::stri_trans_general(tolower(TI), "Latin-ASCII"))) |>
  dplyr::select(SR, TI) ->
  scopus

convert2df(file = 'bibs/WoS.bib', dbsource = "wos", format = "bibtex") |> 
  tibble::as_tibble() |>
  dplyr::mutate(TI = stringr::str_trim(stringi::stri_trans_general(tolower(TI), "Latin-ASCII"))) |>
  dplyr::select(SR, TI) ->
  wos

rio::import('bibs/scielo_2022-11-12.csv') |> 
  janitor::clean_names() |> 
  tibble::as_tibble() |>
  dplyr::rename(TI = title) |> 
  dplyr::mutate(TI = stringr::str_trim(stringi::stri_trans_general(tolower(TI), "Latin-ASCII"))) |>
  dplyr::select(author_s, TI) ->
  scielo


buscar <- 'Anatomical Behavior of the Celiacomesenteric Artery of Pirarucu'
buscar <- stringr::str_trim(stringi::stri_trans_general(tolower(buscar), "Latin-ASCII"))
dplyr::filter(scopus, grepl(buscar, ignore.case = T, TI)) 

# artigos não encontrados

# Identificación de parásitos en paiches "Arapaima gigas" juveniles
# http://dx.doi.org/10.17268/sci.agropecu.2017.04.02  

# Effect of pond fertilization on growth performance of pirarucu (Arapaima gigas) during grow-out phase
# http://dx.doi.org/10.3856/vol50-issue1-fulltext-2617  

# ------------------------------
### rede de colaboração entre países
# ------------------------------

library(ggraph)
library(igraph)

net3

net3 |>
  tidygraph::activate(nodes) |>
  dplyr::filter(qtde_artigos >= 3) ->
  net4

ggraph(net4, layout = 'dh') + 
  geom_edge_link() + 
  geom_node_point(aes(size = qtde_artigos, color = factor(group))) +
  theme(legend.position = 'bottom')


# Load libraries
library(igraph)
library(ggraph)

# Create a data frame for the hierarchical structure
d1 <- data.frame(from = "origin", to = paste("group", seq(1, 10), sep = ""))
d2 <- data.frame(from = rep(d1$to, each = 10), to = paste("subgroup", seq(1, 100), sep = "_"))
hierarchy <- rbind(d1, d2)

# Create a vertices data frame
vertices <- data.frame(name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))))

# Create the graph object
mygraph <- graph_from_data_frame(hierarchy, vertices = vertices)

# Visualize the hierarchical graph
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) +
  geom_edge_link() +
  theme_void()

# For a circular layout
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) +
  geom_edge_diagonal() +
  theme_void()


#
threeFieldsPlot(scientometrics, fields=c("DE","AU","CR"),n=c(20,20,20))
