#' Retorna numero de NAs / proporção NAs numa coluna
#'
#' @param df dataframe de vendas
#' @param nome_coluna nome da coluna especificada
#' @param prop TRUE caso queira proporção senão qtdade linhas
#'
#' @return
#' @export
#'
#' @examples
pegar_num_nas <- function(df, nome_coluna, prop = FALSE) {

# código que retorna o número de NAs da coluna "nome_coluna"
  prop_false = df %>%
    dplyr::summarize(
      dplyr::across(nome_coluna,
             ~ sum(is.na(.x), na.rm = TRUE))) %>%
    dplyr::pull()

  prop_true = df %>%
    dplyr::summarize(
      dplyr::across(nome_coluna,
             ~ sum(is.na(.x), na.rm = TRUE)/dplyr::n())) %>%
    dplyr::pull()

  ifelse(prop == TRUE, prop_true, prop_false)

}
