#' Sumarize a venda média pelo grupo especificado
#'
#' @param df a base de vendas
#' @param nome_coluna coluna com grupos
#'  na qual vai calcular a média de valor venda
#'
#' @return dataframe com as medias de vendas agrupadas por nome_coluna
#' @export
#'
#' @examples
sumarizar_venda_media <- function(df, nome_coluna) {

# código que calcula a venda média
  df %>%
    dplyr::group_by(dplyr::across(nome_coluna))%>%
    dplyr::summarise(
      valor_venda_media = mean(venda_valor, na.rm=TRUE)
    )

}
