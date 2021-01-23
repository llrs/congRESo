#' @param format File format, either csv, json or xml (Default: csv)
#' @return A named character with the urls of the datsets on the format requested.
#' @importFrom xml2 read_html xml_find_all xml_text xml_attrs
diputados <- function(format = "csv") {
    parser("diputados", format)
}

