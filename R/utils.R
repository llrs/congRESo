BASE_URL <- "https://www.congreso.es"

#' @param format File format, either csv, json or xml (Default: csv)
#' @return A named character with the urls of the datsets on the format requested.
#' @importFrom xml2 read_html xml_find_all xml_text xml_attrs
parser <- function(site, format = "csv") {
    format <- match.arg(format, c("csv", "json", "xml"))
    format <- paste0(".", format)
    url <- "https://www.congreso.es/web/guest/opendata/"

    url <- paste0(url, site)
    xml <- xml2::read_html(x = url)
    urls <- xml2::xml_find_all(xml, "//p[@class='point-list opendata']")
    txt <- xml2::xml_text(urls, trim = TRUE)
    data <- gsub("\\s{2,}.*", "", txt)
    files <- xml2::xml_attr(xml2::xml_children(urls), "href")
    datasets <- paste0(BASE_URL, files[endsWith(files, format)])
    names(datasets) <- data
    datasets
}

#' @examples
#' dp <- diputados()
#' head(read_diputados(dp[1]))
parser_files <- function(x) {
    stopifnot(endsWith(x, ".csv"))
    encoding <- "windows-1258"
    o <- utils::read.csv2(x, encoding =  encoding, fileEncoding = encoding)
    fechas <- startsWith(colnames(o), "FECHA")
    o[, fechas] <- lapply(o[ , fechas], function(y) {
        as.Date(y, format = "%d/%m/%Y")
    })
    o
}

text_children <- function(x) {
    names <- xml2::xml_name(xml2::xml_children(x))
    values <- xml2::xml_text(xml2::xml_children(x), trim = TRUE)
    names(values) <- names
    values
}
