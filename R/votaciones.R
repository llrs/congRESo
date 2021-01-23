#' @importFrom utils as.roman
#' @importFrom methods is
#' @note Those voting approved by assent are not returned neither shown
#' @examples
#' if (interactive()){
#'     votaciones(14, "14/02/2020")
#' }
votaciones <- function(legislatura, fecha) {
    # Check legislatura
    legislatura <- utils::as.roman(legislatura)
    if (legislatura < 10) {
        stop("Sorry data not available on the website.", call. = FALSE)
    }
    # Check date input
    if (!is(fecha, "Date")) {
        fecha <- as.Date(fecha, format = "%d/%m/%Y")
    }

    if (is(fecha, "Date")) {
        fecha <- format(fecha, "%d/%m/%Y")
    }

    url <- "https://www.congreso.es/web/guest/opendata/votaciones?p_p_id=votaciones&targetLegislatura="
    url <- paste0(url, legislatura, "&targetDate=", fecha)
    xml <- xml2::read_html(x = url)
    urls <- xml2::xml_find_all(xml, "//div[@class='col-md-2 col-sm-4 col-4 text-right']/a")
    # If there isn't any vote return early
    if (length(urls) == 0) {
        return(character())
    }
    file <- paste0(BASE_URL, xml2::xml_attr(urls, "href"))
    # Download and decompress only the xml files
    td <- tempfile(fileext = ".zip")
    download.file(file, destfile = td, quiet = TRUE)
    files_zip <- unzip(td, list = TRUE)
    unzip(td, exdir = dirname(td), files = files_zip$Name[endsWith(files_zip$Name, ".xml")])
    unlink(td)
    list.files(path = dirname(td), pattern = "*.xml", full.names = TRUE)
}

parse_votacion <- function(x) {
    s <- xml2::read_xml(x)

    i <- text_children(xml2::xml_child(s, "Informacion"))
    t <- text_children(xml2::xml_child(s, "Totales"))

    l <- lapply(xml2::xml_children(xml2::xml_child(s, "Votaciones")), text_children)
    l2 <- t(simplify2array(l))
    df <- as.data.frame(l2)
    o <- c(i, t)
    o$Votos <- df

}
