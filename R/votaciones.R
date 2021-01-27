#' @importFrom utils as.roman
#' @importFrom methods is
#' @note Those voting approved by assent are not returned neither shown
#' @examples
#' if (interactive()){
#'     v <- votaciones(14, "11/02/2020")
#'     v2 <- votaciones(14, "4/02/2020") # Error
#'     v3 <- votaciones(14, "18/02/2020")
#' }
votaciones <- function(legislatura, fecha) {
    url <- website(legislatura, fecha)
    html <- xml2::read_html(x = url)
    out(html)
}


out <- function(html) {
    secciones <- xml2::xml_text(xml2::xml_find_all(html, "//h4"), trim = TRUE)
    secciones <- gsub("\\s+", " ", secciones)
    path <- "//div[@class='cuerpo-votaciones']/div[@class='accord']/div[@class='collapse c_vot']"
    sec <- xml2::xml_find_all(html, path)
    secc <- rep(secciones, time = xml2::xml_length(sec))
    votaciones <- xml2::xml_text(xml2::xml_find_all(html, "//h5"), trim = TRUE)
    exp <- xml2::xml_find_all(html, "//div/a[@href and @class='n_exp']")
    expedientes <- paste0(BASE_URL, xml2::xml_attr(exp, "href"))
    results <- xml2::xml_find_all(html, "//div[@class='result_vot']")

    o <- data.frame(seccion = secc,
               tema = votaciones)
    r <- lapply(results, result_vot)
    rr <- do.call(rbind, r)
    cbind(o, rr)
}


result_vot <- function(x) {
    # If votes were not recorded (only by assenting)
    if (xml2::xml_length(x) == 0) {
        o <- data.frame("Si" = NA, "No" = NA, "Abstenciones" = NA,
                        "Sesion"  = NA, "NumeroVotacion" = NA, "Fecha" = NA,
                        "Titulo" = NA, "TextoExpediente" = NA,
                        "TituloSubGrupo" = "", "TextoSubGrupo" = "",
                        "Asentimiento" = xml2::xml_text(x, trim = TRUE),
                        "Presentes" = NA, "AFavor" = NA, "EnContra" = NA)
        o2 <- data.frame("Abstenciones" = NA, "NoVotan" = NA, "Votos" = NA)
        return(cbind.data.frame(o, o2))
    }

    l <- strsplit(xml2::xml_text(xml2::xml_find_all(x, "./p"), trim = TRUE),
                  split = ": ", fixed = TRUE)
    m <- simplify2array(l)
    colnames(m) <- m[1, ]
    m <- m[-1, , drop = FALSE]
    m <- as.data.frame(m)

    files <- xml2::xml_attr(xml2::xml_find_all(x, "./a"), "href")
    url <- paste0(BASE_URL, files[endsWith(files, ".xml")])
    y <- xml2::read_xml(url)
    y <- parse_votacion(y)
    cbind(m, y)

}
website <- function(legislatura, fecha) {
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
    paste0(url, legislatura, "&targetDate=", fecha)
}


zip_xml <- function(xml) {
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

# Perhaps should export it as the xml file is available in multiple sites
parse_votacion <- function(x) {
    i <- text_children(xml2::xml_child(x, "Informacion"))
    t <- text_children(xml2::xml_child(x, "Totales"))

    l <- lapply(xml2::xml_children(xml2::xml_child(x, "Votaciones")), text_children)
    l2 <- t(simplify2array(l))
    df <- as.data.frame(l2)
    o <- c(i, t)
    o <- data.frame(as.list(o))
    o$Votos[[1]] <- df
    o
}
