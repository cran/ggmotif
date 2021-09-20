#' @name getMotifFromTxt
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Extract and Visualize Motif Information from MEME Software
#' @description
#' \code{getMotifFromXML} Extract motif information from the MEME software results, .txt file.
#'
#' @param data A txt file from MEME software.
#' 
#' @importFrom magrittr %>%
#' @importFrom utils read.table 
#' @importFrom stringr str_sub str_split
#'
#' @examples
#' filepath <- system.file("examples", "meme.txt", package = "ggmotif")
#' motif_extract <- getMotifFromTxt(data = filepath)
#' @export
#'
#' @return Return a vector or a datafram
utils::globalVariables(c(
  "V1","five","end.sym","row.num","nchar.1","nchar.2"
))

getMotifFromTxt <- function(data) {

  df <- read.table(data, sep = ",") %>%
    dplyr::rename(raw = V1)

  df.temp <- df %>%
    dplyr::mutate(
      row.num = rownames(.),
      nchar.1 = nchar(raw),
      nchar.2 = 16,
      start = nchar.1 - nchar.2,
      end = nchar.1
    ) %>%
    dplyr::mutate(
      five = stringr::str_sub(raw, start, end),
      end.sym = stringr::str_sub(raw, 1, 2)
    ) %>%
    dplyr::filter(five == " in BLOCKS format" | end.sym == "//") %>%
    dplyr::mutate(row.num = ifelse(raw == "//", as.numeric(row.num) - 1, as.numeric(row.num) + 3)) %>%
    dplyr::select(1, 2)

  df.motif <- NULL

  for (i in seq(1, nrow(df.temp), 2)) {
    start.row <- df.temp$row.num[i]

    end.row <- df.temp$row.num[i + 1]

    df.motif.temp <- df[start.row:end.row, ] %>%
      as.data.frame()

    colnames(df.motif.temp) <- "raw"

    df.motif.temp <- df.motif.temp %>%
      dplyr::mutate(
        motif.num = paste0("Motif.", (i + 1) / 2),
        input.seq.name = "",
        input.seq.motif = ""
      )

    for (j in 1:nrow(df.motif.temp)) {
      df.motif.temp$input.seq.name[j] <- stringr::str_split(df.motif.temp$raw[j], " ")[[1]][1]
      df.motif.temp$input.seq.motif[j] <- stringr::str_split(df.motif.temp$raw[j], " ")[[1]][(length(stringr::str_split(df.motif.temp$raw[j], " ")[[1]]) - 3)]
    }

    df.motif <- rbind(df.motif, df.motif.temp)
  }

  return(df.motif)
}
