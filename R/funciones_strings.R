# Limpiar nombres----
#' Te devuelve el input inicial sin tildes y sin espacios (y, en su caso, en minúsculas o mayúsculas). Basada en janitor::make_clean_names.
#' @param .x Input.
#' @param case 	The desired target case (default is `"snake"`) will be passed to `snakecase::to_any_case()` with the exception of "old_janitor", which exists only to support legacy code (it preserves the behavior of `clean_names()` prior to addition of the "case" argument (janitor versions <= 0.3.1). "old_janitor" is not intended for new code. See `to_any_case` for a wide variety of supported cases, including "sentence" and "title" case.
#' @param replace A named character vector where the name is replaced by the value.
#' @param ascii Convert the names to ASCII (`TRUE`, default) or not (`FALSE`).
#' @param use_make_names Should `make.names()` be applied to ensure that the output is usable as a name without quoting? (Avoiding `make.names()` ensures that the output is locale-independent but quoting may be required.)
#' @param allow_dupes Allow duplicates in the returned names (`TRUE`) or not (`FALSE`, the default).
#' @param sep_in (short for separator input) if character, is interpreted as a regular expression (wrapped internally into `stringr::regex()`). The default value is a regular expression that matches any sequence of non-alphanumeric values. All matches will be replaced by underscores (additionally to "_" and " ", for which this is always true, even if `NULL` is supplied). These underscores are used internally to split the strings into substrings and specify the word boundaries.
#' @param transliterations A character vector (if not `NULL`). The entries of this argument need to be elements of `stringi::stri_trans_list()` (like "Latin-ASCII", which is often useful) or names of lookup tables (currently only `"german"` is supported). In the order of the entries the letters of the input string will be transliterated via `stringi::stri_trans_general()` or replaced via the matches of the lookup table. When named character elements are supplied as part of 'transliterations', anything that matches the names is replaced by the corresponding value. You should use this feature with care in case of `case = "parsed"`, `case = "internal_parsing"` and `case = "none"`, since for upper case letters, which have transliterations/replacements of length 2, the second letter will be transliterated to lowercase, for example Oe, Ae, Ss, which might not always be what is intended. In this case you can make usage of the option to supply named elements and specify the transliterations yourself.
#' @param parsing_option An integer that will determine the parsing_option.
#' 1: `"RRRStudio"` -> `"RRR_Studio"`
#' 2: `"RRRStudio"` -> `"RRRS_tudio"`
#' 3: `"RRRStudio"` -> `"RRRSStudio"`. This will become for example `"Rrrstudio"` when we convert to lower camel case.
#' -1, -2, -3: These `parsing_options`'s will suppress the conversion after non-alphanumeric values.
#' 0: no parsing
#' @param numerals A character specifying the alignment of numerals (`"middle"`, `"left"`, `"right"`, `"asis"` or `"tight"`). I.e. `numerals = "left"` ensures that no output separator is in front of a digit.
#' @param ... Arguments passed on to `snakecase::to_any_case`.
#' @export
limpiar_nombres <- function(.x,
                            case = "snake",
                            replace = c(`'` = "", `"` = "", `%` = "_percent_", `#` = "_number_"),
                            ascii = TRUE,
                            use_make_names = TRUE,
                            allow_dupes = FALSE,
                            sep_in = "\\.",
                            transliterations = "Latin-ASCII",
                            parsing_option = 1,
                            numerals = "asis",
                            ...) {

  if (case == "old_janitor") {
    return(old_make_clean_names(.x))
  }
  janitor:::warn_micro_mu(string = .x, replace = replace)
  replaced_names <- stringr::str_replace_all(string = .x,
                                             pattern = replace)
  transliterated_names <-
    if (ascii) {
      stringi::stri_trans_general(replaced_names, id = janitor:::available_transliterators(c("Any-Latin", "Greek-Latin", "Any-NFKD", "Any-NFC", "Latin-ASCII")))
      } else {
        replaced_names
        }
  good_start <- stringr::str_replace(string = transliterated_names,
                                     pattern = "\\A[\\h\\s\\p{Punctuation}\\p{Symbol}\\p{Separator}\\p{Other}]*(.*)$",
                                     replacement = "\\1")
  cleaned_within <- stringr::str_replace(string = good_start,
                                         pattern = "[\\h\\s\\p{Punctuation}\\p{Symbol}\\p{Separator}\\p{Other}]+",
                                         replacement = ".")
  made_names <- if (use_make_names) {
    make.names(cleaned_within)
  }
  else {
    cleaned_within
  }
  cased_names <- snakecase::to_any_case(made_names, case = case,
                                        sep_in = sep_in, transliterations = transliterations,
                                        parsing_option = parsing_option, numerals = numerals,
                                        ...)
  if (!allow_dupes) {
    while (any(duplicated(cased_names))) {
      dupe_count <- vapply(seq_along(cased_names), function(i) {
        sum(cased_names[i] == cased_names[1:i])
      }, 1L)
      cased_names[dupe_count > 1] <- paste(cased_names[dupe_count >
                                                         1], dupe_count[dupe_count > 1], sep = "_")
    }
  }
  cased_names
}


# Sin tildes----
#' Te devuelve el input inicial sin tildes (y, en su caso, en minúsculas o mayúsculas).
#' @param .x Input.
#' @param tolower Valor lógico. Si se establece en TRUE te convierte el input en minúsculas. Por defecto `FALSE`.
#' @param toupper Valor lógico. Si se establece en TRUE te convierte el input en mayúsculas. Por defecto `FALSE`.
#' @param quitar_espacios Valor lógico. Si se establece en TRUE te sustituye espacios y guiones por barras bajas. Por defecto `FALSE`.
#' @export
quitar_tildes <- function(.x, tolower = FALSE, toupper = FALSE, quitar_espacios = FALSE) {
  .x <- gsub("á", "a", .x)
  .x <- gsub("é", "e", .x)
  .x <- gsub("í", "i", .x)
  .x <- gsub("ó", "o", .x)
  .x <- gsub("ú", "u", .x)
  .x <- gsub("Á", "A", .x)
  .x <- gsub("É", "E", .x)
  .x <- gsub("Í", "I", .x)
  .x <- gsub("Ó", "O", .x)
  .x <- gsub("Ú", "U", .x)
  .x <- gsub("à", "a", .x)
  .x <- gsub("è", "e", .x)
  .x <- gsub("ì", "i", .x)
  .x <- gsub("ò", "o", .x)
  .x <- gsub("ù", "u", .x)
  .x <- gsub("À", "A", .x)
  .x <- gsub("È", "E", .x)
  .x <- gsub("Ì", "I", .x)
  .x <- gsub("Ò", "O", .x)
  .x <- gsub("Ù", "U", .x)

  .x <- gsub("ñ", "n", .x)
  .x <- gsub("Ñ", "N", .x)

  if (tolower == TRUE){
    .x <-
      .x |>
      tolower()
  }

  if (toupper == TRUE){
    .x <-
      .x |>
      toupper()
  }

  if (quitar_espacios == TRUE){
    .x <- .x |>
      stringr::str_replace_all(" ", "_") |>
      stringr::str_replace_all("-", "_")
  }

  return(.x)
}
