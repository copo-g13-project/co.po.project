# =============================================================================
#  co-po project  |  Enterprise Document Processing  |  PURE R (no Python)
#  VERSION 5  -  ANY DOCUMENT (png/jpg/pdf/txt/csv...) + FILE MENU + CSV OUTPUT
# -----------------------------------------------------------------------------
#  What it does
#    1. Scans the  uploads/  folder and shows a numbered list of all files.
#    2. Asks which file(s) to process      ->  1   |   1,3   |   2-4   |   all
#    3. Asks the processing mode
#          [1] GENERIC  - works for ANY document. Reads text (OCR for images and
#                         scanned PDFs, text layer for normal PDFs, direct read for
#                         txt/csv/tsv), detects TABLES automatically and writes
#                         clean CSV files.
#          [2] STUDENT  - your original 14-column / 30-row student-table profile
#                         (ID, LastName, FirstName, City, ... NewspaperReadership)
#    4. Writes NEW timestamped CSV files into  output/  (nothing is overwritten).
#
#  Required packages (install once):
#     install.packages(c("magick", "tesseract", "pdftools",
#                        "dplyr", "stringr", "tibble"))
#
#  Run in VS Code terminal  (project folder must contain uploads/):
#     Rscript ocr_processing.R
#  Run inside an R terminal / RStudio:
#     source("ocr_processing.R")
#
#  Optional switches (skip the questions):
#     --all                 process every file
#     --files=a.png,b.pdf   process only these names
#     --mode=generic|student
#     --merge | --separate  one combined CSV  or  one CSV set per file
#     --lang=eng            OCR language, e.g. --lang=eng+mar+hin  (Marathi/Hindi)
#     --passes=3            max OCR passes per page (1-6, more = slower, better)
#     --rows=30             student mode: expected number of rows
# =============================================================================


# ----------------------------- 1. CONFIGURATION ------------------------------

UPLOAD_DIR_NAME <- "uploads"
OUTPUT_DIR_NAME <- "output"

TARGET_COLS <- c("ID", "LastName", "FirstName", "City", "State", "Gender",
                 "StudentStatus", "Major", "Country", "Age", "SAT",
                 "AverageScore", "Height", "NewspaperReadership")
N_COLS <- length(TARGET_COLS)
N_TAIL <- 5L

NUMERIC_COLS <- c("ID", "Age", "SAT", "AverageScore", "Height",
                  "NewspaperReadership")
TAIL_COLS    <- NUMERIC_COLS[-1]

EXPECTED_ROWS <- 30L
SPLIT_REGEX   <- "\\s{2,}|\\t"

AGE_RANGE <- c(10, 99)
SAT_RANGE <- c(600, 2400)

TAIL_FIT <- list(Age = c(10, 99), SAT = c(100, 2400), AverageScore = c(0, 100),
                 Height = c(30, 250), NewspaperReadership = c(0, 100))

OCR_TARGET_WIDTH <- 3000L
IMAGE_EXT     <- c("png", "jpg", "jpeg", "tif", "tiff", "bmp", "webp", "gif")
TEXT_EXT      <- c("txt", "csv", "tsv")
SUPPORTED_EXT <- c(IMAGE_EXT, "pdf", TEXT_EXT)

OCR_PASSES <- list(
  list(name = "gray-psm6",   prep = "gray",   psm = "6"),
  list(name = "lat-psm6",    prep = "lat",    psm = "6"),
  list(name = "gray-psm4",   prep = "gray",   psm = "4"),
  list(name = "deskew-psm6", prep = "deskew", psm = "6"),
  list(name = "big-psm6",    prep = "big",    psm = "6"),
  list(name = "lat-psm4",    prep = "lat",    psm = "4"))

STATE_NAMES <- c(state.name, "District of Columbia")
STATE_CODES <- c(state.abb, "DC")

STATUS_LEX   <- c("Undergraduate", "Graduate", "Undergrad", "Grad")
STATUS_CANON <- c(Undergraduate = "Undergraduate", Graduate = "Graduate",
                  Undergrad = "Undergraduate", Grad = "Graduate")

MAJOR_LEX   <- c("Math", "Econ", "Politics")
GENDER_LEX  <- c("Male", "Female")
COUNTRY_LEX <- c("USA", "US", "United States", "United States of America",
                 "Canada", "Mexico", "India", "China", "Japan", "Brazil",
                 "Germany", "France", "United Kingdom", "UK", "South Korea",
                 "Nigeria", "Kenya", "Australia", "New Zealand", "Pakistan",
                 "Bangladesh", "Nepal", "Vietnam", "Egypt", "Spain", "Italy",
                 "Russia", "Ghana", "Colombia", "Peru", "Argentina", "Chile",
                 "Turkey", "Iran", "Saudi Arabia", "Sri Lanka", "Philippines",
                 "Indonesia", "Thailand", "Singapore", "Malaysia", "Taiwan",
                 "Hong Kong", "Ireland", "Sweden", "Norway", "Netherlands",
                 "Poland", "Greece", "Ethiopia", "South Africa", "Jamaica",
                 "Costa Rica", "Puerto Rico", "El Salvador")

# ---- Regex building blocks ---------------------------------------------------
RX_DIGITS <- "[OIl|!]?[0-9][0-9OIl|!]{0,2}"
RX_ID     <- sprintf("(?:%s|[Il|!])", RX_DIGITS)
RX_LAST   <- sprintf("D[O0Q]E ?%s", RX_DIGITS)
RX_FIRST  <- sprintf("(?:J[A4]N[E3]|J[O0Q][E3]) ?%s", RX_DIGITS)
rx_word <- function(alts) sprintf("(?<![A-Za-z])(?:%s)(?![A-Za-z])", alts)
RX_GENDER <- rx_word("Female|Fernale|Femaie|Fema1e|Male|Maie|Ma1e")
RX_STATUS <- rx_word(paste0("Undergraduate|Undergraduete|Undergraduaie|Undergrad|",
                            "Graduate|Graduete|Graduaie|Grad"))
RX_MAJOR  <- rx_word("Math|Mafh|Maih|Econ|Ec0n|Econ0|Politics|Po1itics|Pol1tics|Politlcs")
RX_NUMTOK <- "[0-9OoDQlI|!SsBZEgq][0-9OoDQlI|!SsBZEgq.,]{0,6}"


# ------------------------------ 2. UTILITIES ---------------------------------

log_msg <- function(level = "INFO", ...) {
  if (isTRUE(getOption("copo.quiet")) && level != "ERROR") return(invisible(NULL))
  cat(sprintf("[%s] [%-5s] %s\n",
              format(Sys.time(), "%H:%M:%S"), level, paste0(...)))
}

with_quiet <- function(expr) {
  old <- options(copo.quiet = TRUE); on.exit(options(old), add = TRUE)
  force(expr)
}

check_dependencies <- function() {
  pkgs    <- c("magick", "tesseract", "pdftools", "dplyr", "stringr", "tibble")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(sprintf("Missing R package(s): %s\nInstall with:\n  install.packages(c(%s))",
                 paste(missing, collapse = ", "),
                 paste(sprintf('"%s"', missing), collapse = ", ")),
         call. = FALSE)
  }
  suppressPackageStartupMessages(
    invisible(lapply(c("dplyr", "stringr", "tibble"), library,
                     character.only = TRUE)))
  invisible(TRUE)
}

get_project_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f    <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f) > 0) return(normalizePath(dirname(f[1]), mustWork = FALSE))
  for (i in rev(seq_len(sys.nframe()))) {          # source("ocr_processing.R")
    of <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (is.character(of) && length(of) == 1L) return(normalizePath(dirname(of), mustWork = FALSE))
  }
  getwd()
}

empty_frame <- function() {
  tibble::as_tibble(
    setNames(replicate(N_COLS, character(0), simplify = FALSE), TARGET_COLS))
}

rng <- function(a, b) if (a <= b) seq.int(a, b) else integer(0)

nz <- function(x) if (length(x) == 0L || is.na(x[1]) || !nzchar(x[1])) NA_character_ else x[1]


# ------------------------ 3. FUZZY LEXICON MATCHING ---------------------------

fuzzy_match <- function(x, lexicon, max_dist) {
  if (length(x) == 0L || is.na(x) || !nzchar(x)) return(NA_character_)
  d <- drop(utils::adist(x, lexicon, ignore.case = TRUE))
  i <- which.min(d)
  if (d[i] <= max_dist) lexicon[i] else NA_character_
}

allow_dist <- function(n, cap = 2L) min(cap, n %/% 4L)

lex_match <- function(words, lexicon, cap = 2L) {
  p <- stringr::str_squish(paste(words, collapse = " "))
  fuzzy_match(p, lexicon, allow_dist(nchar(p), cap))
}

match_gender <- function(x) {
  w <- stringr::str_remove_all(x, "[^A-Za-z]")
  if (!nzchar(w)) return(NA_character_)
  if (nchar(w) == 1L) return(if (toupper(w) %in% c("M", "F")) toupper(w) else NA_character_)
  fuzzy_match(w, GENDER_LEX, if (nchar(w) >= 5L) 2L else 1L)
}

match_status <- function(x) {
  w <- stringr::str_remove_all(x, "[^A-Za-z]")
  if (nchar(w) < 4L) return(NA_character_)
  m <- fuzzy_match(w, STATUS_LEX, allow_dist(nchar(w), 3L))
  if (is.na(m)) NA_character_ else unname(STATUS_CANON[m])
}

match_state <- function(words) {
  p <- stringr::str_squish(stringr::str_remove_all(paste(words, collapse = " "), "[^A-Za-z ]"))
  if (!nzchar(p)) return(NA_character_)
  if (nchar(p) == 2L) return(if (toupper(p) %in% STATE_CODES) toupper(p) else NA_character_)
  fuzzy_match(p, STATE_NAMES, allow_dist(nchar(p), 2L))
}


# ------------------------ 4. OCR CHARACTER-REPAIR MODULE ----------------------

repair_numeric_token <- function(x) {
  x <- stringr::str_trim(x)
  x <- stringr::str_remove_all(x, "[\u2018\u2019'`\"\u00B4]")
  x <- stringr::str_replace(x, "^[Ee](?=[0-9lI|!])", "9")
  x <- stringr::str_replace_all(x, c("[OoDQ]"  = "0",
                                     "[lI|!]"  = "1",
                                     "[Ss]"    = "5",
                                     "B"       = "8",
                                     "Z"       = "2",
                                     "[gq]"    = "9",
                                     ","       = "."))
  x <- stringr::str_remove_all(x, "[^0-9.]")
  x <- stringr::str_replace(x, "^([^.]*\\.[^.]*)\\..*$", "\\1")
  dplyr::na_if(x, "")
}

repair_id_token <- function(x) {
  dplyr::na_if(stringr::str_remove_all(repair_numeric_token(x), "[^0-9]"), "")
}

is_numeric_ish <- function(x) {
  cls <- "^[0-9OoDQlI|!SsBZEgq.,]{1,7}$"
  ((stringr::str_detect(x, "[0-9]") & stringr::str_detect(x, cls)) |
     stringr::str_detect(x, "^(E[lI|!1]|[lI|!]{1,2}|[Oo])$")) %in% TRUE
}

is_id_token <- function(x) nchar(x) <= 3L && isTRUE(is_numeric_ish(x))

repair_by_range <- function(v, lo, hi, allow_prefix = FALSE, label = "") {
  num <- suppressWarnings(as.numeric(v))
  ok  <- !is.na(num) & num >= lo & num <= hi
  if (!any(ok)) return(v)
  med <- stats::median(num[ok])
  for (i in which(!ok & !is.na(num))) {
    s     <- stringr::str_remove(as.character(v[i]), "\\..*$")
    cands <- character(0)
    if (nchar(s) > 1L) {
      cands <- c(cands, vapply(seq_len(nchar(s)), function(p)
        paste0(substr(s, 1L, p - 1L), substr(s, p + 1L, nchar(s))), character(1)))
    }
    if (allow_prefix) cands <- c(cands, paste0(1:9, s))
    cn <- suppressWarnings(as.numeric(cands))
    cn <- unique(cn[!is.na(cn) & cn >= lo & cn <= hi])
    if (length(cn) > 0L) {
      fixed <- cn[which.min(abs(cn - med))]
      log_msg("FIX", sprintf("%s: %s -> %s (range repair, column median %s)",
                             label, v[i], fixed, med))
      v[i] <- as.character(fixed)
    }
  }
  v
}

restore_sat <- function(v) {
  num  <- suppressWarnings(as.numeric(v))
  four <- num[!is.na(v) & nchar(v) == 4L & !is.na(num)]
  lo   <- if (length(four) > 0L && stats::median(four) >= 1000) 1000 else SAT_RANGE[1]
  repair_by_range(v, lo, SAT_RANGE[2], allow_prefix = TRUE, label = "SAT")
}

clean_text <- function(x) {
  x <- stringr::str_replace_all(x, "[\u00A0\u200B]", " ")
  x <- stringr::str_remove_all(x, "[_~=`\\\\\"^*]")
  x <- stringr::str_replace_all(x, "(?<=\\p{Lu})0(?=\\p{Lu}|$)", "O")
  x <- stringr::str_replace_all(x, "(?<=\\p{L})0(?=\\p{L}|$)", "o")
  x <- stringr::str_replace_all(x, "^0(?=\\p{Ll})", "O")
  x <- stringr::str_replace_all(x, "(?<=\\p{Ll})1(?=\\p{Ll})", "l")
  x <- stringr::str_replace_all(x, "(?<=\\p{L})[|](?=\\p{L})", "l")
  x <- stringr::str_replace_all(x, "(?<=\\p{Ll})5(?=\\p{Ll})", "s")
  dplyr::na_if(stringr::str_squish(x), "")
}

canon_lex <- function(x, lex, cap = 2L) {
  x <- clean_text(x)
  vapply(seq_along(x), function(i) {
    if (is.na(x[i])) return(NA_character_)
    m <- fuzzy_match(x[i], lex, allow_dist(nchar(x[i]), cap))
    if (is.na(m)) x[i] else m
  }, character(1))
}

canon_last <- function(x) {
  u    <- stringr::str_to_upper(stringr::str_squish(x))
  dist <- drop(utils::adist(stringr::str_sub(u, 1L, 3L), "DOE"))
  dd   <- repair_id_token(stringr::str_trim(stringr::str_sub(u, 4L)))
  ifelse(!is.na(dist) & dist <= 1 & !is.na(dd), paste0("DOE", dd), clean_text(x))
}

canon_first <- function(x) {
  u    <- stringr::str_to_upper(stringr::str_squish(x))
  d4   <- drop(utils::adist(stringr::str_sub(u, 1L, 4L), "JANE"))
  d3   <- drop(utils::adist(stringr::str_sub(u, 1L, 3L), "JOE"))
  stem <- ifelse(!is.na(d4) & d4 <= 1, "JANE", ifelse(!is.na(d3) & d3 <= 1, "JOE", NA_character_))
  rest <- ifelse(stem %in% "JANE", stringr::str_sub(u, 5L), stringr::str_sub(u, 4L))
  dd   <- repair_id_token(stringr::str_trim(rest))
  ifelse(!is.na(stem) & !is.na(dd), paste0(stem, dd), clean_text(x))
}

canon_gender <- function(x) {
  g <- vapply(clean_text(x), function(w) if (is.na(w)) NA_character_ else match_gender(w),
              character(1), USE.NAMES = FALSE)
  ifelse(is.na(g), clean_text(x),
         ifelse(g == "M", "Male", ifelse(g == "F", "Female", g)))
}

canon_status <- function(x) {
  s <- vapply(clean_text(x), function(w) if (is.na(w)) NA_character_ else match_status(w),
              character(1), USE.NAMES = FALSE)
  ifelse(is.na(s), clean_text(x), s)
}

clean_state <- function(x) {
  x <- clean_text(x)
  ifelse(!is.na(x) & nchar(x) == 2L, stringr::str_to_upper(x), x)
}

clean_dataframe <- function(df) {
  df <- df %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(TAIL_COLS), repair_numeric_token),
      LastName      = canon_last(LastName),
      FirstName     = canon_first(FirstName),
      City          = clean_text(City),
      State         = clean_state(State),
      Gender        = canon_gender(Gender),
      StudentStatus = canon_status(StudentStatus),
      Major         = canon_lex(Major, MAJOR_LEX),
      Country       = canon_lex(Country, COUNTRY_LEX, cap = 1L))

  df$SAT <- restore_sat(df$SAT)
  df$Age <- repair_by_range(df$Age, AGE_RANGE[1], AGE_RANGE[2], label = "Age")

  df %>%
    dplyr::mutate(
      ID = suppressWarnings(as.integer(ID)),
      dplyr::across(dplyr::all_of(TAIL_COLS), ~ suppressWarnings(as.numeric(.x))))
}


# ------------------------ 5. ID RECONSTRUCTION --------------------------------

reconcile_ids <- function(ids, max_id = EXPECTED_ROWS, hints = rep(NA_integer_, length(ids))) {
  n   <- length(ids)
  num <- suppressWarnings(as.integer(ids))
  cand_ok <- !is.na(num) & num >= 1L & num <= max_id & !duplicated(num)

  trusted <- rep(FALSE, n)
  idx <- which(cand_ok)
  if (length(idx) > 0L) {
    vals <- num[idx]; m <- length(idx)
    len  <- rep(1L, m); prev <- rep(0L, m)
    for (a in seq_len(m)) for (b in seq_len(a - 1L)) {
      if (vals[b] < vals[a] && len[b] + 1L > len[a]) { len[a] <- len[b] + 1L; prev[a] <- b }
    }
    cur <- which.max(len)
    while (cur > 0L) { trusted[idx[cur]] <- TRUE; cur <- prev[cur] }
  }

  out  <- ifelse(trusted, num, NA_integer_)
  used <- out[!is.na(out)]
  for (i in which(!trusted)) {
    before <- out[seq_len(i - 1L)]; before <- before[!is.na(before)]
    after  <- if (i < n) out[(i + 1L):n] else integer(0); after <- after[!is.na(after)]
    lo <- if (length(before)) max(before) else 0L
    hi <- if (length(after))  min(after)  else max_id + 1L
    free <- setdiff(rng(lo + 1L, hi - 1L), used)
    if (length(free) > 0L) {
      pick <- if (!is.na(hints[i]) && hints[i] %in% free) hints[i] else free[1]
      log_msg("FIX", sprintf("ID '%s' reconstructed as %d (from row order)",
                             ifelse(is.na(num[i]), "unreadable", num[i]), pick))
      out[i] <- pick; used <- c(used, pick)
    }
  }
  out
}


# ------------------- 6. OCR NOISE FILTER (line start) -------------------------

strip_line_noise <- function(line) {
  x <- stringr::str_replace_all(line, "[\u00A0\u200B\u2009\uFEFF]", " ")

  lead_rx <- paste0("^\\s*(?:\\[all|\\[al|[A-Za-z]\\||",
                    "[|\u00A6\\[\\]{}()<>_~=:;,.'\"`\u00B4\\\\/\u2014\u2013-])+\\s*")
  repeat {
    y <- stringr::str_remove(x, stringr::regex(lead_rx, ignore_case = TRUE))
    if (identical(y, x)) break
    x <- y
  }
  x <- stringr::str_replace_all(x, "(?<=\\s)[|\u00A6](?=\\s)", " ")
  x <- stringr::str_replace_all(x, "(?<=\\s)[^\\p{L}\\p{N}\\s]+(?=\\s|$)", " ")
  x <- stringr::str_remove(x, "[\\s|\u00A6\\]\\[)}>_~]+$")
  stringr::str_squish(x)
}


# ------------------- 7. REGEX ROW PARSER (tier 1) + tiers 2/3 -----------------

build_row_regex <- function(n_tail = N_TAIL) {
  tail_rx <- sprintf("((?:%s\\s+){%d}%s)", RX_NUMTOK, n_tail - 1L, RX_NUMTOK)
  paste0(
    "^\\s*",
    "(?:(", RX_ID, ")[\\s.:)\\]-]*)?",
    "(", RX_LAST, ")\\s*",
    "(", RX_FIRST, ")\\s+",
    "(.+?)\\s+",
    "(", RX_GENDER, ")\\s+",
    "(", RX_STATUS, ")\\s+",
    "(", RX_MAJOR, ")\\s+",
    "(?:(.+?)\\s+)?",
    tail_rx,
    "\\s*$")
}

split_city_state <- function(cs) {
  w <- stringr::str_split(stringr::str_squish(cs), " ")[[1]]
  L <- length(w)
  for (len in rev(seq_len(min(3L, L)))) {
    hit <- match_state(w[(L - len + 1L):L])
    if (!is.na(hit)) {
      city <- w[rng(1L, L - len)]
      return(c(nz(if (length(city)) paste(city, collapse = " ") else NA), hit))
    }
  }
  if (L >= 2L) {
    log_msg("WARN", sprintf("State not recognised in '%s' -> last word taken as State.", cs))
    return(c(paste(w[1:(L - 1L)], collapse = " "), w[L]))
  }
  c(nz(cs), NA_character_)
}

assign_tail <- function(tok) {
  out <- rep(NA_character_, N_TAIL); j <- 1L
  for (t in tok) {
    v   <- suppressWarnings(as.numeric(repair_numeric_token(t)))
    hit <- NA_integer_
    for (c in rng(j, N_TAIL)) {
      r <- TAIL_FIT[[c]]
      if (!is.na(v) && v >= r[1] && v <= r[2]) { hit <- c; break }
    }
    if (is.na(hit)) hit <- min(j, N_TAIL)
    out[hit] <- t; j <- hit + 1L
    if (j > N_TAIL) break
  }
  out
}

parse_row_regex <- function(line) {
  for (n in N_TAIL:3L) {
    m <- stringr::str_match(line, stringr::regex(build_row_regex(n), ignore_case = TRUE))
    if (!is.na(m[1, 1])) {
      cs   <- split_city_state(m[1, 5])
      tail <- stringr::str_split(stringr::str_squish(m[1, 10]), " ")[[1]]
      if (n < N_TAIL) log_msg("WARN", sprintf("Only %d trailing numbers found: %s", n, substr(line, 1L, 70L)))
      tail5 <- if (length(tail) == N_TAIL) tail else assign_tail(tail)
      row <- c(nz(repair_id_token(m[1, 2])), nz(m[1, 3]), nz(m[1, 4]), cs[1], cs[2],
               nz(m[1, 6]), nz(m[1, 7]), nz(m[1, 8]), nz(m[1, 9]), tail5)
      if (length(row) != N_COLS) stop("regex row has ", length(row), " fields")
      return(row)
    }
  }
  NULL
}

is_header_line <- function(line) {
  stringr::str_detect(line,
    stringr::regex("last\\s*name|first\\s*name|student\\s*status|newspaper",
                   ignore_case = TRUE)) ||
    stringr::str_detect(line, "^\\s*ID\\b\\s")
}

tokenize_line <- function(line) {
  tk <- stringr::str_split(stringr::str_trim(line), "[\\s\\t]+")[[1]]
  tk <- tk[nzchar(tk)]
  if (length(tk) > 0L && grepl("^[0-9][0-9|!]?[A-Za-z]", tk[1])) {
    m <- stringr::str_match(tk[1], "^([0-9][0-9|!]?)([A-Za-z][A-Za-z'.-]*)$")
    if (!is.na(m[1, 1])) tk <- c(m[1, 2], m[1, 3], tk[-1])
  }
  tk
}

locate_anchor <- function(tokens, fn, gen, sta) {
  n    <- length(tokens)
  cand <- rng(fn + 2L, n - 1L)
  if (length(cand) > 0L) {
    pair <- cand[!is.na(gen[cand]) & !is.na(sta[cand + 1L])]
    if (length(pair) > 0L) return(pair[1])
  }
  only_sta <- which(!is.na(sta) & seq_len(n) >= fn + 3L)
  if (length(only_sta) > 0L) return(only_sta[1] - 1L)
  only_gen <- cand[!is.na(gen[cand])]
  if (length(only_gen) > 0L) return(only_gen[1])
  NA_integer_
}

split_major_country <- function(mid) {
  L <- length(mid)
  if (L == 0L) return(c(NA_character_, NA_character_))
  m_all <- lex_match(mid, MAJOR_LEX)
  if (!is.na(m_all)) return(c(m_all, NA_character_))
  for (len in rev(seq_len(min(2L, L)))) {
    c_hit <- lex_match(mid[(L - len + 1L):L], COUNTRY_LEX, cap = 1L)
    if (!is.na(c_hit)) {
      rest <- mid[rng(1L, L - len)]
      return(c(nz(if (length(rest)) paste(rest, collapse = " ") else NA), c_hit))
    }
  }
  for (len in rev(seq_len(min(3L, L)))) {
    m_hit <- lex_match(mid[1:len], MAJOR_LEX)
    if (!is.na(m_hit)) {
      rest <- mid[rng(len + 1L, L)]
      return(c(m_hit, nz(if (length(rest)) paste(rest, collapse = " ") else NA)))
    }
  }
  if (L == 1L) return(c(mid[1], NA_character_))
  c(paste(mid[1:(L - 1L)], collapse = " "), mid[L])
}

parse_row_by_anchors <- function(tokens) {
  n <- length(tokens)
  if (n < 6L) return(NULL)
  has_id <- is_id_token(tokens[1])
  id_val <- if (has_id) repair_id_token(tokens[1]) else NA_character_
  ln <- if (has_id) 2L else 1L
  fn <- ln + 1L
  if (n < fn + 3L) return(NULL)

  gen <- vapply(tokens, match_gender, character(1), USE.NAMES = FALSE)
  sta <- vapply(tokens, match_status, character(1), USE.NAMES = FALSE)
  g   <- locate_anchor(tokens, fn, gen, sta)
  if (is.na(g) || g + 1L > n) return(NULL)
  s   <- g + 1L
  gender_val <- if (!is.na(gen[g])) gen[g] else match_gender(substr(tokens[g], 1L, 1L))
  status_val <- if (!is.na(sta[s])) sta[s] else tokens[s]

  cs_tok <- tokens[rng(fn + 1L, g - 1L)]
  cs     <- if (length(cs_tok)) split_city_state(paste(cs_tok, collapse = " ")) else c(NA_character_, NA_character_)

  rest <- if (s < n) tokens[(s + 1L):n] else character(0)
  L <- length(rest); k <- 0L
  while (k < min(N_TAIL, L) && isTRUE(is_numeric_ish(rest[L - k]))) k <- k + 1L
  tail_tok <- if (k > 0L) rest[(L - k + 1L):L] else character(0)
  mc       <- split_major_country(rest[rng(1L, L - k)])
  tail5    <- if (k == N_TAIL) tail_tok else assign_tail(tail_tok)

  row <- c(id_val, nz(tokens[ln]), nz(tokens[fn]), cs[1], cs[2],
           nz(gender_val), nz(status_val), mc[1], mc[2], tail5)
  if (length(row) != N_COLS) stop("row assembled with ", length(row), " fields")
  row
}

# tier 3: strict 14-cell split (TAB / 2+ spaces / comma / semicolon / pipe)
fallback_split14 <- function(line) {
  for (sep in c(SPLIT_REGEX, ",", ";", "\\|")) {
    f <- stringr::str_trim(stringr::str_split(stringr::str_trim(line), sep)[[1]])
    if (length(f) == N_COLS) { f[1] <- repair_id_token(f[1]); return(f) }
  }
  NULL
}


# ------------------- 8. UNIFIED LINE LOOP -------------------------------------

parse_lines <- function(lines, source_name = "") {
  rows <- list(); n_regex <- 0L; n_anchor <- 0L; n_split <- 0L; n_rej <- 0L; n_seen <- 0L

  for (raw_line in lines) {
    line <- strip_line_noise(raw_line)
    if (!nzchar(line) || is_header_line(line)) next
    n_seen <- n_seen + 1L

    row <- tryCatch(parse_row_regex(line), error = function(e) NULL)
    if (!is.null(row)) { n_regex <- n_regex + 1L
    } else {
      row <- tryCatch(parse_row_by_anchors(tokenize_line(line)), error = function(e) NULL)
      if (!is.null(row)) { n_anchor <- n_anchor + 1L
      } else {
        row <- fallback_split14(line)
        if (is.null(row)) row <- fallback_split14(raw_line)   # raw line keeps TAB separators
        if (!is.null(row)) n_split <- n_split + 1L
      }
    }

    if (is.null(row) || length(row) != N_COLS) {
      if (stringr::str_count(line, "\\S+") >= 4L) {
        n_rej <- n_rej + 1L
        log_msg("WARN", sprintf("%s: REJECTED line (no pattern matched): %s",
                                source_name, substr(line, 1L, 90L)))
      }
      next
    }
    rows[[length(rows) + 1L]] <- row
  }

  log_msg("INFO", sprintf("%s: %d candidate lines -> %d records (regex %d | anchors %d | split %d | rejected %d)",
                          source_name, n_seen, length(rows), n_regex, n_anchor, n_split, n_rej))
  if (length(rows) == 0L) return(empty_frame())

  mat <- do.call(rbind, rows)
  colnames(mat) <- TARGET_COLS
  df <- tibble::as_tibble(mat)
  hint  <- suppressWarnings(as.integer(repair_id_token(stringr::str_sub(stringr::str_squish(df$LastName), 4L))))
  df$ID <- as.character(reconcile_ids(df$ID, hints = hint))
  df
}


# ------------------- 9. VALIDATION, 30-ROW FINALISE, VOTING --------------------

cell_valid <- function(col, v) {
  v   <- as.character(v)
  num <- suppressWarnings(as.numeric(v))
  inr <- function(r) !is.na(num) & num >= r[1] & num <= r[2]
  res <- switch(col,
    ID            = inr(c(1, EXPECTED_ROWS)),
    LastName      = stringr::str_detect(v, "^DOE[0-9]+$"),
    FirstName     = stringr::str_detect(v, "^(JANE|JOE)[0-9]+$"),
    City          = nchar(v) >= 2L,
    State         = v %in% c(STATE_CODES, STATE_NAMES),
    Gender        = v %in% GENDER_LEX,
    StudentStatus = v %in% c("Undergraduate", "Graduate"),
    Major         = v %in% MAJOR_LEX,
    Country       = TRUE,
    Age           = inr(AGE_RANGE),
    SAT           = inr(SAT_RANGE),
    AverageScore  = inr(TAIL_FIT$AverageScore),
    Height        = inr(TAIL_FIT$Height),
    NewspaperReadership = inr(TAIL_FIT$NewspaperReadership),
    FALSE)
  rep_len(res, length(v)) %in% TRUE
}

valid_matrix <- function(df) {
  m <- do.call(cbind, lapply(TARGET_COLS, function(cn) cell_valid(cn, df[[cn]])))
  colnames(m) <- TARGET_COLS
  m
}

score_table <- function(df) sum(valid_matrix(df))

review_rows <- function(df, source_name = "") {
  if (nrow(df) == 0L) return(invisible(df))
  m   <- valid_matrix(df)
  bad <- which(rowSums(!m) > 0L)
  for (i in bad)
    log_msg("WARN", sprintf("%s: row ID %s needs manual review (columns: %s).",
                            source_name, df$ID[i],
                            paste(TARGET_COLS[!m[i, ]], collapse = ", ")))
  invisible(df)
}

finalize_table <- function(df, n_rows = EXPECTED_ROWS) {
  stopifnot(all(TARGET_COLS %in% names(df)))
  df <- dplyr::select(df, dplyr::all_of(TARGET_COLS))

  stray <- is.na(df$ID) | df$ID < 1L | df$ID > n_rows
  if (any(stray)) log_msg("WARN", sprintf("Dropping %d row(s) with an unusable ID.", sum(stray)))

  df <- df %>%
    dplyr::filter(!stray) %>%
    dplyr::mutate(.n_na = rowSums(is.na(dplyr::across(dplyr::all_of(TARGET_COLS))))) %>%
    dplyr::group_by(ID) %>%
    dplyr::slice_min(.n_na, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.n_na)

  missing_ids <- setdiff(seq_len(n_rows), df$ID)
  if (length(missing_ids) > 0L)
    log_msg("WARN", sprintf("OCR lost %d row(s); placeholders inserted for ID: %s",
                            length(missing_ids), paste(missing_ids, collapse = ", ")))

  out <- tibble::tibble(ID = seq_len(n_rows)) %>%
    dplyr::left_join(df, by = "ID") %>%
    dplyr::arrange(ID) %>%
    dplyr::select(dplyr::all_of(TARGET_COLS))

  stopifnot(nrow(out) == n_rows, identical(names(out), TARGET_COLS),
            identical(out$ID, seq_len(n_rows)))
  out
}

build_table <- function(lines, source_name = "") {
  raw <- parse_lines(lines, source_name = source_name)
  finalize_table(clean_dataframe(raw))
}

consensus_tables <- function(tabs, scores) {
  ord  <- order(scores, decreasing = TRUE)
  tabs <- tabs[ord]
  out  <- tabs[[1]]
  if (length(tabs) == 1L) return(out)

  for (j in seq_along(TARGET_COLS)) {
    cn <- TARGET_COLS[j]
    if (cn == "ID") next
    cols <- lapply(tabs, function(t) as.character(t[[cn]]))
    vals <- do.call(cbind, cols)
    ok   <- do.call(cbind, lapply(cols, function(cv) cell_valid(cn, cv)))
    for (i in seq_len(nrow(vals))) {
      v <- vals[i, ]; w <- 1 + 3 * ok[i, ]
      keep <- !is.na(v)
      if (!any(keep)) next
      v <- v[keep]; w <- w[keep]
      tot  <- tapply(w, v, sum)
      best <- names(tot)[tot == max(tot)]
      pick <- v[v %in% best][1]
      out[[cn]][i] <- if (is.numeric(out[[cn]])) as.numeric(pick) else pick
    }
  }
  out
}

quality_report <- function(tab, source_name) {
  m <- valid_matrix(tab)
  rows <- list()
  for (j in seq_along(TARGET_COLS)) for (i in seq_len(nrow(tab))) {
    if (!m[i, j]) {
      val <- tab[[j]][i]
      rows[[length(rows) + 1L]] <- data.frame(
        File = source_name, ID = tab$ID[i], Column = TARGET_COLS[j],
        Value = ifelse(is.na(val), "", as.character(val)),
        Issue = ifelse(is.na(val), "empty / not read", "does not match expected pattern"),
        stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0L)
    return(data.frame(File = character(0), ID = integer(0), Column = character(0),
                      Value = character(0), Issue = character(0)))
  do.call(rbind, rows)
}


# ----------------------------- 10. EXTRACTION ---------------------------------

prep_image <- function(img, mode = "gray") {
  img <- tryCatch(magick::image_background(img, "white", flatten = TRUE),
                  error = function(e) img)
  img <- tryCatch(magick::image_orient(img), error = function(e) img)
  img <- magick::image_convert(img, colorspace = "Gray")
  w      <- magick::image_info(img)$width[1]
  target <- if (mode == "big") 4500L else OCR_TARGET_WIDTH
  if (w < target) {
    img <- magick::image_resize(img, paste0(target, "x"), filter = "Lanczos")
  } else if (w > 6000L) {
    img <- magick::image_resize(img, "6000x")
  }
  img <- magick::image_border(img, "white", "30x30")
  if (mode == "deskew")
    img <- tryCatch(magick::image_deskew(img, threshold = 40), error = function(e) img)
  img <- magick::image_normalize(img)
  if (mode == "lat") {
    img <- tryCatch(magick::image_lat(img, geometry = "50x50+8%"),
                    error = function(e) magick::image_contrast(img, sharpen = 2))
  } else {
    img <- magick::image_contrast(img, sharpen = 2)
  }
  img
}

.ocr_engines <- new.env()
get_engine <- function(psm) {
  key <- paste0("psm", psm)
  if (is.null(.ocr_engines[[key]])) {
    .ocr_engines[[key]] <- tryCatch(
      tesseract::tesseract(language = "eng",
                           options = list(tessedit_pageseg_mode = psm,
                                          preserve_interword_spaces = "1")),
      error = function(e) {
        ok <- tryCatch({ tesseract::tesseract_download("eng"); TRUE }, error = function(e2) FALSE)
        if (!ok) stop("Could not start Tesseract ('eng'). Run tesseract::tesseract_download('eng') once. Details: ",
                      conditionMessage(e), call. = FALSE)
        tesseract::tesseract(language = "eng",
                             options = list(tessedit_pageseg_mode = psm,
                                            preserve_interword_spaces = "1"))
      })
  }
  .ocr_engines[[key]]
}

ocr_pages <- function(pages, spec) {
  engine <- get_engine(spec$psm)
  out <- character(0)
  for (pg in pages) {
    img <- prep_image(pg, spec$prep)
    tmp <- tempfile(fileext = ".png")
    magick::image_write(img, path = tmp, format = "png", density = "300x300")
    txt <- tesseract::ocr(tmp, engine = engine)
    unlink(tmp)
    out <- c(out, unlist(strsplit(txt, "\r?\n"), use.names = FALSE))
  }
  out
}

load_image_pages <- function(path) {
  img <- magick::image_read(path)
  lapply(seq_along(img), function(i) img[i])
}

render_pdf_pages <- function(path) {
  n <- pdftools::pdf_info(path)$pages
  files <- file.path(tempdir(), sprintf("copo_%s_p%03d.png", as.integer(Sys.time()), seq_len(n)))
  on.exit(unlink(files), add = TRUE)
  pdftools::pdf_convert(path, format = "png", dpi = 300, filenames = files, verbose = FALSE)
  lapply(files, magick::image_read)
}

extract_pdf_text_lines <- function(path) {
  txt <- tryCatch(pdftools::pdf_text(path), error = function(e) character(0))
  unlist(strsplit(txt, "\r?\n"), use.names = FALSE)
}

extract_txt_lines <- function(path) {
  read_with <- function(enc) {
    con <- file(path, open = "r", encoding = enc); on.exit(close(con), add = TRUE)
    readLines(con, warn = FALSE)
  }
  tryCatch(read_with("UTF-8"),
           error = function(e) read_with("latin1"),
           warning = function(w) read_with("latin1"))
}

process_file <- function(path) {
  log_msg("INFO", strrep("-", 60))
  log_msg("INFO", "Processing: ", basename(path))
  ext  <- tolower(tools::file_ext(path))
  full <- EXPECTED_ROWS * N_COLS

  tabs <- list(); scores <- numeric(0)
  try_pass <- function(nm, lines) {
    tab <- with_quiet(build_table(lines, nm))
    sc  <- score_table(tab)
    log_msg("INFO", sprintf("  pass %-12s -> %3d / %d valid cells", nm, sc, full))
    tabs[[length(tabs) + 1L]]   <<- tab
    scores[length(scores) + 1L] <<- sc
    sc >= full
  }
  run_ocr <- function(pages) {
    for (spec in OCR_PASSES) {
      lines <- tryCatch(ocr_pages(pages, spec),
                        error = function(e) { log_msg("ERROR", spec$name, ": ", conditionMessage(e)); NULL })
      if (is.null(lines)) next
      if (try_pass(spec$name, lines)) break
    }
  }

  if (ext %in% TEXT_EXT) {
    log_msg("INFO", sprintf("Format: TEXT (.%s) -> read directly, OCR bypassed", ext))
    try_pass("text", extract_txt_lines(path))

  } else if (ext == "pdf") {
    log_msg("INFO", "Format: PDF -> text layer first, OCR fallback")
    nat <- extract_pdf_text_lines(path)
    perfect <- FALSE
    if (any(nzchar(stringr::str_trim(nat)))) {
      perfect <- try_pass("pdf-text", nat)
    } else {
      log_msg("WARN", basename(path), ": no text layer -> scanned PDF, rasterising at 300 DPI")
    }
    if (!perfect) {
      pages <- render_pdf_pages(path)
      log_msg("INFO", sprintf("  %d page(s) rasterised", length(pages)))
      run_ocr(pages)
    }

  } else if (ext %in% IMAGE_EXT) {
    log_msg("INFO", sprintf("Format: IMAGE (.%s) -> multi-pass magick + tesseract", ext))
    run_ocr(load_image_pages(path))

  } else {
    stop(sprintf("Unsupported file format '.%s' (supported: %s)",
                 ext, paste0(".", SUPPORTED_EXT, collapse = ", ")), call. = FALSE)
  }

  if (length(tabs) == 0L) stop("no extraction pass produced a result", call. = FALSE)

  best <- consensus_tables(tabs, scores)
  log_msg("INFO", sprintf("Result for %s: %d / %d cells valid after voting over %d pass(es)",
                          basename(path), score_table(best), full, length(tabs)))
  review_rows(best, basename(path))
  list(table = best, tabs = tabs, scores = scores)
}



# ----------------------------- 11. USER INTERACTION (file menu) ---------------

# ----------------------------- 11. USER INTERACTION ---------------------------

.io <- new.env()

ask_input <- function(prompt) {
  if (interactive()) return(readline(prompt))
  if (is.null(.io$con)) .io$con <- file("stdin", open = "r")
  cat(prompt); flush(stdout())
  ans <- readLines(.io$con, n = 1L, warn = FALSE)
  if (length(ans) == 0L) NA_character_ else ans
}

parse_selection <- function(ans, n) {
  a <- tolower(stringr::str_squish(ifelse(is.na(ans), "", ans)))
  if (a %in% c("", "all", "a", "*")) return(seq_len(n))
  if (a %in% c("q", "quit", "exit", "0", "n", "no")) return(integer(0))
  idx <- integer(0)
  for (p in Filter(nzchar, stringr::str_split(a, "[,; ]+")[[1]])) {
    if (grepl("^[0-9]+-[0-9]+$", p)) {
      b <- as.integer(strsplit(p, "-")[[1]]); idx <- c(idx, rng(b[1], b[2]))
    } else if (grepl("^[0-9]+$", p)) {
      idx <- c(idx, as.integer(p))
    } else return(NULL)
  }
  idx <- unique(idx)
  if (length(idx) == 0L || any(idx < 1L | idx > n)) NULL else idx
}

select_files <- function(upload_dir, cli) {
  all_f <- list.files(upload_dir, full.names = TRUE)
  all_f <- all_f[!dir.exists(all_f)]
  ext   <- tolower(tools::file_ext(all_f))
  skipped <- all_f[!ext %in% SUPPORTED_EXT]
  files   <- sort(all_f[ext %in% SUPPORTED_EXT])
  if (length(skipped) > 0L)
    log_msg("WARN", "Skipping unsupported file(s): ", paste(basename(skipped), collapse = ", "))
  if (length(files) == 0L)
    stop(sprintf("No supported files in '%s' (supported: %s)", upload_dir,
                 paste0(".", SUPPORTED_EXT, collapse = " ")), call. = FALSE)

  want <- sub("^--files=", "", cli[grepl("^--files=", cli)])
  if (length(want) > 0L) {
    names_wanted <- trimws(strsplit(want[1], ",")[[1]])
    hit <- files[basename(files) %in% names_wanted]
    if (length(hit) == 0L) stop("None of the --files= names were found in uploads/", call. = FALSE)
    return(hit)
  }
  if ("--all" %in% cli) return(files)

  cat("\nFiles found in '", UPLOAD_DIR_NAME, "/':\n", sep = "")
  for (i in seq_along(files))
    cat(sprintf("  [%d] %-40s %8.0f KB\n", i, basename(files[i]),
                file.size(files[i]) / 1024))
  repeat {
    ans <- ask_input("\nWhich file(s) should be processed?  (all | 1,3 | 2-4 | q) [Enter = all]: ")
    if (is.na(ans)) { log_msg("INFO", "No console input available -> processing ALL files."); return(files) }
    sel <- parse_selection(ans, length(files))
    if (is.null(sel)) { cat("  Invalid choice, try again.\n"); next }
    if (length(sel) == 0L) { cat("Cancelled. Nothing processed.\n"); return(character(0)) }
    cat("Selected: ", paste(basename(files[sel]), collapse = ", "), "\n", sep = "")
    return(files[sel])
  }
}

choose_mode <- function(n_files, cli) {
  if (n_files < 2L) return("single")
  if ("--merge" %in% cli)    return("merge")
  if ("--separate" %in% cli) return("separate")
  cat("\nHow should the results be written?\n",
      "  [1] SEPARATE - separate CSV files for every input file\n",
      "  [2] MERGE    - one combined CSV (generic: all text lines of all files;\n",
      "                 student mode: same table in different formats, cells cross-verified)\n", sep = "")
  repeat {
    ans <- trimws(ask_input("Choice [1/2, Enter = 1]: "))
    if (is.na(ans) || ans %in% c("", "1")) return("separate")
    if (ans == "2") return("merge")
    cat("  Please type 1 or 2.\n")
  }
}


# =============================================================================
# 12. GENERIC ENGINE  -  any document -> text lines + auto-detected tables
# =============================================================================

GEN <- new.env()
GEN$passes    <- 3L          # OCR passes tried per page (early stop when good)
GEN$stop_conf <- 90          # stop trying more passes above this avg confidence
GEN$min_text  <- 15L         # chars needed for a PDF page to count as "has text"

NUM_RX   <- "^[-+(]?[$%\u20B9\u20AC\u00A3]?[0-9][0-9,.]*[)%]?$"
NUM_LOOSE <- "^[-+(]?[$%\u20B9\u20AC\u00A3]?[0-9OoDQlI|!SsBZg][0-9OoDQlI|!SsBZg,.]*[)%]?$"

# ---- OCR engine with any language (eng, eng+mar+hin, ...) -------------------
get_engine_lang <- function(lang, psm) {
  key <- paste0(lang, "_psm", psm)
  if (is.null(.ocr_engines[[key]])) {
    mk <- function() tesseract::tesseract(
      language = lang,
      options  = list(tessedit_pageseg_mode = psm, preserve_interword_spaces = "1"))
    .ocr_engines[[key]] <- tryCatch(mk(), error = function(e) {
      log_msg("INFO", "Downloading OCR language data: ", lang)
      ok <- tryCatch({
        for (l in strsplit(lang, "\\+")[[1]]) tesseract::tesseract_download(l)
        TRUE }, error = function(e2) FALSE)
      if (!ok) stop("Could not start Tesseract for language '", lang,
                    "'. Check the internet connection or run tesseract::tesseract_download('",
                    strsplit(lang, "\\+")[[1]][1], "') once. Details: ",
                    conditionMessage(e), call. = FALSE)
      mk()
    })
  }
  .ocr_engines[[key]]
}

# ---- one OCR pass -> data.frame of words with position + confidence ---------
ocr_words <- function(img, spec, lang) {
  engine <- get_engine_lang(lang, spec$psm)
  im  <- prep_image(img, spec$prep)
  tmp <- tempfile(fileext = ".png"); on.exit(unlink(tmp), add = TRUE)
  magick::image_write(im, path = tmp, format = "png", density = "300x300")
  d <- tesseract::ocr_data(tmp, engine = engine)
  if (is.null(d) || nrow(d) == 0L) return(NULL)
  bb <- do.call(rbind, lapply(strsplit(d$bbox, ","), as.numeric))
  w <- data.frame(word = d$word, conf = d$confidence,
                  x1 = bb[, 1], y1 = bb[, 2], x2 = bb[, 3], y2 = bb[, 4],
                  stringsAsFactors = FALSE)
  w[!is.na(w$word) & nzchar(trimws(w$word)), , drop = FALSE]
}

make_line <- function(page, cells, x, conf)
  list(page = as.integer(page), cells = as.character(cells),
       x = as.numeric(x), conf = as.numeric(conf))

# ---- OCR words -> lines -> cells (cells split at wide horizontal gaps) -------
ocr_words_to_lines <- function(w, page) {
  if (is.null(w) || nrow(w) == 0L) return(list())
  w$yc <- (w$y1 + w$y2) / 2
  medh <- max(stats::median(w$y2 - w$y1), 8)
  w    <- w[order(w$yc, w$x1), , drop = FALSE]

  grp <- integer(nrow(w)); g <- 1L; cur <- w$yc[1]; cnt <- 1L
  for (i in seq_len(nrow(w))) {
    if (i > 1L) {
      if (abs(w$yc[i] - cur) > 0.6 * medh) { g <- g + 1L; cur <- w$yc[i]; cnt <- 1L
      } else { cur <- (cur * cnt + w$yc[i]) / (cnt + 1L); cnt <- cnt + 1L }
    }
    grp[i] <- g
  }
  out <- vector("list", g)
  for (k in seq_len(g)) {
    ws <- w[grp == k, , drop = FALSE]
    ws <- ws[order(ws$x1), , drop = FALSE]
    n  <- nrow(ws)
    brk <- if (n > 1L) (ws$x1[-1] - ws$x2[-n]) > 1.3 * medh else logical(0)
    cid <- cumsum(c(TRUE, brk))
    out[[k]] <- make_line(page,
                          tapply(ws$word, cid, paste, collapse = " "),
                          tapply(ws$x1,   cid, min),
                          mean(ws$conf))
  }
  out
}

# ---- text layer (pdf / txt) -> lines -> cells (2+ spaces or TAB = new cell) --
text_to_lines <- function(txt, page) {
  out <- list()
  for (s in txt) {
    s <- gsub("\t", "    ", s, fixed = TRUE)
    s <- gsub("[\u00A0\u200B]", " ", s)
    if (!nzchar(trimws(s))) next
    m <- gregexpr("\\S+(?: \\S+)*", s, perl = TRUE)[[1]]
    if (m[1] == -1L) next
    cells <- regmatches(s, list(m))[[1]]
    out[[length(out) + 1L]] <- make_line(page, cells, as.integer(m), NA_real_)
  }
  out
}

lines_to_df <- function(lines, file) {
  if (length(lines) == 0L)
    return(data.frame(File = character(0), Page = integer(0), Line = integer(0),
                      Text = character(0), Confidence = numeric(0),
                      stringsAsFactors = FALSE))
  pg <- vapply(lines, function(l) l$page, integer(1))
  data.frame(File = file, Page = pg,
             Line = stats::ave(seq_along(pg), pg, FUN = seq_along),
             Text = vapply(lines, function(l) paste(l$cells, collapse = " "), character(1)),
             Confidence = round(vapply(lines, function(l) l$conf, numeric(1)), 1),
             stringsAsFactors = FALSE)
}

# ---- table detection ---------------------------------------------------------
detect_table_blocks <- function(lines) {
  n <- length(lines)
  if (n < 3L) return(list())
  nc  <- vapply(lines, function(l) length(l$cells), integer(1))
  row <- nc >= 2L
  row0 <- row
  for (i in 2:(n - 1L))                       # a single-cell line between two rows
    if (!row0[i] && row0[i - 1L] && row0[i + 1L]) row[i] <- TRUE   # = row with missing cells
  r    <- rle(row)
  ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1L
  lapply(which(r$values & r$lengths >= 3L), function(k) starts[k]:ends[k])
}

fix_numeric_columns <- function(df, label = "") {
  for (j in seq_along(df)) {
    v  <- df[[j]]; used <- !is.na(v) & nzchar(v)
    if (sum(used) < 3L) next
    ok    <- used & grepl(NUM_RX, v)
    if (sum(ok) / sum(used) < 0.7) next               # only clearly numeric columns
    loose <- used & !ok & grepl(NUM_LOOSE, v) & grepl("[0-9]", v)
    for (i in which(loose)) {
      f <- chartr("OoDQlI|!SsBZg", "0000111155829", v[i])
      if (grepl(NUM_RX, f)) {
        log_msg("FIX", sprintf("%s col '%s': '%s' -> '%s'", label, names(df)[j], v[i], f))
        df[[j]][i] <- f
      }
    }
  }
  df
}

block_to_table <- function(lines, idx, ocr, label = "") {
  blk <- lines[idx]
  nc  <- vapply(blk, function(l) length(l$cells), integer(1))
  tb  <- table(nc)
  ncol_ <- max(as.integer(names(tb)[tb == max(tb)]))
  if (ncol_ < 2L || sum(nc == ncol_) < max(2L, ceiling(0.5 * length(blk)))) return(NULL)

  ref  <- blk[nc == ncol_]
  colx <- vapply(seq_len(ncol_), function(j)
    stats::median(vapply(ref, function(l) l$x[j], numeric(1))), numeric(1))

  m <- matrix(NA_character_, nrow = length(blk), ncol = ncol_)
  for (i in seq_along(blk)) {
    l <- blk[[i]]
    if (length(l$cells) == ncol_) { m[i, ] <- l$cells; next }
    last <- 0L
    for (k in seq_along(l$cells)) {
      j <- which.min(abs(colx - l$x[k]))
      if (j <= last) {                                   # collision -> same column
        m[i, last] <- paste(m[i, last], l$cells[k])
      } else { m[i, j] <- l$cells[k]; last <- j }
    }
  }

  pages <- vapply(blk, function(l) l$page, integer(1))
  has_num <- function(x) any(grepl(NUM_RX, x[!is.na(x)]))
  first   <- m[1, ]
  header  <- !has_num(first) && has_num(as.vector(m[-1, , drop = FALSE])) &&
             mean(!is.na(first)) >= 0.6
  if (header) {
    nm <- trimws(first)
    nm[is.na(nm) | !nzchar(nm)] <- paste0("Col", which(is.na(nm) | !nzchar(nm)))
    nm <- make.unique(nm, sep = "_")
    body <- m[-1, , drop = FALSE]; pages <- pages[-1]
    dup  <- apply(body, 1, function(r) identical(tolower(trimws(r)), tolower(trimws(first))))
    body <- body[!dup, , drop = FALSE]; pages <- pages[!dup]   # repeated page headers
  } else {
    nm <- paste0("Col", seq_len(ncol_)); body <- m
  }
  if (nrow(body) == 0L) return(NULL)
  df <- as.data.frame(body, stringsAsFactors = FALSE)
  names(df) <- nm
  df[] <- lapply(df, trimws)
  if (ocr) df <- fix_numeric_columns(df, label)
  if (length(unique(pages)) > 1L) df <- cbind(Page = pages, df)
  df
}

# ---- OCR one page: try several passes, keep the one with best confidence ----
ocr_best_page <- function(img, lang, page) {
  specs <- OCR_PASSES[seq_len(min(GEN$passes, length(OCR_PASSES)))]
  best  <- NULL
  for (spec in specs) {
    w <- tryCatch(ocr_words(img, spec, lang), error = function(e) {
      log_msg("ERROR", spec$name, ": ", conditionMessage(e)); NULL })
    if (is.null(w) || nrow(w) == 0L) next
    sc <- mean(w$conf)
    log_msg("INFO", sprintf("    page %d | pass %-12s -> %4d words | avg confidence %.1f",
                            page, spec$name, nrow(w), sc))
    if (is.null(best) || sc > best$score) best <- list(w = w, score = sc, name = spec$name)
    if (sc >= GEN$stop_conf) break
  }
  if (is.null(best)) return(list(lines = list(), score = NA_real_, name = "ocr-empty"))
  list(lines = ocr_words_to_lines(best$w, page), score = best$score, name = best$name)
}

render_pdf_page <- function(path, p) {
  f <- tempfile(fileext = ".png"); on.exit(unlink(f), add = TRUE)
  suppressWarnings(pdftools::pdf_convert(path, format = "png", pages = p, dpi = 300,
                                         filenames = f, verbose = FALSE))
  magick::image_read(f)
}

read_delimited_direct <- function(path, ext) {
  first <- tryCatch(readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) "")
  if (length(first) == 0L) return(NULL)
  cnt <- c(`,` = lengths(regmatches(first, gregexpr(",", first, fixed = TRUE))),
           `;` = lengths(regmatches(first, gregexpr(";", first, fixed = TRUE))),
           `\t` = lengths(regmatches(first, gregexpr("\t", first, fixed = TRUE))))
  sep <- if (ext == "tsv") "\t" else names(cnt)[which.max(cnt)]
  rd  <- function(enc) utils::read.table(path, sep = sep, header = TRUE, quote = "\"",
                                         comment.char = "", fill = TRUE, check.names = FALSE,
                                         stringsAsFactors = FALSE, colClasses = "character",
                                         fileEncoding = enc)
  d <- tryCatch(rd("UTF-8"), error = function(e) tryCatch(rd("latin1"), error = function(e2) NULL),
                warning = function(w) tryCatch(rd("latin1"), error = function(e2) NULL))
  if (is.null(d) || ncol(d) < 2L) NULL else d
}

# ---- process ONE file (generic mode) ----------------------------------------
process_generic <- function(path, lang) {
  ext  <- tolower(tools::file_ext(path)); base <- basename(path)
  log_msg("INFO", strrep("-", 60))
  log_msg("INFO", "Processing: ", base)

  lines <- list(); methods <- character(0); scores <- numeric(0)
  direct <- NULL; npages <- 1L; ocr_used <- FALSE

  if (ext %in% TEXT_EXT) {
    log_msg("INFO", sprintf("Format: TEXT (.%s) -> read directly, OCR bypassed", ext))
    lines   <- text_to_lines(extract_txt_lines(path), 1L)
    methods <- "text-file"
    if (ext %in% c("csv", "tsv")) direct <- read_delimited_direct(path, ext)

  } else if (ext == "pdf") {
    npages <- pdftools::pdf_info(path)$pages
    txts   <- tryCatch(pdftools::pdf_text(path), error = function(e) rep("", npages))
    log_msg("INFO", sprintf("Format: PDF, %d page(s) -> text layer first, OCR for scanned pages", npages))
    for (p in seq_len(npages)) {
      t <- if (p <= length(txts)) txts[p] else ""
      if (nchar(gsub("\\s", "", t)) >= GEN$min_text) {
        lines   <- c(lines, text_to_lines(strsplit(t, "\r?\n")[[1]], p))
        methods <- c(methods, "pdf-text")
      } else {
        log_msg("INFO", sprintf("  page %d has no text layer -> OCR at 300 DPI", p))
        r <- ocr_best_page(render_pdf_page(path, p), lang, p)
        lines <- c(lines, r$lines); methods <- c(methods, r$name)
        scores <- c(scores, r$score); ocr_used <- TRUE
      }
    }

  } else if (ext %in% IMAGE_EXT) {
    pages  <- load_image_pages(path); npages <- length(pages)
    log_msg("INFO", sprintf("Format: IMAGE (.%s), %d page(s) -> multi-pass OCR (%s)", ext, npages, lang))
    for (p in seq_len(npages)) {
      r <- ocr_best_page(pages[[p]], lang, p)
      lines <- c(lines, r$lines); methods <- c(methods, r$name)
      scores <- c(scores, r$score); ocr_used <- TRUE
    }
  } else {
    stop(sprintf("Unsupported file format '.%s'", ext), call. = FALSE)
  }

  tables <- list()
  if (!is.null(direct)) {
    tables[[1]] <- direct
  } else {
    for (idx in detect_table_blocks(lines)) {
      tb <- tryCatch(block_to_table(lines, idx, ocr_used, base), error = function(e) NULL)
      if (!is.null(tb)) tables[[length(tables) + 1L]] <- tb
    }
  }
  txt_df <- lines_to_df(lines, base)
  conf   <- if (length(scores) && any(!is.na(scores))) round(mean(scores, na.rm = TRUE), 1) else NA_real_
  log_msg("INFO", sprintf("Result: %d page(s), %d text line(s), %d table(s)%s",
                          npages, nrow(txt_df), length(tables),
                          if (is.na(conf)) "" else sprintf(", avg OCR confidence %.1f", conf)))
  if (nrow(txt_df) == 0L)
    log_msg("WARN", base, ": nothing readable found (blank or very low-quality image?)")

  list(file = base, ext = ext, pages = npages, method = paste(unique(methods), collapse = "+"),
       text = txt_df, tables = tables, conf = conf)
}


# =============================================================================
# 13. WRITING CSV + GENERIC RUN
# =============================================================================

# UTF-8 with BOM so Excel shows Marathi/Hindi/accents correctly
write_csv_utf8 <- function(df, path) {
  con <- file(path, open = "wb"); writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con); close(con)
  suppressWarnings(utils::write.table(df, path, sep = ",", row.names = FALSE, na = "",
                     qmethod = "double", append = TRUE, fileEncoding = "UTF-8"))
  log_msg("INFO", "Written: ", path)
  invisible(path)
}

safe_name <- function(x) gsub("[^A-Za-z0-9_-]+", "_", x)

preview_result <- function(res) {
  cat("\n  Preview of ", res$file, ":\n", sep = "")
  if (length(res$tables) > 0L) {
    print(utils::head(res$tables[[1]], 6L), row.names = FALSE)
  } else if (nrow(res$text) > 0L) {
    print(utils::head(res$text[, c("Page", "Line", "Text")], 8L), row.names = FALSE)
  } else cat("  (nothing extracted)\n")
  cat("\n")
}

run_generic <- function(files, mode, lang, root) {
  stamp   <- format(Sys.time(), "%Y%m%d_%H%M%S")
  out_dir <- file.path(root, OUTPUT_DIR_NAME)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  results <- list(); summary_rows <- list()
  for (f in files) {
    res <- tryCatch(process_generic(f, lang), error = function(e) {
      log_msg("ERROR", basename(f), ": ", conditionMessage(e)); NULL })
    if (is.null(res)) {
      summary_rows[[length(summary_rows) + 1L]] <- data.frame(
        File = basename(f), Type = tolower(tools::file_ext(f)), Pages = NA, Method = "",
        TextLines = 0L, Tables = 0L, AvgConfidence = NA, Status = "FAILED",
        OutputFiles = "", stringsAsFactors = FALSE)
      next
    }
    results[[res$file]] <- res
    preview_result(res)
  }
  if (length(results) == 0L) stop("No file could be processed.", call. = FALSE)

  outs <- list()
  if (mode == "merge") {
    all_txt <- do.call(rbind, lapply(results, `[[`, "text"))
    p <- file.path(out_dir, sprintf("combined_text_%s.csv", stamp))
    write_csv_utf8(all_txt, p); outs[["*"]] <- basename(p)
  }
  for (nm in names(results)) {
    r <- results[[nm]]; tag <- sprintf("%s_%s", safe_name(tools::file_path_sans_ext(nm)), r$ext)
    made <- character(0)
    if (mode != "merge") {
      p <- file.path(out_dir, sprintf("%s_text_%s.csv", tag, stamp))
      write_csv_utf8(r$text, p); made <- c(made, basename(p))
    }
    for (k in seq_along(r$tables)) {
      p <- file.path(out_dir, sprintf("%s_table%d_%s.csv", tag, k, stamp))
      write_csv_utf8(r$tables[[k]], p); made <- c(made, basename(p))
    }
    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
      File = nm, Type = r$ext, Pages = r$pages, Method = r$method,
      TextLines = nrow(r$text), Tables = length(r$tables),
      AvgConfidence = r$conf,
      Status = if (nrow(r$text) == 0L) "EMPTY" else "OK",
      OutputFiles = paste(c(made, if (mode == "merge") outs[["*"]]), collapse = " ; "),
      stringsAsFactors = FALSE)
  }
  summ <- do.call(rbind, summary_rows)
  write_csv_utf8(summ, file.path(out_dir, sprintf("summary_%s.csv", stamp)))

  cat("\n================ SUMMARY ================\n")
  print(summ[, c("File", "Type", "Pages", "TextLines", "Tables", "AvgConfidence", "Status")],
        row.names = FALSE)
  cat("\nOutput folder: ", out_dir, "\n", sep = "")
  log_msg("INFO", "DONE.")
  invisible(results)
}


# =============================================================================
# 14. MENUS (profile / language) + STUDENT PROFILE RUN + MAIN
# =============================================================================

choose_profile <- function(cli) {
  m <- sub("^--(mode|profile)=", "", cli[grepl("^--(mode|profile)=", cli)])
  if (length(m) > 0L)
    return(if (tolower(m[1]) %in% c("student", "2", "table14")) "student" else "generic")
  cat("\nProcessing mode:\n",
      "  [1] GENERIC - any document: text + automatic table detection -> CSV\n",
      "  [2] STUDENT - original 14-column / 30-row student table profile\n", sep = "")
  repeat {
    ans <- trimws(ask_input("Choice [1/2, Enter = 1]: "))
    if (is.na(ans) || ans %in% c("", "1")) return("generic")
    if (ans == "2") return("student")
    cat("  Please type 1 or 2.\n")
  }
}

choose_lang <- function(files, cli) {
  l <- sub("^--lang=", "", cli[grepl("^--lang=", cli)])
  if (length(l) > 0L && grepl("^[A-Za-z_]+(\\+[A-Za-z_]+)*$", l[1])) return(l[1])
  need_ocr <- any(tolower(tools::file_ext(files)) %in% c(IMAGE_EXT, "pdf"))
  if (!need_ocr || "--all" %in% cli || any(grepl("^--(files|mode|profile)=", cli))) return("eng")
  ans <- trimws(ask_input("\nOCR language [Enter = eng | e.g. eng+mar+hin for Marathi/Hindi]: "))
  if (is.na(ans) || !nzchar(ans) || !grepl("^[A-Za-z_]+(\\+[A-Za-z_]+)*$", ans)) "eng" else ans
}

run_student_profile <- function(files, mode, root) {
  results <- list()
  for (f in files) {
    res <- tryCatch(process_file(f), error = function(e) {
      log_msg("ERROR", basename(f), ": ", conditionMessage(e)); NULL })
    if (!is.null(res)) results[[basename(f)]] <- res
  }
  if (length(results) == 0L) stop("No file could be processed.", call. = FALSE)

  stamp   <- format(Sys.time(), "%Y%m%d_%H%M%S")
  out_dir <- file.path(root, OUTPUT_DIR_NAME)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  reports <- list()
  if (mode == "merge") {
    all_tabs   <- unlist(lapply(results, `[[`, "tabs"), recursive = FALSE)
    all_scores <- unlist(lapply(results, `[[`, "scores"))
    merged     <- consensus_tables(all_tabs, all_scores)
    log_msg("INFO", sprintf("MERGED %d file(s): %d / %d cells valid",
                            length(results), score_table(merged), EXPECTED_ROWS * N_COLS))
    write_csv_utf8(merged, file.path(out_dir, sprintf("clean_data_%s.csv", stamp)))
    reports[[1]] <- quality_report(merged, paste(names(results), collapse = " + "))
  } else {
    for (nm in names(results)) {
      out_name <- sprintf("clean_%s_%s.csv", gsub("\\.", "_", tools::file_path_sans_ext(nm)), stamp)
      if (anyDuplicated(tools::file_path_sans_ext(names(results))) > 0L)
        out_name <- sprintf("clean_%s_%s.csv", gsub("\\.", "_", nm), stamp)
      write_csv_utf8(results[[nm]]$table, file.path(out_dir, out_name))
      reports[[length(reports) + 1L]] <- quality_report(results[[nm]]$table, nm)
    }
  }
  rep_all <- do.call(rbind, reports)
  write_csv_utf8(rep_all, file.path(out_dir, sprintf("quality_report_%s.csv", stamp)))
  if (nrow(rep_all) == 0L) log_msg("INFO", "DONE - every cell passed validation.")
  else log_msg("WARN", sprintf("DONE - %d cell(s) still empty/suspicious, see quality_report_%s.csv",
                               nrow(rep_all), stamp))
  invisible(results)
}

main <- function() {
  check_dependencies()
  cli <- commandArgs(trailingOnly = TRUE)

  rows_arg <- sub("^--rows=", "", cli[grepl("^--rows=", cli)])
  if (length(rows_arg) > 0L && !is.na(suppressWarnings(as.integer(rows_arg[1]))))
    EXPECTED_ROWS <<- as.integer(rows_arg[1])
  pass_arg <- suppressWarnings(as.integer(sub("^--passes=", "", cli[grepl("^--passes=", cli)])))
  if (length(pass_arg) > 0L && !is.na(pass_arg[1])) GEN$passes <- max(1L, min(6L, pass_arg[1]))

  # project folder: next to the script, else the current working directory
  root <- get_project_root()
  if (!dir.exists(file.path(root, UPLOAD_DIR_NAME)) &&
      dir.exists(file.path(getwd(), UPLOAD_DIR_NAME))) root <- getwd()
  upload_dir <- file.path(root, UPLOAD_DIR_NAME)
  if (!dir.exists(upload_dir)) {
    dir.create(upload_dir, showWarnings = FALSE)
    stop("Folder 'uploads/' was missing, so it was created here:\n  ", upload_dir,
         "\nCopy your png/jpg/pdf/txt files into it and run the script again.", call. = FALSE)
  }
  log_msg("INFO", "Project folder: ", root)

  files   <- select_files(upload_dir, cli)
  if (length(files) == 0L) return(invisible(NULL))
  profile <- choose_profile(cli)
  mode    <- choose_mode(length(files), cli)

  if (profile == "student") return(run_student_profile(files, mode, root))
  lang <- choose_lang(files, cli)
  run_generic(files, mode, lang, root)
}

main()