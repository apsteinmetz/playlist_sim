#!/usr/bin/env Rscript

# Evaluate artist/title resolution through the MusicBrainz recording search API.
# MusicBrainz supplies identifiers and metadata, not acoustic features. Matched
# recording MBIDs can subsequently be joined to the AcousticBrainz data dumps.

project_library <- file.path("renv", "library")
if (dir.exists(project_library)) {
  .libPaths(c(normalizePath(project_library), .libPaths()))
}

required_packages <- c("curl", "jsonlite")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing packages: ", paste(missing_packages, collapse = ", "), ". ",
    "Install them into renv/library before running this script.",
    call. = FALSE
  )
}

`%||%` <- function(x, fallback) {
  if (is.null(x) || length(x) == 0L) fallback else x
}

normalize_text <- function(x) {
  x <- tolower(trimws(x %||% ""))
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^[:alnum:]]+", " ", x)
  gsub("\\s+", " ", trimws(x))
}

text_similarity <- function(x, y) {
  x <- normalize_text(x)
  y <- normalize_text(y)
  denominator <- max(nchar(x), nchar(y), 1L)
  max(0, 1 - as.numeric(utils::adist(x, y)) / denominator)
}

lucene_quote <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  gsub('"', '\\\\"', x, fixed = TRUE)
}

header_value <- function(headers, name) {
  lines <- unlist(strsplit(rawToChar(headers), "\\r?\\n"))
  hits <- grep(paste0("^", name, ":"), lines, ignore.case = TRUE, value = TRUE)
  if (length(hits) == 0L) return(NULL)
  trimws(sub("^[^:]+:", "", tail(hits, 1L)))
}

read_cache <- function(cache_file, request_url) {
  if (!file.exists(cache_file)) return(NULL)
  cached <- tryCatch(
    jsonlite::read_json(cache_file, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(cached) || !identical(cached$request_url, request_url)) return(NULL)
  cached$response
}

write_cache <- function(cache_file, request_url, response) {
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(request_url = request_url, response = response),
    cache_file,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
}

last_request_time <- NULL

musicbrainz_get <- function(url, cache_file, max_attempts = 5L) {
  cached <- read_cache(cache_file, url)
  if (!is.null(cached)) {
    return(list(ok = TRUE, status = 200L, body = cached, cached = TRUE))
  }

  contact <- Sys.getenv("MUSICBRAINZ_CONTACT", unset = "local research project")
  user_agent <- paste0("playlist-sim/0.1 (", contact, ")")

  for (attempt in seq_len(max_attempts)) {
    if (!is.null(last_request_time)) {
      elapsed <- as.numeric(difftime(Sys.time(), last_request_time, units = "secs"))
      if (elapsed < 1.1) Sys.sleep(1.1 - elapsed)
    }

    handle <- curl::new_handle(useragent = user_agent)
    response <- tryCatch(
      curl::curl_fetch_memory(url, handle = handle),
      error = identity
    )
    last_request_time <<- Sys.time()

    if (inherits(response, "error")) {
      if (attempt == max_attempts) {
        return(list(
          ok = FALSE, status = NA_integer_,
          error = conditionMessage(response), body = NULL
        ))
      }
      Sys.sleep(min(60, 2^(attempt - 1L)) + stats::runif(1L, 0, 0.25))
      next
    }

    status <- response$status_code
    body_text <- rawToChar(response$content)
    body <- tryCatch(
      jsonlite::fromJSON(body_text, simplifyVector = FALSE),
      error = function(e) list(raw_body = body_text)
    )

    if (status >= 200L && status < 300L) {
      write_cache(cache_file, url, body)
      return(list(ok = TRUE, status = status, body = body, cached = FALSE))
    }

    if (status %in% c(429L, 503L) && attempt < max_attempts) {
      retry_after <- suppressWarnings(as.numeric(
        header_value(response$headers, "Retry-After") %||% NA_character_
      ))
      wait_seconds <- if (is.na(retry_after)) {
        min(60, 2^(attempt - 1L))
      } else {
        retry_after
      }
      Sys.sleep(max(1.1, wait_seconds) + stats::runif(1L, 0, 0.25))
      next
    }

    if (status >= 500L && attempt < max_attempts) {
      Sys.sleep(min(60, 2^(attempt - 1L)) + stats::runif(1L, 0, 0.25))
      next
    }

    return(list(
      ok = FALSE, status = status, error = paste("HTTP", status), body = body
    ))
  }
}

artist_credit_text <- function(recording) {
  credits <- recording[["artist-credit"]] %||% list()
  names <- vapply(credits, function(credit) {
    credit$name %||% credit$artist$name %||% ""
  }, character(1))
  paste(names[nzchar(names)], collapse = "; ")
}

score_recordings <- function(recordings, artist, title) {
  if (length(recordings) == 0L) return(NULL)
  rows <- lapply(recordings, function(recording) {
    matched_artist <- artist_credit_text(recording)
    matched_title <- recording$title %||% ""
    artist_score <- text_similarity(matched_artist, artist)
    title_score <- text_similarity(matched_title, title)
    search_score <- as.numeric(recording$score %||% 0) / 100
    data.frame(
      musicbrainz_recording_id = recording$id %||% NA_character_,
      matched_artist = matched_artist,
      matched_title = matched_title,
      artist_score = artist_score,
      title_score = title_score,
      search_score = search_score,
      match_score = 0.35 * artist_score + 0.50 * title_score + 0.15 * search_score,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

classify_match <- function(scored) {
  if (is.null(scored) || nrow(scored) == 0L) return("missing")
  scored <- scored[order(-scored$match_score), , drop = FALSE]
  exact <- scored$title_score >= 0.98 & scored$artist_score >= 0.98
  exact_ids <- unique(scored$musicbrainz_recording_id[exact])
  if (length(exact_ids) == 1L) return("exact")
  if (length(exact_ids) > 1L) return("ambiguous")

  second_score <- if (nrow(scored) >= 2L) scored$match_score[[2L]] else 0
  margin <- scored$match_score[[1L]] - second_score
  if (scored$match_score[[1L]] >= 0.85 && margin >= 0.08) return("matched")
  if (scored$match_score[[1L]] >= 0.70) return("ambiguous")
  "missing"
}

input_paths <- file.path("data", c("playlist_1.csv", "playlist_2.csv"))
missing_inputs <- input_paths[!file.exists(input_paths)]
if (length(missing_inputs) > 0L) {
  stop("Missing input files: ", paste(missing_inputs, collapse = ", "), call. = FALSE)
}

dir.create(file.path("data", "cache", "musicbrainz"), recursive = TRUE,
           showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

playlist_rows <- do.call(rbind, lapply(input_paths, function(path) {
  x <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  needed <- c("DJ", "Artist", "Title")
  if (!all(needed %in% names(x))) {
    stop(path, " must contain DJ, Artist, and Title columns.", call. = FALSE)
  }
  x$playlist <- tools::file_path_sans_ext(basename(path))
  x[c("playlist", needed)]
}))
row.names(playlist_rows) <- NULL

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1L) {
  requested_limit <- suppressWarnings(as.integer(args[[1L]]))
  if (is.na(requested_limit) || requested_limit < 1L) {
    stop("The optional track limit must be a positive integer.", call. = FALSE)
  }
  playlist_rows <- utils::head(playlist_rows, requested_limit)
}

results <- playlist_rows
results$musicbrainz_status <- NA_character_
results$musicbrainz_recording_id <- NA_character_
results$matched_artist <- NA_character_
results$matched_title <- NA_character_
results$artist_score <- NA_real_
results$title_score <- NA_real_
results$search_score <- NA_real_
results$match_score <- NA_real_
results$candidate_count <- NA_integer_
results$exact_candidate_count <- NA_integer_

message("Searching MusicBrainz for ", nrow(results), " tracks...")
for (i in seq_len(nrow(results))) {
  query <- paste0(
    'recording:"', lucene_quote(results$Title[[i]]),
    '" AND artist:"', lucene_quote(results$Artist[[i]]), '"'
  )
  url <- paste0(
    "https://musicbrainz.org/ws/2/recording?query=",
    utils::URLencode(query, reserved = TRUE),
    "&fmt=json&limit=5"
  )
  response <- musicbrainz_get(
    url,
    file.path("data", "cache", "musicbrainz", sprintf("%03d.json", i))
  )

  if (!response$ok) {
    results$musicbrainz_status[[i]] <- paste0(
      "error_", response$status %||% "network"
    )
    message(
      "  MusicBrainz request ", i, " failed: ",
      results$musicbrainz_status[[i]], " (", response$error %||% "unknown", ")"
    )
    next
  }

  recordings <- response$body$recordings %||% list()
  scored <- score_recordings(recordings, results$Artist[[i]], results$Title[[i]])
  results$candidate_count[[i]] <- length(recordings)
  results$exact_candidate_count[[i]] <- if (is.null(scored)) {
    0L
  } else {
    length(unique(scored$musicbrainz_recording_id[
      scored$title_score >= 0.98 & scored$artist_score >= 0.98
    ]))
  }
  results$musicbrainz_status[[i]] <- classify_match(scored)

  if (!is.null(scored) && nrow(scored) > 0L) {
    scored <- scored[order(-scored$match_score), , drop = FALSE]
    best <- scored[1L, , drop = FALSE]
    for (column in c(
      "musicbrainz_recording_id", "matched_artist", "matched_title",
      "artist_score", "title_score", "search_score", "match_score"
    )) {
      results[[column]][[i]] <- best[[column]][[1L]]
    }
  }

  if (i %% 10L == 0L) message("  MusicBrainz: ", i, "/", nrow(results))
}

summarize_coverage <- function(x, label) {
  usable <- x$musicbrainz_status %in% c("exact", "matched")
  data.frame(
    playlist = label,
    total_tracks = nrow(x),
    exact = sum(x$musicbrainz_status == "exact", na.rm = TRUE),
    matched = sum(x$musicbrainz_status == "matched", na.rm = TRUE),
    ambiguous = sum(x$musicbrainz_status == "ambiguous", na.rm = TRUE),
    missing = sum(x$musicbrainz_status == "missing", na.rm = TRUE),
    errors = sum(grepl("^error_", x$musicbrainz_status)),
    usable_recording_ids = sum(usable, na.rm = TRUE),
    usable_resolution_pct = round(100 * sum(usable, na.rm = TRUE) / nrow(x), 1),
    stringsAsFactors = FALSE
  )
}

summary_rows <- lapply(split(results, results$playlist), function(x) {
  summarize_coverage(x, unique(x$playlist))
})
summary_rows[[length(summary_rows) + 1L]] <- summarize_coverage(results, "overall")
coverage_summary <- do.call(rbind, summary_rows)
row.names(coverage_summary) <- NULL

utils::write.csv(
  results,
  file.path("output", "musicbrainz_coverage_detail.csv"),
  row.names = FALSE
)
utils::write.csv(
  coverage_summary,
  file.path("output", "musicbrainz_coverage_summary.csv"),
  row.names = FALSE
)

print(coverage_summary, row.names = FALSE)
message("Wrote output/musicbrainz_coverage_detail.csv")
message("Wrote output/musicbrainz_coverage_summary.csv")
