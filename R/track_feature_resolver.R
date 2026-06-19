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

header_value <- function(headers, name) {
  lines <- unlist(strsplit(rawToChar(headers), "\\r?\\n"))
  hits <- grep(paste0("^", name, ":"), lines, ignore.case = TRUE, value = TRUE)
  if (length(hits) == 0L) return(NULL)
  trimws(sub("^[^:]+:", "", tail(hits, 1L)))
}

request_state <- new.env(parent = emptyenv())

wait_for_slot <- function(service, min_interval) {
  last_request <- request_state[[service]]
  if (!is.null(last_request)) {
    elapsed <- as.numeric(difftime(Sys.time(), last_request, units = "secs"))
    if (elapsed < min_interval) Sys.sleep(min_interval - elapsed)
  }
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

get_json <- function(url, service, min_interval, headers = character(),
                     cache_file, max_attempts = 5L) {
  cached <- read_cache(cache_file, url)
  if (!is.null(cached)) {
    return(list(ok = TRUE, status = 200L, body = cached, cached = TRUE))
  }

  for (attempt in seq_len(max_attempts)) {
    wait_for_slot(service, min_interval)
    handle <- curl::new_handle(
      useragent = "playlist-sim/0.1 (playlist similarity research)"
    )
    if (length(headers) > 0L) {
      do.call(curl::handle_setheaders, c(list(handle = handle), as.list(headers)))
    }
    response <- tryCatch(
      curl::curl_fetch_memory(url, handle = handle),
      error = identity
    )
    request_state[[service]] <- Sys.time()

    if (inherits(response, "error")) {
      if (attempt == max_attempts) {
        return(list(
          ok = FALSE, status = NA_integer_, error = conditionMessage(response),
          body = NULL
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

    if (status == 429L && attempt < max_attempts) {
      retry_after <- suppressWarnings(as.numeric(
        header_value(response$headers, "Retry-After") %||% NA_character_
      ))
      wait_seconds <- if (is.na(retry_after)) min(60, 2^(attempt - 1L)) else retry_after
      Sys.sleep(max(wait_seconds, min_interval) + stats::runif(1L, 0, 0.25))
      next
    }
    if (status >= 500L && attempt < max_attempts) {
      Sys.sleep(min(60, 2^(attempt - 1L)) + stats::runif(1L, 0, 0.25))
      next
    }
    return(list(ok = FALSE, status = status, error = paste("HTTP", status), body = body))
  }
}

spotify_token <- function(client_id, client_secret) {
  credentials <- gsub(
    "\\s", "",
    jsonlite::base64_enc(charToRaw(paste0(client_id, ":", client_secret)))
  )
  handle <- curl::new_handle(
    useragent = "playlist-sim/0.1 (playlist similarity research)",
    postfields = "grant_type=client_credentials"
  )
  curl::handle_setheaders(
    handle,
    Authorization = paste("Basic", credentials),
    `Content-Type` = "application/x-www-form-urlencoded"
  )
  response <- curl::curl_fetch_memory(
    "https://accounts.spotify.com/api/token", handle = handle
  )
  if (response$status_code < 200L || response$status_code >= 300L) {
    stop("Spotify authentication failed with HTTP ", response$status_code, ".",
         call. = FALSE)
  }
  jsonlite::fromJSON(rawToChar(response$content), simplifyVector = FALSE)$access_token
}

score_spotify_items <- function(items, artist, title) {
  if (length(items) == 0L) return(NULL)
  rows <- lapply(items, function(item) {
    artist_names <- vapply(item$artists %||% list(), function(x) x$name, character(1))
    artist_scores <- vapply(artist_names, text_similarity, numeric(1), y = artist)
    artist_score <- if (length(artist_scores) == 0L) 0 else max(artist_scores)
    title_score <- text_similarity(item$name, title)
    data.frame(
      spotify_id = item$id %||% NA_character_,
      matched_artist = paste(artist_names, collapse = "; "),
      matched_title = item$name %||% NA_character_,
      artist_score = artist_score,
      title_score = title_score,
      match_score = 0.4 * artist_score + 0.6 * title_score,
      popularity = item$popularity %||% NA_integer_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

classify_match <- function(scored) {
  if (is.null(scored) || nrow(scored) == 0L) return("missing")
  scored <- scored[order(-scored$match_score, -scored$popularity), , drop = FALSE]
  best <- scored[1L, , drop = FALSE]
  second_score <- if (nrow(scored) >= 2L) scored$match_score[[2L]] else 0
  margin <- best$match_score[[1L]] - second_score
  if (best$title_score >= 0.98 && best$artist_score >= 0.98) return("exact")
  if (best$match_score >= 0.85 && margin >= 0.08) return("matched")
  if (best$match_score >= 0.70) return("ambiguous")
  "missing"
}

feature_columns <- c(
  "acousticness", "danceability", "energy", "instrumentalness", "key",
  "liveness", "loudness", "mode", "speechiness", "tempo", "valence"
)

resolve_spotify_track <- function(artist, title, playlist_name, rank, auth_header,
                                  interval) {
  cache_dir <- file.path("data", "cache", "spotify_recent", playlist_name)
  strict_query <- paste0("track:\"", title, "\" artist:\"", artist, "\"")
  strict_url <- paste0(
    "https://api.spotify.com/v1/search?q=",
    utils::URLencode(strict_query, reserved = TRUE), "&type=track&limit=5"
  )
  response <- get_json(
    strict_url, "spotify", interval, auth_header,
    file.path(cache_dir, sprintf("%04d_strict.json", rank))
  )
  if (!response$ok) {
    return(list(status = paste0("error_", response$status %||% "network")))
  }

  scored <- score_spotify_items(
    response$body$tracks$items %||% list(), artist, title
  )
  best_score <- if (is.null(scored)) 0 else max(scored$match_score)

  if (best_score < 0.70) {
    broad_query <- paste(artist, title)
    broad_url <- paste0(
      "https://api.spotify.com/v1/search?q=",
      utils::URLencode(broad_query, reserved = TRUE), "&type=track&limit=5"
    )
    broad_response <- get_json(
      broad_url, "spotify", interval, auth_header,
      file.path(cache_dir, sprintf("%04d_broad.json", rank))
    )
    if (broad_response$ok) {
      broad_scored <- score_spotify_items(
        broad_response$body$tracks$items %||% list(), artist, title
      )
      if (!is.null(broad_scored)) {
        scored <- if (is.null(scored)) broad_scored else rbind(scored, broad_scored)
        scored <- scored[!duplicated(scored$spotify_id), , drop = FALSE]
      }
    }
  }

  status <- classify_match(scored)
  if (is.null(scored) || nrow(scored) == 0L) return(list(status = status))
  scored <- scored[order(-scored$match_score, -scored$popularity), , drop = FALSE]
  best <- scored[1L, , drop = FALSE]
  list(
    status = status,
    spotify_id = best$spotify_id[[1L]],
    matched_artist = best$matched_artist[[1L]],
    matched_title = best$matched_title[[1L]],
    artist_score = best$artist_score[[1L]],
    title_score = best$title_score[[1L]],
    match_score = best$match_score[[1L]]
  )
}

fetch_reccobeats_features <- function(spotify_ids, playlist_name, batch_start,
                                      interval) {
  if (length(spotify_ids) == 0L) {
    return(list(status = "missing", body = NULL))
  }
  id_query <- paste(
    paste0("ids=", utils::URLencode(spotify_ids, reserved = TRUE)),
    collapse = "&"
  )
  url <- paste0("https://api.reccobeats.com/v1/audio-features?", id_query)
  response <- get_json(
    url, "reccobeats", interval, character(),
    file.path(
      "data", "cache", "reccobeats_recent", playlist_name,
      sprintf("batch_%04d.json", batch_start)
    )
  )
  response
}
