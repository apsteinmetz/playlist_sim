`%||%` <- function(x, fallback) {
  if (is.null(x) || length(x) == 0L) fallback else x
}

dj_similarity_feature_columns <- function() {
  c(
    "acousticness", "danceability", "energy", "instrumentalness",
    "liveness", "speechiness", "valence", "loudness", "tempo", "key", "mode"
  )
}

dj_profile_feature_columns <- function() {
  c(
    "acousticness_mean", "acousticness_sd",
    "danceability_mean", "danceability_sd",
    "energy_mean", "energy_sd",
    "instrumentalness_mean", "instrumentalness_sd",
    "liveness_mean", "liveness_sd",
    "speechiness_mean", "speechiness_sd",
    "valence_mean", "valence_sd",
    "loudness_mean", "loudness_sd",
    "tempo_mean", "tempo_sd",
    "key_sin_mean", "key_cos_mean",
    "mode_mean",
    "tracks_matched"
  )
}

dj_profile_similarity_columns <- function() {
  setdiff(dj_profile_feature_columns(), "tracks_matched")
}

read_similarity_input <- function(x) {
  if (is.data.frame(x)) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }
  if (is.character(x) && length(x) == 1L && file.exists(x)) {
    return(readr::read_csv(x, show_col_types = FALSE))
  }
  stop("Input must be a data frame or an existing CSV file path.", call. = FALSE)
}

similarity_require_columns <- function(df) {
  missing <- setdiff(dj_similarity_feature_columns(), names(df))
  if (length(missing) > 0L) {
    stop(
      "Missing required feature columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  df
}

similarity_reference_stats <- function(df) {
  df <- similarity_require_columns(df)

  numeric_cols <- c(
    "acousticness", "danceability", "energy", "instrumentalness",
    "liveness", "speechiness", "valence", "loudness"
  )

  stats_tbl <- tibble::tibble(column = numeric_cols) |>
    dplyr::mutate(
      mean = purrr::map_dbl(column, ~ mean(df[[.x]], na.rm = TRUE)),
      sd = purrr::map_dbl(column, ~ stats::sd(df[[.x]], na.rm = TRUE)),
      sd = dplyr::if_else(is.finite(sd) & sd != 0, sd, 1)
    )

  stats <- purrr::map2(stats_tbl$mean, stats_tbl$sd, function(mean, sd) {
    list(mean = mean, sd = sd)
  }) |>
    stats::setNames(stats_tbl$column)

  tempo_log2 <- log2(pmax(df$tempo, .Machine$double.eps))
  tempo_stats <- list(
    mean = mean(tempo_log2, na.rm = TRUE),
    sd = stats::sd(tempo_log2, na.rm = TRUE)
  )
  if (!is.finite(tempo_stats$sd) || tempo_stats$sd == 0) tempo_stats$sd <- 1

  stats$tempo <- tempo_stats
  stats$tempo_anchor <- tempo_stats$mean
  stats
}

scale_with_reference <- function(x, mean, sd) {
  (x - mean) / sd
}

prepare_similarity_matrix <- function(df, reference_stats = NULL) {
  df <- similarity_require_columns(as.data.frame(df, stringsAsFactors = FALSE))
  reference_stats <- reference_stats %||% similarity_reference_stats(df)

  key_index <- as.numeric(df$key) %% 12
  key_angle <- 2 * pi * key_index / 12
  tempo_log2 <- log2(pmax(df$tempo, .Machine$double.eps))
  tempo_folded <- tempo_log2 - round(tempo_log2 - reference_stats$tempo_anchor)

  matrix_data <- tibble::tibble(
    acousticness = scale_with_reference(
      df$acousticness,
      reference_stats$acousticness$mean,
      reference_stats$acousticness$sd
    ),
    danceability = scale_with_reference(
      df$danceability,
      reference_stats$danceability$mean,
      reference_stats$danceability$sd
    ),
    energy = scale_with_reference(
      df$energy,
      reference_stats$energy$mean,
      reference_stats$energy$sd
    ),
    instrumentalness = scale_with_reference(
      df$instrumentalness,
      reference_stats$instrumentalness$mean,
      reference_stats$instrumentalness$sd
    ),
    liveness = scale_with_reference(
      df$liveness,
      reference_stats$liveness$mean,
      reference_stats$liveness$sd
    ),
    speechiness = scale_with_reference(
      df$speechiness,
      reference_stats$speechiness$mean,
      reference_stats$speechiness$sd
    ),
    valence = scale_with_reference(
      df$valence,
      reference_stats$valence$mean,
      reference_stats$valence$sd
    ),
    loudness = scale_with_reference(
      df$loudness,
      reference_stats$loudness$mean,
      reference_stats$loudness$sd
    ),
    tempo = scale_with_reference(
      tempo_folded,
      reference_stats$tempo_anchor,
      reference_stats$tempo$sd
    ),
    mode = as.numeric(df$mode),
    key_sin = sin(key_angle),
    key_cos = cos(key_angle)
  )

  as.matrix(matrix_data)
}

pairwise_distance_matrix <- function(x, y) {
  x <- as.matrix(x)
  y <- as.matrix(y)
  outer(
    seq_len(nrow(x)),
    seq_len(nrow(y)),
    Vectorize(function(i, j) {
      sqrt(sum((x[i, ] - y[j, ])^2))
    })
  )
}

energy_distance <- function(x, y) {
  if (nrow(x) == 0L || nrow(y) == 0L) return(NA_real_)
  cross <- pairwise_distance_matrix(x, y)
  within_x <- if (nrow(x) > 1L) as.matrix(stats::dist(x)) else matrix(0, 1, 1)
  within_y <- if (nrow(y) > 1L) as.matrix(stats::dist(y)) else matrix(0, 1, 1)
  2 * mean(cross) - mean(within_x) - mean(within_y)
}

centroid_distance <- function(x, y) {
  x_center <- colMeans(x, na.rm = TRUE)
  y_center <- colMeans(y, na.rm = TRUE)
  sqrt(sum((x_center - y_center)^2))
}

spread_distance <- function(x, y) {
  x_spread <- apply(x, 2, stats::sd, na.rm = TRUE)
  y_spread <- apply(y, 2, stats::sd, na.rm = TRUE)
  sqrt(sum((x_spread - y_spread)^2))
}

nearest_neighbor_distance <- function(x, y) {
  cross <- pairwise_distance_matrix(x, y)
  mean(pmin(apply(cross, 1L, min), apply(cross, 2L, min)))
}

distance_to_similarity <- function(distance) {
  if (!is.finite(distance)) return(NA_real_)
  100 / (1 + max(distance, 0))
}

compute_dj_similarity <- function(dj_a, dj_b, reference_data = NULL) {
  dj_a <- read_similarity_input(dj_a)
  dj_b <- read_similarity_input(dj_b)
  similarity_require_columns(dj_a)
  similarity_require_columns(dj_b)

  reference_data <- if (is.null(reference_data)) {
    dplyr::bind_rows(dj_a, dj_b)
  } else {
    read_similarity_input(reference_data)
  }

  reference_stats <- similarity_reference_stats(reference_data)

  matrix_a <- prepare_similarity_matrix(dj_a, reference_stats)
  matrix_b <- prepare_similarity_matrix(dj_b, reference_stats)

  raw_energy <- energy_distance(matrix_a, matrix_b)
  raw_centroid <- centroid_distance(matrix_a, matrix_b)
  raw_spread <- spread_distance(matrix_a, matrix_b)
  raw_nearest <- nearest_neighbor_distance(matrix_a, matrix_b)

  shape_similarity <- distance_to_similarity(raw_energy)
  centroid_similarity <- distance_to_similarity(raw_centroid)
  spread_similarity <- distance_to_similarity(raw_spread)
  nearest_similarity <- distance_to_similarity(raw_nearest)

  overall_similarity <- stats::weighted.mean(
    c(
      shape_similarity,
      centroid_similarity,
      spread_similarity,
      nearest_similarity
    ),
    c(0.55, 0.15, 0.10, 0.20),
    na.rm = TRUE
  )

  list(
    similarity_index = overall_similarity,
    similarity_components = list(
      shape_similarity = shape_similarity,
      centroid_similarity = centroid_similarity,
      spread_similarity = spread_similarity,
      nearest_neighbor_similarity = nearest_similarity,
      raw_distances = list(
        energy = raw_energy,
        centroid = raw_centroid,
        spread = raw_spread,
        nearest_neighbor = raw_nearest
      )
    ),
    sizes = list(
      tracks_a = nrow(dj_a),
      tracks_b = nrow(dj_b)
    )
  )
}

normalize_track_key <- function(artist, title) {
  track_key <- paste(tolower(trimws(artist)), tolower(trimws(title)), sep = "\r")
  dplyr::na_if(track_key, "\r")
}

build_dj_profiles <- function(roster, master_features) {
  roster <- read_similarity_input(roster) |>
    dplyr::as_tibble()
  master_features <- read_similarity_input(master_features) |>
    dplyr::as_tibble()

  required_roster <- c("DJ", "Artist", "Title")
  missing_roster <- setdiff(required_roster, names(roster))
  if (length(missing_roster) > 0L) {
    stop(
      "Roster is missing required columns: ",
      paste(missing_roster, collapse = ", "),
      call. = FALSE
    )
  }

  if (!"track_key" %in% names(roster)) {
    roster <- roster |>
      dplyr::mutate(track_key = normalize_track_key(Artist, Title))
  }

  if (!"track_key" %in% names(master_features)) {
    stop("Master features must include a `track_key` column.", call. = FALSE)
  }

  feature_rows <- roster |>
    dplyr::select(DJ, track_key) |>
    dplyr::filter(!is.na(track_key)) |>
    dplyr::distinct() |>
    dplyr::inner_join(master_features, by = "track_key") |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(dj_similarity_feature_columns()), ~ !is.na(.x)))

  if (nrow(feature_rows) == 0L) {
    stop("No overlapping roster tracks were found in the master feature table.", call. = FALSE)
  }

  feature_rows |>
    dplyr::mutate(
      key_index = as.numeric(key) %% 12,
      key_angle = 2 * pi * key_index / 12
    ) |>
    dplyr::group_by(DJ) |>
    dplyr::summarise(
      tracks_matched = dplyr::n(),
      acousticness_mean = mean(acousticness, na.rm = TRUE),
      acousticness_sd = stats::sd(acousticness, na.rm = TRUE),
      danceability_mean = mean(danceability, na.rm = TRUE),
      danceability_sd = stats::sd(danceability, na.rm = TRUE),
      energy_mean = mean(energy, na.rm = TRUE),
      energy_sd = stats::sd(energy, na.rm = TRUE),
      instrumentalness_mean = mean(instrumentalness, na.rm = TRUE),
      instrumentalness_sd = stats::sd(instrumentalness, na.rm = TRUE),
      liveness_mean = mean(liveness, na.rm = TRUE),
      liveness_sd = stats::sd(liveness, na.rm = TRUE),
      speechiness_mean = mean(speechiness, na.rm = TRUE),
      speechiness_sd = stats::sd(speechiness, na.rm = TRUE),
      valence_mean = mean(valence, na.rm = TRUE),
      valence_sd = stats::sd(valence, na.rm = TRUE),
      loudness_mean = mean(loudness, na.rm = TRUE),
      loudness_sd = stats::sd(loudness, na.rm = TRUE),
      tempo_mean = mean(tempo, na.rm = TRUE),
      tempo_sd = stats::sd(tempo, na.rm = TRUE),
      key_sin_mean = mean(sin(key_angle), na.rm = TRUE),
      key_cos_mean = mean(cos(key_angle), na.rm = TRUE),
      mode_mean = mean(as.numeric(mode), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with("_sd"),
        ~ dplyr::if_else(is.finite(.x) & .x != 0, .x, 1)
      )
    )
}

compute_profile_similarity_matrix <- function(profiles) {
  profiles <- read_similarity_input(profiles) |>
    dplyr::as_tibble()

  missing <- setdiff(dj_profile_feature_columns(), names(profiles))
  if (length(missing) > 0L) {
    stop(
      "Profiles are missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  feature_matrix <- profiles |>
    dplyr::select(dplyr::all_of(dj_profile_similarity_columns())) |>
    data.matrix()
  centers <- colMeans(feature_matrix, na.rm = TRUE)
  spreads <- apply(feature_matrix, 2L, stats::sd, na.rm = TRUE)
  spreads[!is.finite(spreads) | spreads == 0] <- 1
  feature_matrix <- sweep(feature_matrix, 2L, centers, "-")
  feature_matrix <- sweep(feature_matrix, 2L, spreads, "/")
  dj_names <- profiles$DJ
  distance_object <- stats::dist(feature_matrix)
  distance_matrix <- as.matrix(distance_object)
  similarity_matrix <- 100 / (1 + distance_matrix)
  diag(similarity_matrix) <- 100
  rownames(similarity_matrix) <- dj_names
  colnames(similarity_matrix) <- dj_names

  similarity_long <- tibble::as_tibble(as.data.frame(as.table(similarity_matrix))) |>
    dplyr::rename(DJ_A = Var1, DJ_B = Var2, similarity_index = Freq) |>
    dplyr::mutate(
      DJ_A = as.character(DJ_A),
      DJ_B = as.character(DJ_B)
    ) |>
    dplyr::filter(DJ_A < DJ_B)

  list(
    similarity_matrix = similarity_matrix,
    similarity_long = similarity_long,
    distances = distance_matrix
  )
}
