`%||%` <- function(x, fallback) {
  if (is.null(x) || length(x) == 0L) fallback else x
}

dj_similarity_feature_columns <- function() {
  c(
    "acousticness", "danceability", "energy", "instrumentalness",
    "liveness", "speechiness", "valence", "loudness", "tempo", "key", "mode"
  )
}

read_similarity_input <- function(x) {
  if (is.data.frame(x)) return(as.data.frame(x, stringsAsFactors = FALSE))
  if (is.character(x) && length(x) == 1L && file.exists(x)) {
    return(utils::read.csv(x, stringsAsFactors = FALSE, check.names = FALSE))
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
  stats <- list()
  numeric_cols <- c(
    "acousticness", "danceability", "energy", "instrumentalness",
    "liveness", "speechiness", "valence", "loudness"
  )
  for (column in numeric_cols) {
    stats[[column]] <- list(
      mean = mean(df[[column]], na.rm = TRUE),
      sd = stats::sd(df[[column]], na.rm = TRUE)
    )
    if (!is.finite(stats[[column]]$sd) || stats[[column]]$sd == 0) {
      stats[[column]]$sd <- 1
    }
  }
  tempo_log2 <- log2(pmax(df$tempo, .Machine$double.eps))
  stats$tempo <- list(
    mean = mean(tempo_log2, na.rm = TRUE),
    sd = stats::sd(tempo_log2, na.rm = TRUE)
  )
  if (!is.finite(stats$tempo$sd) || stats$tempo$sd == 0) stats$tempo$sd <- 1
  stats$tempo_anchor <- stats$tempo$mean
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

  matrix_data <- data.frame(
    acousticness = scale_with_reference(
      df$acousticness, reference_stats$acousticness$mean,
      reference_stats$acousticness$sd
    ),
    danceability = scale_with_reference(
      df$danceability, reference_stats$danceability$mean,
      reference_stats$danceability$sd
    ),
    energy = scale_with_reference(
      df$energy, reference_stats$energy$mean, reference_stats$energy$sd
    ),
    instrumentalness = scale_with_reference(
      df$instrumentalness, reference_stats$instrumentalness$mean,
      reference_stats$instrumentalness$sd
    ),
    liveness = scale_with_reference(
      df$liveness, reference_stats$liveness$mean, reference_stats$liveness$sd
    ),
    speechiness = scale_with_reference(
      df$speechiness, reference_stats$speechiness$mean,
      reference_stats$speechiness$sd
    ),
    valence = scale_with_reference(
      df$valence, reference_stats$valence$mean, reference_stats$valence$sd
    ),
    loudness = scale_with_reference(
      df$loudness, reference_stats$loudness$mean, reference_stats$loudness$sd
    ),
    tempo = scale_with_reference(
      tempo_folded, reference_stats$tempo_anchor, reference_stats$tempo$sd
    ),
    mode = as.numeric(df$mode),
    key_sin = sin(key_angle),
    key_cos = cos(key_angle),
    stringsAsFactors = FALSE
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

  if (is.null(reference_data)) {
    reference_data <- rbind(dj_a, dj_b)
  } else {
    reference_data <- read_similarity_input(reference_data)
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
      shape_similarity, centroid_similarity, spread_similarity,
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
