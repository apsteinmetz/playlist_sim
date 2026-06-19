#!/usr/bin/env Rscript

project_library <- file.path("renv", "library")
if (dir.exists(project_library)) {
  .libPaths(c(normalizePath(project_library), .libPaths()))
}

required_packages <- c("readr", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing packages: ", paste(missing_packages, collapse = ", "), ".",
    call. = FALSE
  )
}

source(file.path("R", "dj_similarity.R"), local = FALSE)

playlist_1 <- readr::read_csv(
  file.path("data", "processed", "playlist_1_features.csv"),
  show_col_types = FALSE
)
playlist_2 <- readr::read_csv(
  file.path("data", "processed", "playlist_2_features.csv"),
  show_col_types = FALSE
)

result <- compute_dj_similarity(playlist_1, playlist_2)

summary <- tibble::tibble(
  similarity_index = result$similarity_index,
  shape_similarity = result$similarity_components$shape_similarity,
  centroid_similarity = result$similarity_components$centroid_similarity,
  spread_similarity = result$similarity_components$spread_similarity,
  nearest_neighbor_similarity = result$similarity_components$nearest_neighbor_similarity,
  energy_distance = result$similarity_components$raw_distances$energy,
  centroid_distance = result$similarity_components$raw_distances$centroid,
  spread_distance = result$similarity_components$raw_distances$spread,
  nearest_neighbor_distance = result$similarity_components$raw_distances$nearest_neighbor,
  tracks_a = result$sizes$tracks_a,
  tracks_b = result$sizes$tracks_b
)

dir.create("output", showWarnings = FALSE)
readr::write_csv(summary, file.path("output", "dj_similarity_summary.csv"))

summary

