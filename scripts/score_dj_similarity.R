#!/usr/bin/env Rscript

project_library <- file.path("renv", "library")
if (dir.exists(project_library)) {
  .libPaths(c(normalizePath(project_library), .libPaths()))
}

source(file.path("R", "dj_similarity.R"), local = FALSE)

playlist_1 <- utils::read.csv(
  file.path("data", "processed", "playlist_1_features.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
playlist_2 <- utils::read.csv(
  file.path("data", "processed", "playlist_2_features.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

result <- compute_dj_similarity(playlist_1, playlist_2)

summary <- data.frame(
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
  tracks_b = result$sizes$tracks_b,
  stringsAsFactors = FALSE
)

dir.create("output", showWarnings = FALSE)
utils::write.csv(summary, file.path("output", "dj_similarity_summary.csv"),
                 row.names = FALSE)

print(summary, row.names = FALSE)
message("Wrote output/dj_similarity_summary.csv")

