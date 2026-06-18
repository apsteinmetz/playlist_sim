# Playlist Similarity

An R project for comparing two song playlists and explaining their similarity.

## Project structure

- `R/`: reusable functions
- `scripts/`: runnable analysis and application scripts
- `tests/testthat/`: automated tests
- `data/raw/`: source playlist data (not committed)
- `data/processed/`: cleaned data (not committed)
- `output/`: generated results (not committed)
- `docs/`: project documentation

## Create sample playlists

From the project root, run:

```r
Rscript --vanilla scripts/create_sample_playlists.R
```

The optional first argument is the random seed. The default is `20260618`.
The script randomly selects two eligible DJs, takes their 1,000 most recent
distinct tracks, and randomizes the order of that recency-limited pool before
writing `data/playlist_1.csv` and `data/playlist_2.csv`.

## Measure music-feature API coverage

ReccoBeats accepts Spotify track IDs but does not currently provide an
artist/title search endpoint. The script uses a project-local `.Renviron`, the
file named by `PLAYLIST_SIM_ENV_FILE`, or this machine's existing Google Drive
environment file. Then run:

```r
Rscript --vanilla -e "source('scripts/measure_api_coverage.R', echo = FALSE)"
```

The script searches Spotify for each artist/title pair and retrieves audio
features from ReccoBeats. It caches successful responses, sends requests
sequentially, observes `Retry-After`, and backs off on rate-limit and server
errors. Results are written to `output/` and `data/processed/`.

For the production sample, search the 1,000-track recency queues until 100
complete feature records per DJ are found:

```r
Rscript --vanilla -e "source('scripts/build_feature_playlists.R', echo = FALSE)"
```

The collector works in batches of 25, checkpoints after every batch, and stops
each DJ independently once the target is reached.

## Evaluate MusicBrainz resolution

Run the MusicBrainz recording-resolution pilot with:

```r
Rscript --vanilla scripts/evaluate_musicbrainz_coverage.R
```

The script uses a descriptive user agent, sends no more than one request per
1.1 seconds, caches successful searches, and retries `503` and other transient
failures with backoff. MusicBrainz IDs can be joined to the frozen
AcousticBrainz feature dumps; MusicBrainz itself does not provide audio
features.

## Compare two DJs

The DJ similarity function lives in `R/dj_similarity.R`. It compares two
feature tables as distributions over the matched ReccoBeats features and
returns an index from 0 to 100, plus component scores for:

- overall feature-shape similarity
- centroid similarity
- spread similarity
- nearest-neighbor similarity

Run the current pair with:

```r
Rscript --vanilla -e "source('scripts/score_dj_similarity.R', echo = FALSE)"
```

The runner reads `data/processed/playlist_1_features.csv` and
`data/processed/playlist_2_features.csv` and writes
`output/dj_similarity_summary.csv`.
