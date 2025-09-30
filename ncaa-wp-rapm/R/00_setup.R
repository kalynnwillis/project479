# Create directories and initialize renv if desired
required_dirs <- c(
  "data/raw",
  "data/interim",
  "data/processed",
  "figs",
  "tables"
)

for (d in required_dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Initialize renv if lockfile exists, otherwise initialize a new one
if (file.exists("renv.lock")) {
  renv::restore()
} else {
  renv::init(bare = TRUE)
}

message("Setup complete.")
