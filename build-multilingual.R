# Build multilingual pkgdown sites
#
# This script builds the English site first, then builds each translated
# language site into a subdirectory (e.g., docs/es/, docs/fr/, etc.).
#
# Usage:
#   Rscript build-multilingual.R
#
# The script temporarily swaps in translated content (vignettes, README,
# _pkgdown.yml) for each language build, then restores the originals.

library(pkgdown)
library(fs)

languages <- c("es", "fr", "pt", "ru")

# --- Step 1: Build English site ---
cat("Building English site...\n")
build_site_github_pages(new_process = FALSE, install = FALSE)
cat("English site built.\n")

# --- Step 2: Build each language site ---
for (lang in languages) {
  cat(sprintf("Building %s site...\n", lang))

  lang_dir <- file.path("translations", lang)

  # Backup originals
  file_copy("_pkgdown.yml", "_pkgdown.yml.bak", overwrite = TRUE)
  file_copy("README.md", "README.md.bak", overwrite = TRUE)

  vignette_files <- dir_ls("vignettes", glob = "*.Rmd")
  for (f in vignette_files) {
    file_copy(f, paste0(f, ".bak"), overwrite = TRUE)
  }

  # Copy translated files into place
  file_copy(
    file.path(lang_dir, "_pkgdown.yml"),
    "_pkgdown.yml",
    overwrite = TRUE
  )
  file_copy(
    file.path(lang_dir, "README.md"),
    "README.md",
    overwrite = TRUE
  )

  translated_vignettes <- dir_ls(
    file.path(lang_dir, "vignettes"),
    glob = "*.Rmd"
  )
  for (f in translated_vignettes) {
    file_copy(f, file.path("vignettes", path_file(f)), overwrite = TRUE)
  }

  # Build to language subdirectory
  tryCatch(
    {
      build_site(
        override = list(destination = file.path("docs", lang))
      )
      cat(sprintf("%s site built successfully.\n", lang))
    },
    error = function(e) {
      cat(sprintf("Error building %s site: %s\n", lang, e$message))
    }
  )

  # Restore originals
  file_move("_pkgdown.yml.bak", "_pkgdown.yml")
  file_move("README.md.bak", "README.md")

  for (f in vignette_files) {
    file_move(paste0(f, ".bak"), f)
  }

  cat(sprintf("%s site restored.\n", lang))
}

cat("All sites built.\n")
