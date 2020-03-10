install.packages("tidyverse")
install.packages("remotes")
remotes::install_github("hadley/emo")
install.packages("microbenchmark")
install.packages("data.table")
install.packages(c("tvthemes", "hrbrthemes", "ggbeeswarm", "ggmosaic", "ggalluvial", "patchwork", "plotly", "rayshader", "gganimate", "magick", "ggraph", "maps"))
remotes::install_github("ryantimpe/brickr@ad3feef6dd54efce21c9f94f6a92f1b92f7a22e1")
remotes::install_github("gsimchoni/ggwithimages")
remotes::install_github("ricardo-bion/ggradar")
install.packages(c("glmnet", "randomForest", "e1071", "ranger", "gbm", "xgboost", "tidymodels", "caret"))
install.packages(c("reticulate", "keras"))
reticulate::install_miniconda("miniconda")
Sys.setenv(WORKON_HOME = "virtualenvs")
reticulate::virtualenv_create("r-reticulate", python = "miniconda/bin/python")
keras::install_keras(
  method = "virtualenv",
  conda = "miniconda/bin/conda",
  envname = "r-reticulate",
  tensorflow = "1.13.1",
  restart_session = FALSE
)
line <- "WORKON_HOME=/cloud/project/virtualenvs"
writeLines(line, ".Renviron")
dirs <- list.files(pattern = "^[0-9]")
for (dir in dirs) {
  writeLines(line, file.path(dir, ".Renviron"))
}
rstudioapi::restartSession()
reticulate::virtualenv_install("r-reticulate", "networkx")
reticulate::virtualenv_install("r-reticulate", "matplotlib")
virtualenv_install("r-reticulate", "pandas")
matplotlib <- import("matplotlib")
matplotlib$use("Agg", force = TRUE)
