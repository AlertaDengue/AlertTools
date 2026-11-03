# The base image is pinned; the report additionally records installed R/INLA versions.
FROM rocker/r2u:24.04@sha256:dc2a96c3b368969635c25c177c64514ba8e37cf2259dd3584de7c33520191d92
RUN apt-get update && apt-get install -y --no-install-recommends \
    python3 procps ca-certificates r-cran-devtools r-cran-assertthat \
    r-cran-dbi r-cran-dplyr r-cran-forecast r-cran-lifecycle r-cran-lubridate \
    r-cran-plyr r-cran-purrr r-cran-stringr r-cran-tibble r-cran-tidyr \
    r-cran-zoo r-cran-testthat r-cran-withr r-cran-rsqlite r-cran-rpostgres \
    r-cran-fmesher r-cran-sp r-cran-rcmdcheck r-cran-lintr \
    && rm -rf /var/lib/apt/lists/*
ENV INLA_VALIDATION_ENGINE=25.12.16
# The ARM build names its executable 'inla'; the R wrapper expects 'inla.mkl'.
# Alias only the executable name; do not alter engine code or numerical settings.
RUN Rscript -e 'install.packages("https://inla.r-inla-download.org/R/testing/src/contrib/INLA_25.12.16.tar.gz", repos = NULL, type = "source"); library(INLA); INLA::inla.binary.install(os = "Ubuntu-24.04"); target <- file.path(system.file(package = "INLA"), "bin/linux/64bit/inla.mkl"); if (!file.exists(target)) stopifnot(file.symlink("inla", target)); fit <- INLA::inla(y ~ 1, data = data.frame(y = c(2, 3, 4, 2, 5, 3)), family = "poisson", num.threads = "1:1"); stopifnot(inherits(fit, "inla"))'
RUN apt-get update && apt-get install -y --no-install-recommends r-cran-sn \
    && rm -rf /var/lib/apt/lists/* \
    && Rscript -e 'fit <- INLA::inla(y ~ 1, data = data.frame(y = c(2, 3, 4, 2, 5, 3)), family = "poisson", num.threads = "1:1", control.compute = list(config = TRUE)); draws <- INLA::inla.posterior.sample(2, fit, seed = 814L, num.threads = "1:1", parallel.configs = FALSE); stopifnot(length(draws) == 2L)'
WORKDIR /workspace
