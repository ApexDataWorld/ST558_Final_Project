# start from the rstudio/plumber image
FROM rstudio/plumber:latest

# install the linux libraries needed for R packages
RUN apt-get update -qq && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libpng-dev \
    pandoc \
  && rm -rf /var/lib/apt/lists/*

# install R packages required for model/API
RUN R -e "install.packages( \
    c('recipes','parsnip','workflows','dplyr','tibble', \
      'ggplot2','yardstick','readr','ranger'), \
    repos = 'https://cloud.r-project.org' \
  )"

# set a working directory inside the container
WORKDIR /app

# copy API file and data folder into the image
COPY myAPI.R /app/myAPI.R
COPY data /app/data

# open port to traffic
EXPOSE 8000

# when the container starts, start the myAPI.R script
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('myAPI.R'); pr$run(host='0.0.0.0', port=8000, swagger=TRUE)"]
