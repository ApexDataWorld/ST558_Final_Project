# Use an image with R and plumber preinstalled
FROM rstudio/plumber:latest

# Create working directory inside the container
WORKDIR /app

# Copy API file and data folder into the image
COPY myAPI.R /app/myAPI.R
COPY data /app/data

# Install required R packages
RUN R -e "install.packages(c('tidyverse','tidymodels','ggplot2','yardstick','readr','ranger'), repos='https://cloud.r-project.org')"

# Expose the port that plumber will run on
EXPOSE 8000

# Start the plumber API when the container runs
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('myAPI.R'); pr$run(host='0.0.0.0', port=8000, swagger=TRUE)"]
