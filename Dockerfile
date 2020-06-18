## Dockerfile

#
### wibee
#

# base
# - https://hub.docker.com/r/rocker/shiny/tags
FROM rocker/shiny-verse:3.6.3

# system libraries of general use
RUN apt-get update \
 && apt-get install -y \
      libcairo2-dev \
      libcurl4-gnutls-dev \
      libssh2-1-dev \
      libssl-dev \
      libudunits2-dev \
      libxt-dev \
      pandoc \
      pandoc-citeproc \
      sudo \
 && rm -rf /var/lib/apt/lists/*

# R packages
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('httr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sf', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflet', repos='http://cran.rstudio.com/')"

# R shiny app files
COPY app.r /srv/shiny-server/
COPY .Renviron /srv/shiny-server/
COPY www/ /srv/shiny-server/www/
RUN chown -R shiny:shiny /srv/shiny-server

# R secrets --  https://github.com/rocker-org/shiny/issues/76
COPY --chown=shiny:shiny .Renviron /home/shiny/.Renviron


