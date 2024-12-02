FROM rstudio/plumber

RUN apt-get update -qq && apt-get install -y libssl-dev libcurl4-gnutls-dev libpng-dev

RUN R -e "install.packages(c('tidyverse', 'tidymodels', 'ranger'))"

COPY / /

EXPOSE 9651

ENTRYPOINT ["R", "-e", \
"pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=9651)"]

