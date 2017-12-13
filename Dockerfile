FROM rocker/shiny

# Add application codebase
ADD ./ /srv/shiny-server/scorecard_builder_app
RUN chmod a+rwx -R /srv/shiny-server/scorecard_builder_app

# Temporal fix for RStan package
# http://discourse.mc-stan.org/t/error-when-installing-rstan-2-16-2/1730
# https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Mac-or-Linux
RUN echo "\nCXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function" >> /etc/R/Makeconf
RUN echo "\nCXXFLAGS+=-DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION" >> /etc/R/Makeconf
RUN echo "\nCPPFLAGS+=-DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION" >> /etc/R/Makeconf

# Install additional Ubuntu packages
RUN sudo apt-get update && sudo apt-get -y install libssl-dev
RUN sudo apt-get -y install libfftw3-3 libfftw3-bin libfftw3-dev libfftw3-double3

# Install additional R packages

RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('shinydashboard')"
RUN R -e "install.packages('polycor')"
RUN R -e "install.packages('gdata')"
RUN R -e "install.packages('pROC')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('mlbench')"
RUN R -e "install.packages('caret')"
RUN R -e "install.packages('e1071')"
RUN R -e "install.packages('woe')"
RUN R -e "install.packages('data.table')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('corrplot')"
RUN R -e "install.packages('mlbench')"
RUN R -e "install.packages('InformationValue')"
RUN R -e "install.packages('shinythemes')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('shinyjs')"
RUN R -e "install.packages('shinydashboard')"
RUN R -e "install.packages('shinyBS')"
