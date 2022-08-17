FROM rocker/rstudio:1e2f341a5e94
LABEL maintainer="Alice Lepissier <alice.lepissier@gmail.com>"


##### START Binder compatibility
# from https://mybinder.readthedocs.io/en/latest/tutorials/dockerfile.html
ARG NB_USER
ARG NB_UID
ENV USER ${NB_USER}
ENV NB_UID ${NB_UID}
ENV HOME /home/${NB_USER}

COPY . ${HOME}/work
USER root
RUN chown -R ${NB_UID} ${HOME}
##### END Binder compatibility code