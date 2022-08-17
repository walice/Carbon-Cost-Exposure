FROM jupyter/scipy-notebook:70178b8e48d7
LABEL maintainer="Alice Lepissier <alice.lepissier@gmail.com>"


# Fix DL4006
SHELL ["/bin/bash", "-o", "pipefail", "-c"]


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


#USER root


##### START R code
# R pre-requisites
RUN apt-get update --yes && \
    apt-get install --yes --no-install-recommends \
    fonts-dejavu \
    unixodbc \
    unixodbc-dev \
    r-cran-rodbc \
    gfortran \
    gcc \
    gnupg && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Fix for devtools https://github.com/conda-forge/r-devtools-feedstock/issues/4
RUN ln -s /bin/tar /bin/gtar


USER $NB_UID


# R packages including IRKernel which gets installed globally
RUN conda install --quiet --yes \
    'r-base' \
    'r-caret' \
    'r-crayon' \
    'r-devtools' \
    'r-forecast' \
    'r-hexbin' \
    'r-htmltools' \
    'r-htmlwidgets' \
    'r-irkernel' \
    'r-nycflights13' \
    'r-randomforest' \
    'r-rcurl' \
    'r-rmarkdown' \
    'r-rodbc' \
    'r-rsqlite' \
    'r-shiny' \
    'r-tidymodels' \
    'r-tidyverse' \
    'unixodbc' && \
    conda clean --all -f -y && \
    fix-permissions "${CONDA_DIR}" && \
    fix-permissions "/home/${NB_USER}"

# Install e1071 R package (dependency of the caret R package)
RUN conda install --quiet --yes 'r-e1071' && \
    conda clean --all -f -y && \
    fix-permissions "${CONDA_DIR}" && \
    fix-permissions "/home/${NB_USER}"
##### END R code


USER root


##### START RStudio code
RUN \
    # download R studio
    curl --silent -L --fail \
        https://s3.amazonaws.com/rstudio-ide-build/server/bionic/amd64/rstudio-server-1.2.1578-amd64.deb > /tmp/rstudio.deb && \
    \
    # install R studio
    apt-get update && \
    apt-get install -y --no-install-recommends /tmp/rstudio.deb && \
    rm /tmp/rstudio.deb && \
    apt-get clean && rm -rf /var/lib/apt/lists/*
##### END RStudio code


USER ${NB_USER}


##### Jupyter notebook extensions & packages
RUN pip install \
        cookiecutter==1.7.3 \
        jupyterlab_vim==0.14.2 && \
    \
    pip install \ 
        jupyter-server-proxy==1.6.0 \
        jupyter-rsession-proxy==1.2.0 && \
    jupyter labextension install @jupyterlab/server-proxy && \
    \
    pip install jupyterlab_latex==2.0.0 && \
    jupyter labextension install @jupyterlab/latex && \
    \
    pip install \
        jupyterlab-git==0.32.2 \
        nbgitpuller==0.10.1 && \
    jupyter serverextension enable --sys-prefix nbgitpuller && \
    jupyter lab build