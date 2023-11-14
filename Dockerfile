FROM fedora:39

RUN yum install -y curl gcc gcc-c++ gfortran \
                   libtool libtool-ltdl \
                   make cmake \
                   git \
                   pkgconfig \
                   sudo \
                   automake autoconf \
                   yum-utils && \
    yum clean all && rm -rf /var/cache

ARG username

RUN useradd ${username:-cwilliam} -u 1000 -m -G users,wheel && \
    echo "${username:-cwilliam} ALL=(ALL:ALL) NOPASSWD:ALL" >> /etc/sudoers && \
    chown -R ${username:-cwilliam}:${username:-cwilliam} /home/cwilliam

USER ${username:-cwilliam}

COPY --chown=1000:1000 . /home/${username:-cwilliam}

RUN set -ex ; \
   cd $HOME/phav52i ; \
   make install

WORKDIR /home/${username:-cwilliam}/pha_v52i