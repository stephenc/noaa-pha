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

RUN useradd user -u 1000 -m -G users,wheel && \
    echo "user ALL=(ALL:ALL) NOPASSWD:ALL" >> /etc/sudoers && \
    chown -R user:user /home/user

USER user

COPY --chown=1000:1000 . /home/user

WORKDIR /home/user

RUN set -ex ; \
   cd phav52i ; \
   make install