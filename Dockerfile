FROM fedora:39

RUN yum install -y \
    gcc gcc-c++ gfortran \
    gawk make \
    tcsh \
    sudo \
    && yum clean all \
    && rm -rf /var/cache/yum

ARG username=cwilliam

RUN useradd ${username} -u 1000 -m -G users,wheel && \
    echo "${username} ALL=(ALL:ALL) NOPASSWD:ALL" >> /etc/sudoers

USER ${username}
ENV USHCNBASE=/home/${username}

COPY --chown=1000:1000 . ${USHCNBASE}

RUN set -ex; \
    mkdir -p ${USHCNBASE}/bin; \
    mkdir -p ${USHCNBASE}/data/realworld/meta; \
    mkdir -p ${USHCNBASE}/data/realworld/monthly/raw; \
    mkdir -p ${USHCNBASE}/data/realworld/monthly/his; \
    mkdir -p ${USHCNBASE}/data/realworld/log; \
    csh ${USHCNBASE}/phav52d/scripts/source_compiles/compile.csh FAST 21_21 ${USHCNBASE}

WORKDIR ${USHCNBASE}
