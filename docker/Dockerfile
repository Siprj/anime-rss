# Ubuntu 20.04 LTS (Focal Fossa), supported until April 2025.
ARG UBUNTU_VERSION=20.04
ARG GHC_VERSION=9.2.2
ARG CABAL_VERSION=3.6.2.0
# Given that BuildKit can build multiple stages at the same time, it might be a good idea
# to keep the number of jobs quite low depending on the number of stages that can
# potentially run cabal.
ARG CABAL_INDEX_STATE=2022-06-15T00:00:00Z
ARG HLINT_VERSION=3.4
# The version (image tag) of the devel image used for building the production image.

FROM ubuntu:$UBUNTU_VERSION as devel-base
ARG CABAL_INDEX_STATE
ARG CABAL_VERSION
ARG GHC_VERSION
ARG HLINT_VERSION
RUN apt-get update \
    && apt-get install -y \
       build-essential curl locales libgmp-dev libtinfo-dev zlib1g-dev git \
       libpq-dev \
    && localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8 \
    && rm -rf /var/lib/apt/lists/* /usr/share/doc /usr/share/man
ENV LANG=en_US.UTF-8
ENV PATH="/opt/ghcup/bin:$PATH"
RUN mkdir -p /opt/ghcup/bin/ \
    && curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup --output /opt/ghcup/bin/ghcup \
    && chmod +x /opt/ghcup/bin/ghcup \
    && ghcup install ghc $GHC_VERSION -i /opt/ghc/ \
    && ghcup install cabal $CABAL_VERSION -i /opt/cabal/bin/
ENV PATH="/opt/ghc/bin:/opt/cabal/bin:$PATH"
RUN cabal update --index-state="$CABAL_INDEX_STATE" \
    && cabal install --installdir=/usr/bin\
       hlint-$HLINT_VERSION \
    && git clone https://github.com/Siprj/cabal-fmt \
    && (cd cabal-fmt && cabal install --install-method=copy --installdir=/usr/bin) \
    && rm -r -f cabal-fmt && rm -r -f /root/.cabal \
    && echo root:password | chpasswd

FROM devel-base as devel
ARG USER=devel
ARG UID=1000
RUN useradd -r -u "$UID" -U -m -s /bin/bash "$USER" \
    && sed -e "s|PS1='|PS1='[D] |g" -i /home/$USER/.bashrc \
    && echo $USER:password | chpasswd
USER "$USER"
RUN mkdir /home/$USER/.cabal
WORKDIR "/home/$USER"
RUN cabal update
