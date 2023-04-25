# Ubuntu 20.04 LTS (Focal Fossa), supported until April 2025.
ARG UBUNTU_VERSION=22.04
ARG NGIXN_VERSION=1.24.0
ARG GHC_VERSION=9.4.4
ARG CABAL_VERSION=3.6.2.0
ARG HLINT_VERSION=3.5
ARG HLS_VERSION=1.10.0.0

FROM ubuntu:$UBUNTU_VERSION as anime-rss-base
RUN apt-get update \
    && apt-get install -y \
       build-essential curl locales libgmp-dev libtinfo-dev zlib1g-dev git \
       libpq-dev \
    && curl -fsSL https://deb.nodesource.com/setup_19.x | bash - \
    && apt-get install -y nodejs \
    && localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8 \
    && rm -rf /var/lib/apt/lists/* /usr/share/doc /usr/share/man
ENV LANG=en_US.UTF-8

FROM anime-rss-base as anime-rss-devel-base
ARG CABAL_INDEX_STATE
ARG CABAL_VERSION
ARG GHC_VERSION
ARG HLINT_VERSION
ENV GHCUP_INSTALL_BASE_PREFIX="/opt/"
ENV PATH="/opt/.ghcup/bin:$PATH"
RUN mkdir -p /opt/.ghcup/bin/ \
    && curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup --output /opt/.ghcup/bin/ghcup \
    && chmod +x /opt/.ghcup/bin/ghcup \
    && ghcup install ghc $GHC_VERSION \
    && ghcup set ghc $GHC_VERSION \
    && ghcup install cabal $CABAL_VERSION \
    && ghcup set cabal $CABAL_VERSION
ENV PATH="/opt/hlint/:$PATH"
RUN HLIN_TMP=$(mktemp -d ".hlint-XXXXXX") \
    && curl -L --proto '=https' -o "${HLIN_TMP}/hlint.tar.gz" https://github.com/ndmitchell/hlint/releases/download/v${HLINT_VERSION}/hlint-${HLINT_VERSION}-x86_64-linux.tar.gz \
    && mkdir -p /opt/hlint/ \
    && tar xf "${HLIN_TMP}/hlint.tar.gz" -C /opt/hlint/ \
    && rm -r -f "${HLIN_TMP}" \
    && cabal update \
    && cabal install --install-method=copy --installdir=/usr/bin --install-method=copy cabal-fmt \
    && rm -r -f /root/.cabal \
    && rm -r -f /opt/ghcup/cache \
    && rm -r -f /opt/ghcup/tmp \
    && echo root:password | chpasswd

FROM anime-rss-base as anime-rss-service
COPY anime-rss-dist/bin/ /anime-rss-service/bin/
RUN useradd -U -u 1000 -d /anime-rss-service anime-rss-service
USER anime-rss-service
WORKDIR /anime-rss-service/
CMD /anime-rss-service/bin/anime-rss

FROM nginx:$NGIXN_VERSION as anime-rss-nginx
COPY anime-rss-dist/frontend/ /frontend/
CMD ["nginx", "-g", "daemon off;"]

FROM anime-rss-devel-base as anime-rss-devel
ARG USER=devel
ARG UID=1000
ARG HLS_VERSION
RUN useradd -r -u "$UID" -U -m -s /bin/bash "$USER" \
    && sed -e "s|PS1='|PS1='[D] |g" -i /home/$USER/.bashrc \
    && ghcup install hls $HLS_VERSION \
    && ghcup set hls $HLS_VERSION \
    && rm -r -f /opt/.ghcup/cache \
    && rm -r -f /opt/.ghcup/tmp \
    && echo $USER:password | chpasswd

USER "$USER"
RUN mkdir /home/$USER/.cabal
WORKDIR "/home/$USER"
RUN cabal update
