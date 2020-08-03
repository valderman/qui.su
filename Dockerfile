FROM debian:stable AS build
ARG DEBIAN_FRONTEND=noninteractive
ARG USER=hootsman

# Install system deps
RUN apt-get update && \
    apt-get -y install npm wget libgmp-dev zlib1g-dev cabal-install ghc && \
    apt-get clean && \
    cabal update && \
    cabal install cabal-install && \
    apt-get -y --autoremove remove cabal-install


# Set up users and directories
RUN mkdir -p /app/{client/src,client/public,server}

# Install npm deps
WORKDIR /app
COPY ./client/package.json ./client/
RUN cd ./client && npm install


# Build server
WORKDIR /app
COPY ./server/hootsman.cabal ./server/
COPY ./server/src            ./server/src
COPY ./server/app            ./server/app
COPY ./Makefile              ./
RUN export PATH=$HOME/.cabal/bin:$PATH && \
    cabal update && make -j server

# Build client
WORKDIR /app
COPY ./client/src     ./client/src
COPY ./client/public  ./client/public
RUN make -j client

VOLUME /app


# Set up app image
FROM debian:stable
ARG USER=hootsman
RUN apt-get update && \
    apt-get -y install libgmp10 ca-certificates && \
    apt-get clean

RUN mkdir -p /data/upload /app/static && \
    ln -s /data/upload /app/static/upload && \
    useradd $USER -d /data && \
    chown -R $USER:$USER /data
COPY --from=build /app/_app /app
RUN chown -R ${USER}:${USER} /app

USER ${USER}:${USER}
WORKDIR /data
VOLUME /data
ENV HTTP_PORT 8080
EXPOSE ${HTTP_PORT}
CMD HOOTSMAN_STATIC_DIR=/app/static \
    HOOTSMAN_HTTP_PORT=$HTTP_PORT \
    /app/hootsman-exe
