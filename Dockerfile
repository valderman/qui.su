FROM debian:stable AS build
ARG DEBIAN_FRONTEND=noninteractive
ARG USER=hootsman
ARG STACK_RESOLVER=lts-14.17


# Install system deps
RUN apt-get update && \
    apt-get -y install npm wget libgmp-dev zlib1g-dev && \
    apt-get clean


# Set up users and directories
RUN mkdir -p /stack /app/{client/src,client/public,server} && \
    useradd $USER -d /app && \
    chown -R $USER:$USER /stack /app
USER ${USER}:${USER}


# Set up stack
WORKDIR /stack
RUN wget https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-linux-x86_64-static.tar.gz && \
    tar -xzf stack-2.1.3-linux-x86_64-static.tar.gz && \
    rm stack-2.1.3-linux-x86_64-static.tar.gz && \
    mv stack-2.1.3-linux-x86_64-static/* ./ && \
    rmdir stack-2.1.3-linux-x86_64-static
RUN ./stack setup --resolver $STACK_RESOLVER && ./stack update


# Install npm deps
WORKDIR /app
COPY --chown=${USER}:${USER} ./client/package.json ./client/package-lock.json \
     ./client/
RUN cd ./client && npm install


# Install stack deps
WORKDIR /app
COPY --chown=${USER}:${USER} ./server/stack.yaml ./server/hootsman.cabal \
     ./server/
RUN cd ./server && /stack/stack install --only-dependencies


# Build and export the application
WORKDIR /app
COPY --chown=${USER}:${USER} ./server/src     ./server/src
COPY --chown=${USER}:${USER} ./server/app     ./server/app
RUN PATH=$PATH:/stack make server

COPY --chown=${USER}:${USER} ./Makefile       ./
COPY --chown=${USER}:${USER} ./client/src     ./client/src
COPY --chown=${USER}:${USER} ./client/public  ./client/public
RUN PATH=$PATH:/stack make

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

USER ${USER}:${USER}
WORKDIR /data
VOLUME /data
ENV HTTP_PORT 8080
EXPOSE ${HTTP_PORT}
CMD HOOTSMAN_STATIC_DIR=/app/static \
    HOOTSMAN_HTTP_PORT=$HTTP_PORT \
    /app/hootsman-exe
