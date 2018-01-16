FROM debian:stable-slim

# Install stack dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      curl \
      ca-certificates \
      g++ \
      gcc \
      git \
      gnupg \
      libc6-dev \
      libffi-dev \
      libgmp-dev \
      make \
      xz-utils \
      zlib1g-dev

# Install stack
RUN curl -L https://github.com/commercialhaskell/stack/releases/download/v1.6.3/stack-1.6.3-linux-x86_64-static.tar.gz -o /usr/local/bin/stack.tar.gz && \
    tar xf /usr/local/bin/stack.tar.gz -C /usr/local/bin --strip-components=1 stack-1.6.3-linux-x86_64-static/stack && \
    chmod +x /usr/local/bin/stack

# Install extra libraries for ActiveHs dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      libtinfo-dev # for hint
      libpcre3-dev # for pcre-light
      pkg-config   # for pcre-light

# Build ActiveHs dependencies
COPY activehs.cabal stack.yaml /development/init/
RUN stack --stack-yaml=/development/init/stack.yaml setup
RUN stack --stack-yaml=/development/init/stack.yaml --no-system-ghc build --only-dependencies

# Make convenience scripts available
RUN ln -s /development/activehs/docker/container-script/build /development/activehs/docker/container-script/run /usr/local/bin/
    
# Install ActiveHs tool dependencies, locales and useful tools
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      dvipng \
      graphviz \
      locales \
      screen \
      texlive-latex-recommended \
      texlive-latex-extra

# Set locale         
RUN echo en_US.UTF-8 UTF-8 >> /etc/locale.gen && \
    locale-gen
ENV LC_ALL en_US.UTF-8

VOLUME /development/activehs
VOLUME /activehs-server

WORKDIR /activehs-server
