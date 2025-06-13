# Build stage
FROM haskell:9.10 AS builder

RUN apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    apt-get update --allow-releaseinfo-change && \
    apt-get install -y \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

RUN cabal update

# Copy and build
WORKDIR /app
COPY planets.cabal /app
COPY CHANGELOG.md /app
COPY LICENSE /app

RUN cabal build all --only-dependencies

COPY app /app/app

RUN cabal build all

# Install all executables to /usr/local/bin
RUN cabal install exe:planets --installdir=/usr/local/bin

# Runtime stage
FROM debian:bullseye-slim

RUN apt-get update && \
    apt-get install -y \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Copy all binaries from builder
COPY --from=builder /usr/local/bin/planets /usr/local/bin/planets

WORKDIR /app

# Default entrypoint is the main executable
ENTRYPOINT ["/usr/local/bin/planets"]