FROM fpco/stack-build:lts-11.22 as build
RUN mkdir /opt/build
WORKDIR /opt/build

# Install package index
COPY ./stack.yaml /opt/build/
RUN stack update

# Install dependencies and build plan
COPY ./package.yaml ./Setup.hs ./healths.cabal /opt/build/
RUN stack install --only-dependencies

# Build actual executable
COPY ./src/ ./app/ ./test/ /opt/build/
RUN stack build --system-ghc && stack install --local-bin-path /opt/build

FROM ubuntu:16.04
RUN mkdir -p /opt/app
WORKDIR /opt/app
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev

COPY --from=build /opt/build/healths .

ENV HEALTHS_PORT 8080
ENV HEALTHS_INFLUXHOST influxdb
ENV HEALTHS_DATABASE /data/state.json
EXPOSE 8080
ENTRYPOINT ["/opt/app/healths"]
CMD ["-p", "${HEALTHS_PORT}", "-i", "${HEALTHS_INFLUXHOST}", "-d", "${HEALTHS_DATABASE}"]
