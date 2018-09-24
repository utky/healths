FROM fpco/stack-build:lts-11.22 as build
RUN mkdir /opt/build
COPY . /opt/build
WORKDIR /opt/build
RUN stack build --system-ghc
RUN stack install --local-bin-path /opt/build

FROM ubuntu:16.04
RUN mkdir -p /opt/app
WORKDIR /opt/app
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev

COPY --from=build /opt/build/healths .
CMD ["/opt/app/healths"]
