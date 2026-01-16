FROM haskell:9.12.2-slim-bookworm AS builder
RUN echo cabal update date: 2026-01-05
RUN cabal update --verbose=2

WORKDIR /hs-socket-check
COPY --link ./hs-socket-check.cabal ./
RUN cabal update --verbose=2
RUN cabal build --only-dependencies
COPY --link ./app/ ./app/
COPY --link ./src/ ./src/

RUN cabal build
RUN cp $( cabal list-bin hs-socket-check | fgrep --max-count=1 hs-socket-check ) /usr/local/bin/
RUN which hs-socket-check
RUN hs-socket-check --help || true


FROM debian:bookworm-slim
COPY --link --from=builder /usr/local/bin/hs-socket-check /usr/local/bin/

CMD ["/usr/local/bin/hs-socket-check"]
