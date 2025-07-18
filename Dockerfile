FROM haskell:9.6

WORKDIR /app

COPY . .

RUN cabal update && cabal install balatro-server --installdir=/app/bin --install-method=copy

EXPOSE 3000

CMD ["/app/bin/balatro-server"]
