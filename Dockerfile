## -*- docker-image-name: "tippenein/hasken" -*-

FROM fpco/stack-build

WORKDIR /opt/server

COPY ./hasken.cabal /opt/server/hasken.cabal

COPY . /opt/server
RUN cabal update && cabal install --only-dependencies -j4

# RUN stack install

ENV HASKEN_ENV=prod
ENV PORT=8080
ENV PATH=/root/.cabal/bin:$PATH
EXPOSE 8080

ENTRYPOINT hasken serve
