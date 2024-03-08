FROM haskell:9.8.1

WORKDIR /opt/rinha-haskell

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./rinha-haskell.cabal /opt/rinha-haskell/rinha-haskell.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/rinha-haskell
RUN cabal install

CMD ["rinha-haskell"]