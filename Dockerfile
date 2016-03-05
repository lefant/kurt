FROM ubuntu:14.04
MAINTAINER Fabian Linzberger <e@lefant.net>

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get -y update && \
    apt-get install -y --no-install-recommends haskell-platform && \
    apt-get clean

RUN cabal update

ADD src /root/src
ADD Setup.hs /root/Setup.hs
ADD kurt.cabal /root/kurt.cabal
ADD COPYRIGHT /root/COPYRIGHT

WORKDIR /root
RUN cabal install

RUN apt-get -y install netcat && apt-get clean

ENV GHCRTS -N
CMD ["/root/.cabal/bin/kurt"]
#CMD ["nc.traditional", "-w", "2", "kgsgtp_1", "12345", "-e", "/root/.cabal/bin/kurt"]
