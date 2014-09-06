FROM ubuntu:14.04
MAINTAINER Fabian Linzberger <e@lefant.net>

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get -y update && \
    apt-get install -y --no-install-recommends openjdk-7-jre-headless && \
    apt-get clean

ADD http://files.gokgs.com/javaBin/kgsGtp-3.5.11.tar.gz /root/kgsGtp-3.5.11.tar.gz
WORKDIR /root
RUN tar xvzf kgsGtp-3.5.11.tar.gz

ADD kgsGtp.conf /root/kgsGtp.conf
ADD kgsGtp.sh /root/kgsGtp.sh

EXPOSE 12345
CMD ["mode=custom"]
ENTRYPOINT ["/root/kgsGtp.sh"]
