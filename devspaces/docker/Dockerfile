# Pull base image.
FROM phusion/baseimage:0.11

LABEL description="Devspaces implementation for SciPio ERP"

# Install deps
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get -y --allow-downgrades --allow-remove-essential --allow-change-held-packages install \
    apt-transport-https \
    curl \
    htop \
    mc \
    make \
    build-essential \
    socat \
    wget \
    vim  && \
    rm -rf /var/lib/apt/lists/*

#
# UTF-8 by default
#
RUN apt-get -qq update && \
    apt-get -qqy install gnupg2 locales && \
    locale-gen en_US.UTF-8 && \
    rm -rf /var/lib/apt/lists/*

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

#
# Pull Zulu OpenJDK binaries from official repository:
# Version: 8-latest
#
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 0xB1998361219BD9C9 && \
    echo "deb http://repos.azulsystems.com/ubuntu stable main" >> /etc/apt/sources.list.d/zulu.list && \
    apt-get -qq update && \
    apt-get -qqy install zulu-8 && \
    rm -rf /var/lib/apt/lists/*

#
# Install nginx
#
RUN apt-get update -y && \
    apt-get install -y nginx && \
    rm -rf /var/lib/apt/lists/*

#
# Install Image scripts
#
COPY assets/services /etc/service
COPY assets/entrypoint.sh /opt/scripts/entrypoint.sh
RUN chmod +x /opt/scripts/entrypoint.sh

#
# Configure nginx
#
COPY assets/configs /opt/configs
RUN rm /etc/nginx/sites-enabled/default && \
    ln -s /opt/configs/nginx/scipio.conf /etc/nginx/sites-enabled/default

WORKDIR /data

EXPOSE 8080 8443 8983 80

ENTRYPOINT /opt/scripts/entrypoint.sh

CMD tail -f /dev/null
