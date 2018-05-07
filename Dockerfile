FROM debian:9.4
MAINTAINER Habib Alamin

RUN useradd -s /bin/bash -m arksink

ADD . /var/www/arksink
WORKDIR /var/www/arksink

ENV ARKSINK_DB_HOST db
ENV ARKSINK_DB_USERNAME postgres
ENV ARKSINK_REDIS_HOST cache

RUN docker/setup-dependency-management-requirements.sh

RUN docker/install-nodejs.sh

RUN apt-get install -y sudo libpq-dev postgresql-9.6 coffeescript \
 && apt-get install -y --no-install-recommends yarn \
 && yarn global add elm

RUN docker/install-redis.sh
RUN docker/install-sassc.sh
RUN docker/install-stack.sh

USER arksink
RUN mkdir -p ~/.stack
USER root

EXPOSE 3000

ENTRYPOINT ["docker/entrypoint.sh"]
CMD ["make", "run-server"]
