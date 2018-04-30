FROM debian:9.4
MAINTAINER Habib Alamin

WORKDIR /var/www/arksink

ENV ARKSINK_DB_HOST db
ENV ARKSINK_DB_USER postgres
ENV ARKSINK_REDIS_HOST redis

RUN docker/setup-dependency-management-requirements.sh

RUN docker/install-nodejs.sh

RUN apt-get install -y libpq-dev postgresql-9.6 coffeescript \
 && apt-get install -y --no-install-recommends yarn \
 && yarn global add elm

RUN docker/install-redis.sh
RUN docker/install-sassc.sh
RUN docker/install-stack.sh

RUN stack install dotenv
RUN make build

EXPOSE 3000

CMD make run-server
