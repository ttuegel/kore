FROM alpine:edge
RUN apk update
RUN apk add build-base curl ncurses git openjdk8 maven z3
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN ln -s /lib64 /lib