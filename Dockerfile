FROM alpine:edge
RUN apk update
RUN apk add curl openjdk8 maven z3
RUN curl -sSL https://get.haskellstack.org/ | sh