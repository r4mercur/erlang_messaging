FROM erlang:26-alpine as builder

WORKDIR /app

RUN apk add --no-cache git bash

COPY rebar.config rebar.lock ./

RUN rebar3 deps
COPY src/ ./src/

RUN rebar3 compile
RUN rebar3 release

COPY --from=builder /app/_build/default/rel/erlang_messaging ./

EXPOSE 4040

HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD nc -z localhost 4040 || exit 1

CMD ["./bin/erlang_messaging", "foreground"]