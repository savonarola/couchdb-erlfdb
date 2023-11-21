all: check

build:
	@rebar3 compile

check: build
	@rebar3 eunit

clean:
	@rebar3 clean
	@rm -rf _build

fmt:
	@erlfmt -w '{src,include,priv,test}/**/*.{erl,hrl,app.src,es}'
