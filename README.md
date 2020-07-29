rebar3_docs
=====

Generates nice looking documentation

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_docs, {git, "https://github.com/jfacorro/rebar3_docs.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:

    $ rebar3 docs
    ===> Fetching rebar3_docs
    ===> Compiling rebar3_docs
    <Plugin Output>

Useful links:

- https://github.com/census-instrumentation/opencensus-erlang/tree/master/doc
- https://github.com/uwiger/edown
- https://github.com/erszcz/docsh
