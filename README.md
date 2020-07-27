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
        {rebar3_docs, {git, "https://host/user/rebar3_docs.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_docs
    ===> Fetching rebar3_docs
    ===> Compiling rebar3_docs
    <Plugin Output>
