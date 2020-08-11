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

- [docsh - Documentationi in the Erlang shell](https://github.com/erszcz/docsh)
- [HTML+JS Sidenav How To](https://www.w3schools.com/howto/howto_js_sidenav.asp)
- [Erlang Ecosystem Foundation - Documentation Working Group](https://github.com/erlef/documentation-wg/issues/5)
- [edown - edoc to markdown](https://github.com/uwiger/edown)
