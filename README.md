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

Options
--------

This plugin provides some configuration options by adding a `{docs,
Options}` entry to the project's `rebar.config`.

Available options:

- `categories`: list of category name with the modules that belong to
  them. This information is used to create sections in the navigation
  menu. When a module is not included in any category it is added at
  the beginning of the modules list in the navigation menu.

  ```
  { categories
  , [ {"Name1", [module1, module2]}
    , {"Name2", [module3]}
    ]
  }
  ```

--------

Useful links
----------------

- [docsh - Documentationi in the Erlang shell](https://github.com/erszcz/docsh)
- [HTML+JS Sidenav How To](https://www.w3schools.com/howto/howto_js_sidenav.asp)
- [Erlang Ecosystem Foundation - Documentation Working Group](https://github.com/erlef/documentation-wg/issues/5)
- [edown - edoc to markdown](https://github.com/uwiger/edown)
