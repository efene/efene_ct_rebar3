efene rebar3 Common Test Plugin
===============================

efene rebar3 plugin to run Common Test

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { efene_ct_rebar3, ".*", {git, "git@github.com:efene/efene_ct_rebar3.git", {branch, "master"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 efene ct
    ===> Fetching efene_ct_rebar3
    ===> Compiling efene_ct_rebar3
    <Plugin Output>
