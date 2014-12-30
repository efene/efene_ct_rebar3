efene_ct_rebar3
=====

efene rebar3 common test plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { efene_ct_rebar3, ".*", {git, "git@host:user/efene_ct_rebar3.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 efene_ct_rebar3
    ===> Fetching efene_ct_rebar3
    ===> Compiling efene_ct_rebar3
    <Plugin Output>
