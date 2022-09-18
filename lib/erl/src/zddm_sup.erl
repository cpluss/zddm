-module(zddm_sup).

-export([proxy/1, proxy/2]).

%% @doc proxy sets-up a supervisor child towards the supplied
%% module which will act as a data proxy.
proxy(Module) ->
    ?FUNCTION_NAME(Module, #{restart => permanent}).

%% @doc proxy sets-up a supervisor child towards the supplied
%% module which will act as a data proxy. Supply your own options
%% to override the child-spec.
proxy(Module, Options) ->
    Spec = #{
        id => Module,
        start => {zddm_proxy, start_link, [Module]}
    },
    maps:merge(Spec, Options).
