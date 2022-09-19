%%%-----------------------------------
%%% @doc Proxy provides the main interface for ZDDM. It acts
%%% as a wrapper to route traffic between two storage adapters, based
%%% on a traffic-gate to manage how much of traffic will be diverted
%%% to the new storage adapter from the old.
%%%
%%% NOTE: This is only thread-safe if the underlying
%%% storage adapters are thread-safe.
%%% @end
%%%-----------------------------------
-module(zddm_proxy).

-export([
    create/3,
    enable/1,
    disable/1,
    read/2,
    write/3
]).

-type proxy(K, V) :: #{
    gate => zddm_traffic_gate:traffic_gate(K),
    old_adapter => zddm_storage_adapter:storage_adapter(K, V),
    new_adapter => zddm_storage_adapter:storage_adapter(K, V),

    enabled => enabled | disabled
}.

%% @doc create a new proxy that can be passed around and
%% used to interact with the proxy API. It's enabled
%% by default.
-spec create(
    zddm_traffic_gate:traffic_gate(K),
    zddm_storage_adapter:storage_adapter(K, V),
    zddm_storage_adapter:storage_adapter(K, V)
) -> proxy(K, V).
create(Gate, OldAdapter, NewAdapter) ->
    #{
        gate => Gate,
        old_adapter => OldAdapter,
        new_adapter => NewAdapter,
        enabled => enabled
    }.

%% @doc enable the proxy functionality.
enable(Proxy) ->
    maps:put(enabled, enabled, Proxy).

%% @doc disable the proxy functionality.
disable(Proxy) ->
    maps:put(enabled, disabled, Proxy).

%% @doc performs a read towards the proxy, using the traffic
%% gate therein to determine whether to read from the new
%% storage or the old storage accordingly.
-spec read(proxy(K, V), K) -> {ok, V} | {error, term()}.
read(
    #{
        gate := Gate,
        old_adapter := OldAdapter,
        new_adapter := NewAdapter,

        % We match on enabled here to make sure we're good to go
        enabled := enabled
    },
    Key
) ->
    case zddm_traffic_gate:should_pass(Gate, Key) of
        false ->
            % Default to always read from the old gate
            zddm_storage_adapter:read(OldAdapter, Key);
        true ->
            case zddm_storage_adapter:read(NewAdapter, Key) of
                {ok, Data} ->
                    {ok, Data};
                % Default to read old storage on every other result
                _ ->
                    case zddm_storage_adapter:read(OldAdapter, Key) of
                        {ok, Data} ->
                            % Write data to new storage so it'll produce
                            % a hit in the future, and return.
                            zddm_storage_adapter:write(NewAdapter, Key, Data),
                            {ok, Data};
                        % Propagate errors upwards from the old storage
                        Error ->
                            Error
                    end
            end
    end;
% In case it's disabled we only need to read from the old
% adapter
read(#{old_adapter := OldAdapter, enabled := disabled}, Key) ->
    zddm_storage_adapter:read(OldAdapter, Key).

%% @doc performs a write towards the proxy, using the traffic
%% gate therein to determine whether to write to the new
%% storage and the old storage accordingly.
-spec write(proxy(K, V), K, V) -> ok | {error, term()}.
write(
    #{
        gate := Gate,
        old_adapter := OldAdapter,
        new_adapter := NewAdapter,

        % We match on enabled here to make sure we're good to go
        enabled := enabled
    },
    Key,
    Data
) ->
    case zddm_traffic_gate:should_pass(Gate, Key) of
        false ->
            % Default to only write to the old storage
            zddm_storage_adapter:write(OldAdapter, Key, Data);
        true ->
            % Write to old first, return error if we get one otherwise proceed with
            % writing to the new storage as the first one was a success.
            case zddm_storage_adapter:write(OldAdapter, Key, Data) of
                ok -> zddm_storage_adapter:write(NewAdapter, Key, Data);
                Error -> Error
            end
    end;
% In case it's disabled we only need to write to the old
% adapter
write(#{old_adapter := OldAdapter, enabled := disabled}, Key, Data) ->
    zddm_storage_adapter:write(OldAdapter, Key, Data).
