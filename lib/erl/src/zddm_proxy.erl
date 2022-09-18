%%%-----------------------------------
%%% @doc Proxy provides the main interface for ZDDM. It acts
%%% as a wrapper to route traffic between two storage adapters, based
%%% on a traffic-gate to manage how much of traffic will be diverted
%%% to the new storage adapter from the old.
%%%
%%% In order to use it you need to define a module which is responsible
%%% for creating the traffic gate, and the storage adapters. The module
%%% then acts as a routing point for a gen_server which is spun up
%%% to route all the traffic locally.
%%%
%%% In case you need to scale the local gen_server instance you can do
%%% so by using something like poolboy. Right now the functionality
%%% does not exist built-into the proxy itself, unfortunately.
%%%
%%% NOTE: This is only thread-safe if the underlying
%%% storage adapters are thread-safe.
%%% @end
%%%-----------------------------------
-module(zddm_proxy).

-behaviour(gen_server).

-export([
    % actual API to interact
    read/2,
    write/3,
    enable/1,
    disable/1,

    % gen_server exports
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    terminate/2,
    code_change/3
]).

%% @doc create_gate creates the traffic gate for this proxy.
-callback create_gate() -> zddm_traffic_gate:traffic_gate(term()).

%% @doc create_adapters creates both the new and the old adapter
%% for this proxy.
-callback create_adapters() ->
    #{
        old_adapter => zddm_storage_adapter:storage_adapter(term(), term()),
        new_adapter => zddm_storage_adapter:storage_adapter(term(), term())
    }.

% --
% public API

%% @doc read the key from the specified Module proxy.
read(Module, Key) ->
    gen_server:call(Module, {?FUNCTION_NAME, Key}).

%% @doc write the key and data from the specified Module proxy.
write(Module, Key, Value) ->
    gen_server:call(Module, {?FUNCTION_NAME, Key, Value}).

%% @doc enable the Module proxy.
enable(Module) ->
    gen_server:call(Module, ?FUNCTION_NAME).

%% @doc disable the Module proxy.
disable(Module) ->
    gen_server:call(Module, ?FUNCTION_NAME).

% --
% state management & configuration of the proxy itself

% Our state would is the proxy configuration itself
-record(proxy, {
    gate :: zddm_traffic_gate:traffic_gate(term()),
    old_adapter :: zddm_storage_adapter:storage_adapter(term(), term()),
    new_adapter :: zddm_storage_adapter:storage_adapter(term(), term()),

    enabled :: boolean(),
    module :: module()
}).

-spec create_proxy(module(), enabled | disabled) -> #proxy{}.
create_proxy(Module, EnabledEnum) ->
    Enabled =
        case EnabledEnum of
            enabled -> true;
            _ -> false
        end,
    #{old_adapter := OldAdapter, new_adapter := NewAdapter} = Module:create_adapters(),
    #proxy{
        gate = Module:create_gate(),
        old_adapter = OldAdapter,
        new_adapter = NewAdapter,
        enabled = Enabled,
        module = Module
    }.

% --
% gen_server callbacks
start_link([Module]) ->
    gen_server:start_link({local, Module}, ?MODULE, [Module], []).

init([Module]) ->
    {ok, create_proxy(Module, enabled)}.

handle_call(disable, _From, Proxy) ->
    {reply, ok, Proxy#proxy{enabled = false}};
handle_call(enable, _From, Proxy) ->
    {reply, ok, Proxy#proxy{enabled = true}};
handle_call({read, Key}, _From, Proxy) ->
    ShouldPass =
        Proxy#proxy.enabled and
            zddm_traffic_gate:should_pass(Proxy#proxy.gate, Key),
    case ShouldPass of
        false ->
            % Default to always read from the old gate
            Result = zddm_storage_adapter:read(Proxy#proxy.old_adapter, Key),
            {reply, Result, Proxy};
        true ->
            case zddm_storage_adapter:read(Proxy#proxy.new_adapter, Key) of
                {ok, Data} ->
                    {reply, {ok, Data}, Proxy};
                % Default to read old storage on every other result
                _ ->
                    case zddm_storage_adapter:read(Proxy#proxy.old_adapter, Key) of
                        % Propagate errors upwards from the old storage
                        {error, Error} ->
                            {reply, {error, Error}, Proxy};
                        {ok, Data} ->
                            % Write data to new storage so it'll produce
                            % a hit in the future, and return.
                            zddm_storage_adapter:write(Proxy#proxy.new_adapter, Key, Data),
                            {reply, Data, Proxy}
                    end
            end
    end;
handle_call({write, Key, Data}, _From, Proxy) ->
    case Proxy#proxy.enabled of
        false ->
            ok;
        true ->
            % Write to old first, return error if we get one otherwise proceed with
            % writing to the new storage as the first one was a success.
            case zddm_storage_adapter:write(Proxy#proxy.old_adapter, Key, Data) of
                ok -> zddm_storage_adapter:write(Proxy#proxy.new_adapter, Key, Data);
                Error -> Error
            end
    end.

handle_cast(_Msg, Proxy) ->
    {noreply, Proxy}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, Proxy, _Extra) ->
    % Recreate the proxy in order to use the latest version
    case Proxy#proxy.enabled of
        true -> {ok, create_proxy(Proxy#proxy.module, enabled)};
        false -> {ok, create_proxy(Proxy#proxy.module, disabled)}
    end.
