%%%-----------------------------------
%%% @doc Traffic gates are configured to let specific keys
%%% through [deterministically] over time. This allows us to
%%% control how much of the read and write traffic we're introducing
%%% the double write overhead into.
%%%
%%% This allows us to start initially small, negating most of the
%%% performance impact a large amount of reads and writes would cause,
%%% and slowly ramp up over time in order to capture the full extent
%%% of the data.
%%%
%%% The better hit rate we have when reading and writing from the new
%%% storage backend, the better.
%%% @end
%%%-----------------------------------
-module(zddm_traffic_gate).

%% -type deterministic_hash_rate_fun :: fun(). is a function which takes
%% a generic key `K` and returns a value between 0 -> 1.0, which
%% in turn will be used to gate traffic.
%%
%% Note that this should have a good statistical distribution within
%% its range (0 - 1.0) for any given key, and be deterministic
%% over time given the same input.
-type deterministic_hash_rate_fun(K) :: fun((K) -> float()).

%% -type traffic_gate(K) :: map(). is the config required in order to leverage
%% the traffic-gate module. It contains the configuration for what
%% hash-rate-function to use, as well as the pass-through rate.
-type traffic_gate(K) ::
    #{
        rate => float(),
        hash_rate_fun => deterministic_hash_rate_fun(K)
    }.

-export_type([deterministic_hash_rate_fun/1, traffic_gate/1]).

-export([
    create/2,
    update_rate/2,
    should_pass/2
]).

%% @doc create sets up a new gate configuration with the supplied
%% Rate and HashRate Function.
-spec create(float(), deterministic_hash_rate_fun(K)) -> traffic_gate(K).
create(Rate, HashRateFun) ->
    #{rate => Rate, hash_rate_fun => HashRateFun}.

%% @doc update_rate sets the rate of a gate.
-spec update_rate(traffic_gate(K), float()) -> traffic_gate(K).
update_rate(Gate, NewRate) ->
    maps:put(rate, NewRate, Gate).

%% @doc should_pass returns true or false depending on if
%% a specific key would be let-through the traffic-gate
%% as configured.
-spec should_pass(traffic_gate(K), K) -> boolean().
% It does not matter what the key is if the rate is either
% 0.0 (disabled) or 1.0 (enabled).
should_pass(#{rate := 0.0}, _) ->
    false;
should_pass(#{rate := 1.0}, _) ->
    true;
should_pass(
    #{
        rate := Rate,
        hash_rate_fun := HashRateFun
    },
    Key
) ->
    HashRateFun(Key) < Rate.
