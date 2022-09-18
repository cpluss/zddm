-module(zddm_hashers).

-export([sha_hash_rate/1]).

-include_lib("eunit/include/eunit.hrl").

%% @doc sha_hash_rate uses crypto:hash method (with sha1)
%% to retrieve an 8-byte hash from a Key. This hash is divided
%% by the maximum 8-byte value to determine the rate of the key
%% itself (between 0 -> 1.0).
sha_hash_rate(Key) when is_integer(Key) ->
    ?FUNCTION_NAME(integer_to_binary(Key));
sha_hash_rate(Key) when is_atom(Key) ->
    ?FUNCTION_NAME(atom_to_binary(Key));
sha_hash_rate(Key) when is_list(Key) ->
    ?FUNCTION_NAME(list_to_binary(Key));
sha_hash_rate(Key) when is_binary(Key) ->
    ByteSize = precision_size_bytes(),
    RemainingBytes = 20 - ByteSize,
    MaxSize = max_size_from_bytes(ByteSize),
    <<
        _:RemainingBytes/binary,
        HashSeed:ByteSize/unsigned-integer-unit:8
    >> = crypto:hash(
        sha,
        <<Key/binary>>
    ),
    HashSeed / MaxSize.

% Number of bytes we should target for precision in order
% to get a good distribution within any space.
precision_size_bytes() -> 4.
% Get the maximum integer value for a specific value of
% bytes.
max_size_from_bytes(NumBytes) ->
    <<MaxSize:NumBytes/unsigned-integer-unit:8>> =
        list_to_binary([255 || _ <- lists:seq(1, NumBytes)]),
    MaxSize.
