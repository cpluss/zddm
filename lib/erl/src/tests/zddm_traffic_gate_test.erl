-module(zddm_traffic_gate_test).

-include_lib("eunit/include/eunit.hrl").

should_pass_always_open_test_() ->
    ?_assertEqual(
        true,
        zddm_traffic_gate:should_pass(#{rate => 1.0}, not_set)
    ).

should_pass_always_close_test_() ->
    ?_assertEqual(
        false,
        zddm_traffic_gate:should_pass(#{rate => 0.0}, not_set)
    ).

update_rate_test_() ->
    ?_assertEqual(
        #{rate => 0.9},
        zddm_traffic_gate:update_rate(#{rate => 0.0}, 0.9)
    ).

pass_rate_test_() ->
    % Do 100k samples
    NumSamples = 100000,
    % 50%
    Rate = 0.5,
    Gate = zddm_traffic_gate:create_gate(
        Rate,
        fun zddm_hashers:sha_hash_rate/1
    ),

    % Evaluate 100k times
    Results = [
        zddm_traffic_gate:should_pass(Gate, N)
     || N <- lists:seq(0, NumSamples)
    ],
    NumPasses = length([R || R <- Results, R =:= true]),
    NumBlocks = length([R || R <- Results, R =/= true]),
    ?debugFmt("NumPasses = ~p, NumBlocks = ~p", [NumPasses, NumBlocks]),

    % We should be within 0.1% precision
    ErrorMargin = 0.001,

    % Compute how far off we are
    PassTarget = Rate * NumSamples,
    PassDiff = abs(PassTarget - NumPasses),
    BlockTarget = (1.0 - Rate) * NumSamples,
    BlockDiff = abs(BlockTarget - NumBlocks),
    ?debugFmt("PassDiff = ~p, BlockDiff = ~p", [PassDiff, BlockDiff]),
    ?debugFmt("Error Margin = ~p", [(ErrorMargin * NumSamples)]),
    [
        ?_assert(PassDiff < (ErrorMargin * NumSamples)),
        ?_assert(BlockDiff < (ErrorMargin * NumSamples))
    ].
