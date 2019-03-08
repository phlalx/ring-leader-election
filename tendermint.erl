-module(tendermint).
-export([proc/1]).

-include_lib("stdlib/include/assert.hrl").

-define(TIMEOUT_PROPOSE, 1000).
-define(TIMEOUT_PROPOSE_RAND, 3000).

-record(state,
    { h = 0,
      round = 0,
      step = nil,
      decision = nil,
      locked_value = nil,
      locked_round = -1,
      valid_value = nil,
      valid_round = -1,
      p = 0,
      neighbors = nil
    }).

proposer(_H, _ROUND) -> 0.

getValue() -> 42.

broadcast(U, Neighbors) ->
    io:format("~w broadcasts value ~w to neighbors~w~n", [self(), U, Neighbors]),
    F = fun(X) -> X ! {msg, U} end,
    lists : map(F , Neighbors).

start_round(State, Round) ->
    io:format("~w starts round ~w~n", [self(), Round]),
    S = State#state.p,
    case proposer(State#state.h, State#state.round) of
        S ->
             Proposal = case valid_value of
                          nil -> getValue();
                          V -> V
                        end,
             H = State#state.h,
             Round = State#state.round,
             ValidRound = State#state.valid_round,
             P = State#state.p,
             broadcast({ proposal, H, Round, Proposal, ValidRound, P }, State#state.neighbors);
        _ ->
            R = rand : uniform (?TIMEOUT_PROPOSE_RAND),
            timer:send_after(?TIMEOUT_PROPOSE + R, timeout_propose)
    end,
    State#state{round = Round, step = propose}.

manage_proposal(State, { H, Round, V, Vr, P}) ->
    io:format("~w received proposal message h = ~w, round = ~w, v = ~w, vr = ~w ~n",
        [self(), H, Round, V, Vr]),
    ?assert((P == proposer(State#state.h, State#state.round)) and (H == State#state.h)),
    Id = if
        false -> nil;
        true -> nil
    end,
    broadcast({ prevote, H, Round, Id }, State#state.neighbors),
    State.

proc(State) ->
    receive
        init_state -> proc(#state{});
        { neighbors, N, Sender } ->
            io:format("~w is initialized with neighbors ~w~n", [self(), N]),
            Sender ! ok,
            proc(State#state{neighbors = N});
        { init, U, Sender } ->
            io:format("~w is initialized with id ~w~n", [self(), U]),
            Sender ! ok,
            proc(State#state{p = U});
        start ->
            io:format("~w is starting tendermint consensus algorithm~n", [self()]),
            State2 = start_round(State, 0),
            proc(State2);
        { msg, { proposal, H, Round, V, Vr, P } } ->
            State2 = manage_proposal(State, { H, Round, V, Vr, P }),
            proc(State2);
        { msg, U } ->
            io:format("~w received message ~w~n", [self(), U]),
            proc(State);
        timeout_propose ->
            io:format("~w received timeout_propose~n", [self()]),
            proc(State);
        quit ->
            io:format("~w says bye bye!~n", [self()])
    end,
    ok.
