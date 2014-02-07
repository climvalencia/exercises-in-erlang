-module(guess_the_number).

-behaviour(gen_fsm).

% gen_fsm
-export([
    game_stopped/3,
    game_started/3
]).

-export([
    start_link/0,
    init/1,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4,
    stop/0
]).

% API
-export([
    new_game/0,
    new_game/1,
    new_game/2,
    guess/1,
    auto_guess/2
]).

-record(state, {
    number,
    min,
    max,
    guesses_left
}).

-define(DEFAULT_MIN, 1).
-define(DEFAULT_MAX, 101).
-define(INFO(Format, Vars), io:format(Format ++ "~n", Vars)).

-type min() :: integer().
-type max() :: integer().

%%%=============================================================================
%%% API
%%%=============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(term()) -> {ok, term()}.
init([]) ->
    ?INFO("Server started", []),
    {ok, game_stopped, #state{}}.

-spec stop() -> ok.
stop() ->
    gen_fsm:send_all_state_event(?MODULE, stop).

-spec new_game() -> {min(), max()}.
new_game() ->
    new_game(?DEFAULT_MIN, ?DEFAULT_MAX).

-spec new_game(max()) -> {min(), max()}.
new_game(Max) when is_integer(Max) ->
    new_game(?DEFAULT_MIN, Max).

-spec new_game(min(), max()) -> {min(), max()}.
new_game(Min, Max) when is_integer(Min), is_integer(Max) ->
    gen_fsm:sync_send_all_state_event(?MODULE, {new_game, {Min, Max}}).

-spec guess(integer()) -> lower | higher | equal.
guess(N) when is_integer(N) ->
    gen_fsm:sync_send_event(?MODULE, {guess, N}).

-spec auto_guess(min(), max()) -> ok.
auto_guess(Min, Max) when is_integer(Min), is_integer(Max) ->
    Guess = int_ceil((Max + Min) / 2),
    ?INFO("Guess: ~p", [Guess]),
    case (catch guess(Guess)) of
        lower -> auto_guess(Min, Guess - 1);
        higher -> auto_guess(Guess + 1, Max);
        X -> X
    end.

%%%=============================================================================
%%% gen_fsm callbacks
%%%=============================================================================
game_stopped({guess, _}, From, St) ->
    Reply = {error, stopped},
    gen_fsm:reply(From, Reply),
    {next_state, game_stopped, St}.

game_started({guess, Guess}, From, St) ->
    Number = St#state.number,
    Reply = case Number - Guess of
        0 -> equal;
        N when N < 0 -> lower;
        N when N > 0 -> higher
    end,
    gen_fsm:reply(From, Reply),
    case Reply of
        equal ->
            {next_state, game_stopped, St};
        _ ->
            GuessesLeft = St#state.guesses_left - 1,
            case GuessesLeft of
                G when G > 0 ->
                    ?INFO("Guesses left: ~p", [GuessesLeft]),
                    {next_state, game_started, St#state{guesses_left = GuessesLeft}};
                _ ->
                    ?INFO("Game over! Number was ~p", [St#state.number]),
                    {next_state, game_stopped, St}
            end
    end.

handle_event(stop, _, St) ->
    {stop, normal, St}.

handle_sync_event({new_game, {Min, Max}}, From, _, St) ->
    GuessesLeft = int_ceil(log2(Max)),
    Number = crypto:rand_uniform(Min, Max),
    NewSt = St#state{number = Number,
                  min = Min,
                  max = Max,
                  guesses_left = GuessesLeft},
    gen_fsm:reply(From, {Min, Max}),
    ?INFO("Guess the number in range [~p,~p). Guesses left: ~p",
        [Min, Max, GuessesLeft]),
    {next_state, game_started, NewSt}.

handle_info(_Info, StateName, St) -> {next_state, StateName, St}.
code_change(_OldVsn, StateName, St, _Extra) -> {ok, StateName, St}.
terminate(_Reason, _StateName, _St) -> ok.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================
-spec log2(number()) -> float().
log2(N) ->
    math:log10(N) / math:log10(2).

%% credit to mochinum.erl for int_ceil/1
-spec int_ceil(number()) -> integer().
int_ceil(N) ->
    T = trunc(N),
    case (N - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.