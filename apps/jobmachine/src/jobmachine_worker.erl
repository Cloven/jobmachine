-module(jobmachine_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Jobmachine State
%% ------------------------------------------------------------------

-record(jm_worker_state,
  {
    command,
    port,
    timeout,
    output
  }).

%% ------------------------------------------------------------------
%% Jobmachine Function Exports
%% ------------------------------------------------------------------

-export([watchdog/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Command]) ->
  init([Command,5000]);
init([Command,Timeout]) ->
  %%PrivDir = code:priv_dir(jobmachine),
  WorkerState = #jm_worker_state{
    command = Command,
    port = open_port({spawn_executable, Command}, [stream, {line, 4096}, binary, exit_status, hide]),
    timeout = Timeout,
    output = []
  },
  spawn_link(jobmachine_worker, watchdog, [self(), Timeout]),
  {ok, WorkerState}.

handle_call(_, _From, State) ->
  {reply, ok, State}.

handle_info({_Port, {data, {_EolIndicator, LineData}}}, State=#jm_worker_state{output = Output} ) ->
  io:format("info: ~p~n",[LineData]),
  NewOutput = [LineData | Output],
  NewState = State#jm_worker_state{ output = NewOutput },
  {noreply, NewState};
handle_info({_Port, {exit_status, ExitStatus}}, State) ->
  io:format("port exited: ~p~n",[ExitStatus]),
  io:format("end state: ~p~n",[State]),
  {noreply, State}.

handle_cast({watchdog_timeout, Timeout}, State) ->
  io:format("timeout reached: ~p~n",[Timeout]),
  {noreply, State};
handle_cast(Msg, State) ->
  io:format("cast: ~p~n",[Msg]),
  {noreply, State}.

terminate(_Reason, _State) ->
  %% TODO: kill the job if it wants to be killed
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec watchdog(Pid :: pid, TimeoutMilliseconds :: non_neg_integer()) -> ok.
watchdog(Pid, TimeoutMilliseconds) ->
  receive
  after TimeoutMilliseconds ->
      gen_server:cast(Pid, {watchdog_timeout, TimeoutMilliseconds})
  end,
  ok.
