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
    timeout
  }).

%% ------------------------------------------------------------------
%% Jobmachine Function Exports
%% ------------------------------------------------------------------

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
    timeout = Timeout
  },
  {ok, WorkerState}.

handle_call(_, _From, State) ->
  {reply, ok, State}.

handle_cast(Msg, State) ->
  io:format("msg: ~p~n",[Msg]),
  {noreply, State}.

handle_info(Info, State) ->
  io:format("info: ~p~n",[Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  %% TODO: kill the job if it wants to be killed
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
