-module(jobmachine_engine).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("jobmachine.hrl").

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
%% Jobmachine Function Exports
%% ------------------------------------------------------------------

-export([list_to_list_of_tuples/2]).
%%
%% ------------------------------------------------------------------
%% Jobmachine Engine State 
%% ------------------------------------------------------------------

-record(jm_engine_state, {
    redis_client,
    jobs
  }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, jobmachine}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_State) ->
  {ok, C} = eredis:start_link(),
  Jobspecs = load_jobspecs(C),
  Jobs2 = start_jobs(Jobspecs),
  NewState = #jm_engine_state{jobs = Jobs2, redis_client = C},
  {ok, NewState}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec(load_jobspecs(_RedisClient) -> J::list()).
load_jobspecs(C) ->
  {ok, Jobspecs} = eredis:q(C, ["hgetall", "jobspecs"]),
  Jobtuples = list_to_list_of_tuples(Jobspecs),
  [parse_job(J) || J <- Jobtuples].
  
-spec(parse_job({JobName :: binary(), JobJson :: binary()}) -> #jm_job{}).
parse_job({_Jobname, JobJson}) ->
  io:format("json: ~p~n",[JobJson]),
  JobProplist = jsx:decode(JobJson),
  io:format("~p~n",[JobProplist]), 
  Job = #jm_job{name    = proplists:get_value(<<"name">>, JobProplist, <<"default">>),
                submitter = proplists:get_value(<<"submitter">>, JobProplist, <<"anonymous">>),
                submit_datetime = iso8601:format(calendar:universal_time()),
                execute_datetime = proplists:get_value(<<"execute_datetime">>, JobProplist, iso8601:format(calendar:universal_time())),
                state = waiting,
                kind = immediate,
                command = proplists:get_value(<<"command">>, JobProplist, <<"/bin/true">>)
                },
  Job.

-spec(start_jobs(J::list(#jm_job{})) -> R::list()).
start_jobs(Joblist) ->
  [start_job(J) || J <- Joblist].

-spec(start_job(J :: #jm_job{}) -> #jm_job{}).
start_job(#jm_job{name = Name, command = Command} = Job) ->
  io:format("starting ~p ~p ~n", [Name, Command]),
  Job2 = Job#jm_job{ state = started },
  Job2.

list_to_list_of_tuples(L) ->
  list_to_list_of_tuples(L,[]).

-spec(list_to_list_of_tuples(L::list(),A::list()) -> B::list()).
list_to_list_of_tuples([], Acc) -> Acc;
list_to_list_of_tuples([H1,H2|T], Acc) ->
  list_to_list_of_tuples(T, [{H1, H2} | Acc]).
