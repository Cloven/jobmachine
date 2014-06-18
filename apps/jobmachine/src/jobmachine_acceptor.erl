-module(jobmachine_acceptor).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("jobmachine.hrl").

-record(jm_worker_state, {
    redis_client,
    job_queue,
    worker_queue
  }).


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
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).  %% note: not registering names

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({RedisHost, RedisPort, JobQueue}) ->
  {ok, RedisClient} = eredis:start_link(RedisHost, RedisPort),
  WorkerQueue = <<"blurp">>,
  NewState = #jm_worker_state{ redis_client = RedisClient, job_queue = JobQueue, worker_queue = WorkerQueue },
  gen_server:cast(self(), startup),
  {ok, NewState}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(startup, State) ->
  get_job(State);
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

-spec(parse_job(JobJson :: binary()) -> #jm_job{}).
parse_job(JobJson) ->
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

get_job(#jm_worker_state{redis_client = RedisClient, job_queue = JobQueue, worker_queue = WorkerQueue} = State) ->
  {ok, Res} = eredis:q(RedisClient, ["BRPOPLPUSH", JobQueue, WorkerQueue, 0], infinity),
  io:format("~p~n",[Res]),
  get_job(State).
