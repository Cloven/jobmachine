-type iso8601_datetime() :: binary().

-type job_kind() :: immediate | scheduled.
-type job_state() :: waiting | paused | started | failed | completed.
-type job_do_logging() :: boolean().

-record(jm_job,
  {
    name :: binary(),
    submitter :: binary(),
    submit_datetime :: iso8601_datetime(),
    execute_datetime :: iso8601_datetime(),
    state :: job_state(),
    kind :: job_kind(),
    command :: binary()
  }
).
    
