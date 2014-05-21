{application,jobmachine,
             [{description,"a dependency scheduler"},
              {vsn,"0.0.1"},
              {registered,[]},
              {applications,[kernel,stdlib,folsom,eredis]},
              {mod,{jobmachine_app,[]}},
              {env,[]},
              {modules,[jobmachine_app,jobmachine_engine,jobmachine_sup]}]}.
