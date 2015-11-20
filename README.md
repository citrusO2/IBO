# IBO
Private Repository for Intelligent Business Object Research

## rebar
rebar is an Erlang build tool and needs to be installed locally for development.

To compile the application use:
```shell
project-root> rebar compile
```

## Application
How to start the application (use erl_startscript to get dependencies and codepaths):
```shell
ebin> ibo_app:install([node()]).            # to execute the installation of the database, must be done first once!
ebin> application:loaded_applications().    # lists all loaded applications (dependencies must be started)
ebin> application:load(ibo).                # loads application ibo (needs ibo.app-file)
ebin> application:start(ibo).               # starts the application
ebin> application:stop(ibo).                # stops the application
```

## Mnesia
To create an inital Mnesia database at the project root:
```shell
project-root> mnesia:create_schema([node()]).
ok
project-root> init:stop().
ok
```
Mnesia queries (unless using the dirty function) need to be wrapped in a transaction