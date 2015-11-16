# IBO
Private Repository for Intelligent Business Object Research

## rebar
rebar is an Erlang build tool and needs to be installed locally for development.

To compile the application use:
```shell
project-root> rebar compile
```

## Application
Not yet working, because supervisors need to be configured:
```shell
ebin> application:loaded_applications().    # lists all loaded applications
ebin> application:load(ibo).                # loads application ibo (needs ibo.app-file)
ebin> application:start(ibo).               # starts the application
ebin> application:stop(ibo).                # stops the application
```

Use the following commands in the meantime (also see test/console.txt):
```shell
ebin> ibo_app:install([node()]).
ebin> mnesia:start().
ebin> directory_server:start_link().
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