# IBO
Private Repository for Intelligent Business Object Research

## rebar
rebar is an Erlang build tool and needs to be installed locally for development.

To compile the application use:
```shell
project-root> rebar compile
```

## Mnesia
To create an inital Mnesia database at the project root:
```shell
project-root> mnesia:create_schema([node()]).
ok
project-root> init:stop().
ok
```