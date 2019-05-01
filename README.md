# Actor Based Business Process Execution via Intelligent Business Objects

Repository for Intelligent Business Object Research

Contains a working prototype of a business process execution engine, utilizing actors and Intelligent Business Objects (IBOs)
* Backend written in Erlang
* Frontend written in Javascript/AngularJS

## Using the prototype

Refer to page 59 of `IBO_master_thesis.pdf` on the prerequisites and how to start and use the prototype 

## Abstract

Business processes are a vital part of every company. But executing business processes with conventional business process engines is complex, and so people rely on services such as email, where processes naturally emerge from the users’ interactions.
Instead of relying on the typical centralised and monolithic architecture of business process engines, this thesis tries to show, that a decentralised approach utilising the Actor Model and Intelligent Business Objects (IBOs) is also feasible. Using the Actor Model, the process engine is distributed on several smaller independent actors, which enables a high flexibility. But instead of needing to store process configurations on actors to execute a process, the process configuration is stored in an Intelligent Business Object, which is passed around by the actors for each process instance. As each actor can execute its part of the process without any pre-configuration, a process can even be updated while it is already executing.

Starting with a basic architecture, the system of actors is gradually expanded, until business processes that allow human interactions can be executed. The architecture is then implemented using the programming language Erlang to create a working prototype. The approach is considered feasible when processes based on the Actor Model and IBOs can be set up, executed and interacted with using a graphical user interface.

As the prototype fulfilled the requirements as mentioned above, the approach is viable. Furthermore, the graphical representations of IBOs and the Business Process Model and Notation (BPMN) are both graph oriented and similar, which might allow conversions between the two.
How well this new approach fares against existing solutions has yet to be determined, as its current limited implementation does not permit a direct comparison yet. But this work could open up a new chapter in how business processes are executed.
