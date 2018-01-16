pring
===========

A partisan application

On all nodes::

    pring:print_ring().
    pring:all_members().
    pring:broadcast_ring().

On Node 1::

    {ok, Idxs} = pring_broadcast_handler:ring_apply(fun (Ring) ->
        Idxs = riak_core_ring:my_indices(Ring),
        {ok, Ring, Idxs}
    end).

    [Idx1, Idx2 | _] = Idxs.

    pring:transfer_node(Idx1, 'pring2@127.0.0.1').

    pring:broadcast_ring().

    % on all nodes
    pring:print_ring().

    pring:transfer_node(Idx2, 'pring3@127.0.0.1').

    pring:broadcast_ring().

    % on all nodes
    pring:print_ring().

demo screencast: https://asciinema.org/a/aYwww1dlkI9vxjMIykjG9uuuI

Build
-----

::

    rebar3 release

Test
----

::

    rebar3 ct

Run
---

::

    rebar3 run

Clustering
----------

::

    make devrel

    # on 3 different shells
    make dev1-console
    make dev2-console
    make dev3-console

    # join all nodes:
    make devrel-join

    # check node members
    make devrel-status

    # join node1 to node2 manually:
    ./_build/dev1/rel/pring/bin/pring-admin cluster join pring2@127.0.0.1

    # check node1 members
    ./_build/dev1/rel/pring/bin/pring-admin cluster members

    # check node1 connections
    ./_build/dev1/rel/pring/bin/pring-admin cluster connections

Ping node2 from node1 using partisan::

    1> pring:ping('pring2@127.0.0.1').
    ok

    % check logs/console on node2, you should see:
    got msg ping

Quit
----

::

    1> q().

TODO
----

* define license and create LICENSE file

License
-------

TODO
