-module(sdk_checking).

-export([main/1]).

main(_State) ->
    case os:cmd("which fdbcli") of
        "" ->
            help(),
            {error, not_found_foundation_client};
        _ ->
            ok
    end.

help() ->
    io:format(">>> Checking SDK dependency failed <<<\n"),
    io:format("You should install the FoundationDB client first\n"),
    io:format("You can use the 'scripts/fetch_fdb_cli.sh' to download the install packeage\n").
