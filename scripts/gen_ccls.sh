#!/bin/bash

template=$(cat <<EOF
clang
%c -std=c11
-I./include/
-I/usr/include/foundationdb/
-I$(erl -eval 'io:format("~s", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])])' -s init stop -noshell)
EOF
)
echo "$template"
