% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(erlfdb_tenant).

-export([create_tenant/2, open_tenant/2, delete_tenant/2]).

-define(IS_DB, {erlfdb_database, _}).
-define(TENANT_MAP_PREFIX, <<16#FF, 16#FF, "/management/tenant/map/">>).

-import(erlfdb, [get/2, clear/2, set/3, wait/1, transactional/2, set_option/2]).

create_tenant(?IS_DB = Db, Tenant) ->
    transactional(Db, fun(Tx) ->
        create_tenant(Tx, Tenant)
    end);
create_tenant(Tx, Tenant) ->
    set_option(Tx, special_key_space_enable_writes),
    Key = tenant_key(Tenant),
    case check_tenant_existence(Tx, Key) of
        not_found ->
            set(Tx, Key, <<>>);
        _ ->
            {error, tenant_already_exists}
    end.

open_tenant(?IS_DB = Db, Name) ->
    erlfdb_nif:database_open_tenant(Db, Name).

delete_tenant(?IS_DB = Db, Tenant) ->
    transactional(Db, fun(Tx) ->
        delete_tenant(Tx, Tenant)
    end);
delete_tenant(Tx, Tenant) ->
    set_option(Tx, special_key_space_enable_writes),
    Key = tenant_key(Tenant),
    case check_tenant_existence(Tx, Key) of
        not_found ->
            {error, tenant_not_found};
        _ ->
            clear(Tx, Key)
    end.

check_tenant_existence(Tx, Key) ->
    wait(get(Tx, Key)).

tenant_key(Tenant) ->
    <<?TENANT_MAP_PREFIX/binary, Tenant/binary>>.
