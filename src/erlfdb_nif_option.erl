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

%% Semi-automatically generated

-module(erlfdb_nif_option).

-export([to_network_option/1]).

-include("fdb_options.hrl").

to_network_option(local_address) ->
    ?FDB_NET_OPTION_LOCAL_ADDRESS;
to_network_option(cluster_file) ->
    ?FDB_NET_OPTION_CLUSTER_FILE;
to_network_option(trace_enable) ->
    ?FDB_NET_OPTION_TRACE_ENABLE;
to_network_option(trace_roll_size) ->
    ?FDB_NET_OPTION_TRACE_ROLL_SIZE;
to_network_option(trace_max_logs_size) ->
    ?FDB_NET_OPTION_TRACE_MAX_LOGS_SIZE;
to_network_option(trace_log_group) ->
    ?FDB_NET_OPTION_TRACE_LOG_GROUP;
to_network_option(trace_format) ->
    ?FDB_NET_OPTION_TRACE_FORMAT;
to_network_option(trace_clock_source) ->
    ?FDB_NET_OPTION_TRACE_CLOCK_SOURCE;
to_network_option(trace_file_identifier) ->
    ?FDB_NET_OPTION_TRACE_FILE_IDENTIFIER;
to_network_option(trace_partial_file_suffix) ->
    ?FDB_NET_OPTION_TRACE_PARTIAL_FILE_SUFFIX;
to_network_option(knob) ->
    ?FDB_NET_OPTION_KNOB;
to_network_option(tls_plugin) ->
    ?FDB_NET_OPTION_TLS_PLUGIN;
to_network_option(tls_cert_bytes) ->
    ?FDB_NET_OPTION_TLS_CERT_BYTES;
to_network_option(tls_cert_path) ->
    ?FDB_NET_OPTION_TLS_CERT_PATH;
to_network_option(tls_key_bytes) ->
    ?FDB_NET_OPTION_TLS_KEY_BYTES;
to_network_option(tls_key_path) ->
    ?FDB_NET_OPTION_TLS_KEY_PATH;
to_network_option(tls_verify_peers) ->
    ?FDB_NET_OPTION_TLS_VERIFY_PEERS;
to_network_option(client_buggify_enable) ->
    ?FDB_NET_OPTION_CLIENT_BUGGIFY_ENABLE;
to_network_option(client_buggify_disable) ->
    ?FDB_NET_OPTION_CLIENT_BUGGIFY_DISABLE;
to_network_option(client_buggify_section_activated_probability) ->
    ?FDB_NET_OPTION_CLIENT_BUGGIFY_SECTION_ACTIVATED_PROBABILITY;
to_network_option(client_buggify_section_fired_probability) ->
    ?FDB_NET_OPTION_CLIENT_BUGGIFY_SECTION_FIRED_PROBABILITY;
to_network_option(tls_ca_bytes) ->
    ?FDB_NET_OPTION_TLS_CA_BYTES;
to_network_option(tls_ca_path) ->
    ?FDB_NET_OPTION_TLS_CA_PATH;
to_network_option(tls_password) ->
    ?FDB_NET_OPTION_TLS_PASSWORD;
to_network_option(disable_multi_version_client_api) ->
    ?FDB_NET_OPTION_DISABLE_MULTI_VERSION_CLIENT_API;
to_network_option(callbacks_on_external_threads) ->
    ?FDB_NET_OPTION_CALLBACKS_ON_EXTERNAL_THREADS;
to_network_option(external_client_library) ->
    ?FDB_NET_OPTION_EXTERNAL_CLIENT_LIBRARY;
to_network_option(external_client_directory) ->
    ?FDB_NET_OPTION_EXTERNAL_CLIENT_DIRECTORY;
to_network_option(disable_local_client) ->
    ?FDB_NET_OPTION_DISABLE_LOCAL_CLIENT;
to_network_option(client_threads_per_version) ->
    ?FDB_NET_OPTION_CLIENT_THREADS_PER_VERSION;
to_network_option(retain_client_library_copies) ->
    ?FDB_NET_OPTION_RETAIN_CLIENT_LIBRARY_COPIES;
to_network_option(disable_client_statistics_logging) ->
    ?FDB_NET_OPTION_DISABLE_CLIENT_STATISTICS_LOGGING;
to_network_option(enable_slow_task_profiling) ->
    ?FDB_NET_OPTION_ENABLE_SLOW_TASK_PROFILING;
to_network_option(enable_run_loop_profiling) ->
    ?FDB_NET_OPTION_ENABLE_RUN_LOOP_PROFILING;
to_network_option(buggify_enable) ->
    ?FDB_NET_OPTION_BUGGIFY_ENABLE;
to_network_option(buggify_disable) ->
    ?FDB_NET_OPTION_BUGGIFY_DISABLE;
to_network_option(buggify_section_activated_probability) ->
    ?FDB_NET_OPTION_BUGGIFY_SECTION_ACTIVATED_PROBABILITY;
to_network_option(buggify_section_fired_probability) ->
    ?FDB_NET_OPTION_BUGGIFY_SECTION_FIRED_PROBABILITY;
to_network_option(distributed_client_tracer) ->
    ?FDB_NET_OPTION_DISTRIBUTED_CLIENT_TRACER;
to_network_option(_) ->
    error(badarg).
