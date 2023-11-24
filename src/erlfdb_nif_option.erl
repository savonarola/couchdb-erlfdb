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

-export([
    to_network_option/1,
    to_database_option/1,
    to_transaction_option/1,
    to_stream_mode/1,
    to_mutation_type/1
]).

-include("fdb_options.hrl").

%% Some options are not included in the official C API
-define(FDB_TR_EXTRA_OPTION_ALLOW_WRITES, -10000).
-define(FDB_TR_EXTRA_OPTION_DISALLOW_WRITES, -10001).

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

to_database_option(location_cache_size) ->
    ?FDB_DB_OPTION_LOCATION_CACHE_SIZE;
to_database_option(max_watches) ->
    ?FDB_DB_OPTION_MAX_WATCHES;
to_database_option(machine_id) ->
    ?FDB_DB_OPTION_MACHINE_ID;
to_database_option(datacenter_id) ->
    ?FDB_DB_OPTION_DATACENTER_ID;
to_database_option(snapshot_ryw_enable) ->
    ?FDB_DB_OPTION_SNAPSHOT_RYW_ENABLE;
to_database_option(snapshot_ryw_disable) ->
    ?FDB_DB_OPTION_SNAPSHOT_RYW_DISABLE;
to_database_option(transaction_logging_max_field_length) ->
    ?FDB_DB_OPTION_TRANSACTION_LOGGING_MAX_FIELD_LENGTH;
to_database_option(transaction_timeout) ->
    ?FDB_DB_OPTION_TRANSACTION_TIMEOUT;
to_database_option(transaction_retry_limit) ->
    ?FDB_DB_OPTION_TRANSACTION_RETRY_LIMIT;
to_database_option(transaction_max_retry_delay) ->
    ?FDB_DB_OPTION_TRANSACTION_MAX_RETRY_DELAY;
to_database_option(transaction_size_limit) ->
    ?FDB_DB_OPTION_TRANSACTION_SIZE_LIMIT;
to_database_option(transaction_causal_read_risky) ->
    ?FDB_DB_OPTION_TRANSACTION_CAUSAL_READ_RISKY;
to_database_option(transaction_include_port_in_address) ->
    ?FDB_DB_OPTION_TRANSACTION_INCLUDE_PORT_IN_ADDRESS;
to_database_option(transaction_bypass_unreadable) ->
    ?FDB_DB_OPTION_TRANSACTION_BYPASS_UNREADABLE;
to_database_option(use_config_database) ->
    ?FDB_DB_OPTION_USE_CONFIG_DATABASE;
to_database_option(test_causal_read_risky) ->
    ?FDB_DB_OPTION_TEST_CAUSAL_READ_RISKY;
to_database_option(_) ->
    error(badarg).

to_transaction_option(causal_write_risky) ->
    ?FDB_TR_OPTION_CAUSAL_WRITE_RISKY;
to_transaction_option(causal_read_risky) ->
    ?FDB_TR_OPTION_CAUSAL_READ_RISKY;
to_transaction_option(causal_read_disable) ->
    ?FDB_TR_OPTION_CAUSAL_READ_DISABLE;
to_transaction_option(include_port_in_address) ->
    ?FDB_TR_OPTION_INCLUDE_PORT_IN_ADDRESS;
to_transaction_option(next_write_no_write_conflict_range) ->
    ?FDB_TR_OPTION_NEXT_WRITE_NO_WRITE_CONFLICT_RANGE;
to_transaction_option(read_your_writes_disable) ->
    ?FDB_TR_OPTION_READ_YOUR_WRITES_DISABLE;
to_transaction_option(read_ahead_disable) ->
    ?FDB_TR_OPTION_READ_AHEAD_DISABLE;
to_transaction_option(durability_datacenter) ->
    ?FDB_TR_OPTION_DURABILITY_DATACENTER;
to_transaction_option(durability_risky) ->
    ?FDB_TR_OPTION_DURABILITY_RISKY;
to_transaction_option(durability_dev_null_is_web_scale) ->
    ?FDB_TR_OPTION_DURABILITY_DEV_NULL_IS_WEB_SCALE;
to_transaction_option(priority_system_immediate) ->
    ?FDB_TR_OPTION_PRIORITY_SYSTEM_IMMEDIATE;
to_transaction_option(priority_batch) ->
    ?FDB_TR_OPTION_PRIORITY_BATCH;
to_transaction_option(initialize_new_database) ->
    ?FDB_TR_OPTION_INITIALIZE_NEW_DATABASE;
to_transaction_option(access_system_keys) ->
    ?FDB_TR_OPTION_ACCESS_SYSTEM_KEYS;
to_transaction_option(read_system_keys) ->
    ?FDB_TR_OPTION_READ_SYSTEM_KEYS;
to_transaction_option(raw_access) ->
    ?FDB_TR_OPTION_RAW_ACCESS;
to_transaction_option(debug_dump) ->
    ?FDB_TR_OPTION_DEBUG_DUMP;
to_transaction_option(debug_retry_logging) ->
    ?FDB_TR_OPTION_DEBUG_RETRY_LOGGING;
to_transaction_option(transaction_logging_enable) ->
    ?FDB_TR_OPTION_TRANSACTION_LOGGING_ENABLE;
to_transaction_option(debug_transaction_identifier) ->
    ?FDB_TR_OPTION_DEBUG_TRANSACTION_IDENTIFIER;
to_transaction_option(log_transaction) ->
    ?FDB_TR_OPTION_LOG_TRANSACTION;
to_transaction_option(transaction_logging_max_field_length) ->
    ?FDB_TR_OPTION_TRANSACTION_LOGGING_MAX_FIELD_LENGTH;
to_transaction_option(server_request_tracing) ->
    ?FDB_TR_OPTION_SERVER_REQUEST_TRACING;
to_transaction_option(timeout) ->
    ?FDB_TR_OPTION_TIMEOUT;
to_transaction_option(retry_limit) ->
    ?FDB_TR_OPTION_RETRY_LIMIT;
to_transaction_option(max_retry_delay) ->
    ?FDB_TR_OPTION_MAX_RETRY_DELAY;
to_transaction_option(size_limit) ->
    ?FDB_TR_OPTION_SIZE_LIMIT;
to_transaction_option(snapshot_ryw_enable) ->
    ?FDB_TR_OPTION_SNAPSHOT_RYW_ENABLE;
to_transaction_option(snapshot_ryw_disable) ->
    ?FDB_TR_OPTION_SNAPSHOT_RYW_DISABLE;
to_transaction_option(lock_aware) ->
    ?FDB_TR_OPTION_LOCK_AWARE;
to_transaction_option(used_during_commit_protection_disable) ->
    ?FDB_TR_OPTION_USED_DURING_COMMIT_PROTECTION_DISABLE;
to_transaction_option(read_lock_aware) ->
    ?FDB_TR_OPTION_READ_LOCK_AWARE;
to_transaction_option(use_provisional_proxies) ->
    ?FDB_TR_OPTION_USE_PROVISIONAL_PROXIES;
to_transaction_option(report_conflicting_keys) ->
    ?FDB_TR_OPTION_REPORT_CONFLICTING_KEYS;
to_transaction_option(special_key_space_relaxed) ->
    ?FDB_TR_OPTION_SPECIAL_KEY_SPACE_RELAXED;
to_transaction_option(special_key_space_enable_writes) ->
    ?FDB_TR_OPTION_SPECIAL_KEY_SPACE_ENABLE_WRITES;
to_transaction_option(tag) ->
    ?FDB_TR_OPTION_TAG;
to_transaction_option(auto_throttle_tag) ->
    ?FDB_TR_OPTION_AUTO_THROTTLE_TAG;
to_transaction_option(span_parent) ->
    ?FDB_TR_OPTION_SPAN_PARENT;
to_transaction_option(expensive_clear_cost_estimation_enable) ->
    ?FDB_TR_OPTION_EXPENSIVE_CLEAR_COST_ESTIMATION_ENABLE;
to_transaction_option(bypass_unreadable) ->
    ?FDB_TR_OPTION_BYPASS_UNREADABLE;
to_transaction_option(use_grv_cache) ->
    ?FDB_TR_OPTION_USE_GRV_CACHE;
to_transaction_option(allow_writes) ->
    ?FDB_TR_EXTRA_OPTION_ALLOW_WRITES;
to_transaction_option(disallow_writes) ->
    ?FDB_TR_EXTRA_OPTION_DISALLOW_WRITES;
to_transaction_option(_) ->
    error(badarg).

to_stream_mode(stream_want_all) ->
    ?FDB_STREAMING_MODE_WANT_ALL;
to_stream_mode(stream_iterator) ->
    ?FDB_STREAMING_MODE_ITERATOR;
to_stream_mode(stream_exact) ->
    ?FDB_STREAMING_MODE_EXACT;
to_stream_mode(stream_small) ->
    ?FDB_STREAMING_MODE_SMALL;
to_stream_mode(stream_medium) ->
    ?FDB_STREAMING_MODE_MEDIUM;
to_stream_mode(stream_large) ->
    ?FDB_STREAMING_MODE_LARGE;
to_stream_mode(stream_serial) ->
    ?FDB_STREAMING_MODE_SERIAL;
to_stream_mode(_) ->
    error(badarg).

to_mutation_type(add) ->
    ?FDB_MUTATION_TYPE_ADD;
to_mutation_type(bit_and) ->
    ?FDB_MUTATION_TYPE_BIT_AND;
to_mutation_type(bit_or) ->
    ?FDB_MUTATION_TYPE_BIT_OR;
to_mutation_type(bit_xor) ->
    ?FDB_MUTATION_TYPE_BIT_XOR;
to_mutation_type(append_if_fits) ->
    ?FDB_MUTATION_TYPE_APPEND_IF_FITS;
to_mutation_type(max) ->
    ?FDB_MUTATION_TYPE_MAX;
to_mutation_type(min) ->
    ?FDB_MUTATION_TYPE_MIN;
to_mutation_type(set_versionstamped_key) ->
    ?FDB_MUTATION_TYPE_SET_VERSIONSTAMPED_KEY;
to_mutation_type(set_versionstamped_value) ->
    ?FDB_MUTATION_TYPE_SET_VERSIONSTAMPED_VALUE;
to_mutation_type(byte_min) ->
    ?FDB_MUTATION_TYPE_BYTE_MIN;
to_mutation_type(byte_max) ->
    ?FDB_MUTATION_TYPE_BYTE_MAX;
to_mutation_type(compare_and_clear) ->
    ?FDB_MUTATION_TYPE_COMPARE_AND_CLEAR;
to_mutation_type(_) ->
    error(badarg).
