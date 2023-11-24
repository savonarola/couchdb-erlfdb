// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

// General
ATOM_MAP(ok);
ATOM_MAP(error);

ATOM_MAP(true);
ATOM_MAP(false);

ATOM_MAP(ready);

ATOM_MAP(lt);
ATOM_MAP(lteq);
ATOM_MAP(gt);
ATOM_MAP(gteq);

ATOM_MAP(not_found);

ATOM_MAP(erlfdb_error);
ATOM_MAP(erlfdb_future);
ATOM_MAP(erlfdb_database);
ATOM_MAP(erlfdb_tenant);
ATOM_MAP(erlfdb_transaction);

ATOM_MAP(invalid_future_type);

ATOM_MAP(writes_not_allowed);


// Conflict Range Types
ATOM_MAP(read);
ATOM_MAP(write);


// Error Predicates
ATOM_MAP(retryable);
ATOM_MAP(maybe_committed);
ATOM_MAP(retryable_not_committed);
