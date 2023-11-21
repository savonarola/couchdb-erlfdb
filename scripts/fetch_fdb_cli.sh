#!/usr/bin/env bash

set -euo pipefail

usage() {
    echo "Usage: $0 <deb|rpm> <OUTPUT Directory>"
    echo ""
    echo "Download the FoundationDB client binary package into specific directory"
    exit 1
}

if [ "$#" -ne 3 ]; then
    usage
fi

FDB_VERSION="7.1.43"
TYPE="${1}"
OUTPUT="${2}"

case "$TYPE" in
    deb)
        LINK="https://github.com/apple/foundationdb/releases/download/${FDB_VERSION}/foundationdb-clients_${FDB_VERSION}-1_amd64.deb"
        ;;
    rpm)
        LINK="https://github.com/apple/foundationdb/releases/download/${FDB_VERSION}/foundationdb-clients-${FDB_VERSION}-1.el7.x86_64.rpm"
        ;;
esac

wget -P "$OUTPUT" "$LINK"
