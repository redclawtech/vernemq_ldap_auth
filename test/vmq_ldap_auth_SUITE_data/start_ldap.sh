#!/bin/sh

set -x

$bind_port=$1

uri="ldap://localhost:$tcp_port"

SLAPADD=/usr/sbin/slapadd
SLAPD=/usr/sbin/slapd

setup_slapd() {
    set -e
    mkdir -p $WORKDIR/ldap
    $SLAPD -h "$uri" -f $ARTIFACTS/slapd.conf &
    attempts=0
    until ldapsearch -x -H "$uri" -b "dc=example,dc=com" '(objectclass=*)'; do
        attempts=$(($attempts + 1))
        if [[ $attempts -gt 10 ]]; then
        echo "failed to connect to slapd :("
        exit 1
        fi
        sleep 1
    done
    set +e
}

