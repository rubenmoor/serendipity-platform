#!/usr/bin/env bash

set -e
if [ $# -ne 1 ]
then
        echo "Usage: $0 server/ip-address"
        exit 1
fi

set -x

server=$1

path=$(nix-build ./server.nix --no-out-link)
nix-copy-closure --to --use-substitutes $server $path
profile="/nix/var/nix/profiles/system"
ssh $server << EOF
nix-env --profile $profile --set $path
$profile/bin/switch-to-configuration switch
EOF
