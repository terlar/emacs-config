#!/usr/bin/env bash

[[ "\${TRACE}" ]] && set -o xtrace
set -o errexit -o pipefail -o nounset
shopt -s nullglob

main() {
$0
}

main "\$@"
