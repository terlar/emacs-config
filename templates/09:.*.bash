#!/usr/bin/env bash

[[ "\${TRACE}" ]] && set -x
set -eou pipefail
shopt -s nullglob

main() {
$0
}

main "\$@"
