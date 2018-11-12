#!/usr/bin/env sh

[ "\${TRACE}" ] && set -o xtrace
set -o errexit -o nounset

main() {
$0
}

main "\$@"
