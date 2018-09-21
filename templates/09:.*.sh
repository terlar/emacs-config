#!/usr/bin/env sh

[ "\${TRACE}" ] && set -o xtrace
set -o errexit -o pipefail -o nounset

main() {
$0
}

main "\$@"
