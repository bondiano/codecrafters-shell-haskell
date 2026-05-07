#!/bin/sh
#
# This script is used to run your program on CodeCrafters
#
# This runs after .codecrafters/compile.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

# The codecrafters tester (gopsutil v3.24.5) calls `pgrep -P <pid>` to find
# child processes, but `pgrep` is missing from the Haskell runtime image.
# Install a small shim into a system PATH so the tester can find it.
install_pgrep_shim() {
    if command -v pgrep >/dev/null 2>&1; then
        return 0
    fi
    for d in /usr/local/bin /usr/bin /bin; do
        if [ -d "$d" ] && [ -w "$d" ]; then
            target="$d/pgrep"
            cat >"$target" <<'PGREP_EOF'
#!/bin/sh
# Minimal pgrep shim. Supports only `pgrep -P <ppid>` form.
if [ "$1" != "-P" ] || [ -z "$2" ]; then
    exit 1
fi
target=$2
found=0
for stat in /proc/[0-9]*/stat; do
    [ -r "$stat" ] || continue
    line=$(cat "$stat" 2>/dev/null) || continue
    pid=${line%% *}
    rest=${line##*) }
    set -- $rest
    ppid=$2
    if [ "$ppid" = "$target" ]; then
        echo "$pid"
        found=1
    fi
done
[ "$found" = 1 ] || exit 1
PGREP_EOF
            chmod +x "$target" 2>/dev/null && return 0
        fi
    done
    return 0
}
install_pgrep_shim || true

stackInstallRoot=$(cd $(dirname "$0") && stack path --local-install-root) # Fetch the path from within the project directory
exec "$stackInstallRoot/bin/codecrafters-shell-exe" +RTS -N1 -RTS "$@"
