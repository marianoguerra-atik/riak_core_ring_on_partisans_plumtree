#!/bin/sh

# /bin/sh on Solaris is not a POSIX compatible shell, but /usr/bin/ksh is.
if [ `uname -s` = 'SunOS' -a "${POSIX_SHELL}" != "true" ]; then
    POSIX_SHELL="true"
    export POSIX_SHELL
    # To support 'whoami' add /usr/ucb to path
    PATH=/usr/ucb:$PATH
    export PATH
    exec /usr/bin/ksh $0 "$@"
fi
unset POSIX_SHELL # clear it so if we invoke other scripts, they run as ksh as well


SCRIPT=$(readlink $0 || true)
if [ -z $SCRIPT ]; then
    SCRIPT=$0
fi;


SCRIPT_DIR="$(cd `dirname "$SCRIPT"` && pwd -P)"
RELEASE_ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd -P)"
REL_NAME="{{ release_name }}"
CUTTLEFISH_CONF="{{ release_name }}.conf"
REL_VSN="{{ rel_vsn }}"
ERTS_VSN="{{ erts_vsn }}"
CODE_LOADING_MODE="${CODE_LOADING_MODE:-embedded}"
REL_DIR="$RELEASE_ROOT_DIR/releases/$REL_VSN"
ERL_OPTS="{{ erl_opts }}"
RUNNER_LOG_DIR="${RUNNER_LOG_DIR:-$RELEASE_ROOT_DIR/log}"
RUNNER_BASE_DIR=$RELEASE_ROOT_DIR
RUNNER_ETC_DIR="${RUNNER_ETC_DIR:-$RELEASE_ROOT_DIR/etc}"

find_erts_dir() {
    __erts_dir="$RELEASE_ROOT_DIR/erts-$ERTS_VSN"
    if [ -d "$__erts_dir" ]; then
        ERTS_DIR="$__erts_dir";
        ROOTDIR="$RELEASE_ROOT_DIR"
    else
        __erl="$(which erl)"
        code="io:format(\"~s\", [code:root_dir()]), halt()."
        __erl_root="$("$__erl" -noshell -eval "$code")"
        ERTS_DIR="$__erl_root/erts-$ERTS_VSN"
        ROOTDIR="$__erl_root"
    fi
}

# Get node pid
relx_get_pid() {
    if output="$(relx_nodetool rpcterms os getpid)"
    then
        echo "$output" | sed -e 's/"//g'
        return 0
    else
        echo "$output"
        return 1
    fi
}

relx_get_longname() {
    id="longname$(relx_gen_id)-${NAME}"
    "$BINDIR/erl" -boot start_clean -eval 'io:format("~s~n", [node()]), halt()' -noshell -name $id | sed -e 's/.*@//g'
}

# Connect to a remote node
relx_rem_sh() {
    # Generate a unique id used to allow multiple remsh to the same node
    # transparently
    id="remsh$(relx_gen_id)-${NAME}"

    # Get the node's ticktime so that we use the same thing.
    TICKTIME="$(relx_nodetool rpcterms net_kernel get_net_ticktime)"

    # Setup remote shell command to control node
    exec "$BINDIR/erl" "$NAME_TYPE" "$id" -remsh "$NAME" -boot start_clean \
         -boot_var ERTS_LIB_DIR "$ERTS_LIB_DIR" \
         -setcookie "$COOKIE" -hidden -kernel net_ticktime $TICKTIME
}

# Generate a random id
relx_gen_id() {
    od -X -N 4 /dev/urandom | head -n1 | awk '{print $2}'
}

# Control a node
relx_nodetool() {
    command="$1"; shift

    "$ERTS_DIR/bin/escript" "$ROOTDIR/bin/nodetool" "$NAME_TYPE" "$NAME" \
                                -setcookie "$COOKIE" "$command" $@
}

# Run an escript in the node's environment
relx_escript() {
    shift; scriptpath="$1"; shift
    export RELEASE_ROOT_DIR

    "$ERTS_DIR/bin/escript" "$ROOTDIR/$scriptpath" $@
}

# Output a start command for the last argument of run_erl
relx_start_command() {
    printf "exec \"%s\" \"%s\"" "$RELEASE_ROOT_DIR/bin/$REL_NAME" \
           "$START_OPTION"
}

# Make sure log directory exists
mkdir -p "$RUNNER_LOG_DIR"

# Use $CWD/sys.config if exists, otherwise releases/VSN/sys.config
if [ -z "$NAME_ARG" ]; then
    NODENAME=`egrep '^[ \t]*nodename[ \t]*=[ \t]*' $RUNNER_ETC_DIR/$CUTTLEFISH_CONF 2> /dev/null | tail -n 1 | cut -d = -f 2`
    if [ -z "$NODENAME" ]; then
        echo "vm.args needs to have a -name parameter."
        echo "  -sname is not supported."
        exit 1
    else
        NAME_TYPE="-name"
        NAME="${NODENAME# *}"
    fi
fi

PIPE_DIR="${PIPE_DIR:-/tmp/erl_pipes/$NAME/}"

# Extract the target cookie
#COOKIE_ARG=`grep -e '-setcookie' $RUNNER_ETC_DIR/vm.args`
if [ -z "$COOKIE_ARG" ]; then
    COOKIE=`egrep '^[ \t]*distributed_cookie[ \t]*=[ \t]*' $RUNNER_ETC_DIR/$CUTTLEFISH_CONF 2> /dev/null | cut -d = -f 2 | tr -d ' '`
    if [ -z "$COOKIE" ]; then
        echo "vm.args needs to have a -setcookie parameter."
        exit 1
    else
        COOKIE_ARG="-setcookie $COOKIE"
    fi
fi

find_erts_dir
export ROOTDIR="$RELEASE_ROOT_DIR"
export BINDIR="$ERTS_DIR/bin"
export EMU="beam"
export PROGNAME="erl"
export LD_LIBRARY_PATH="$ERTS_DIR/lib:$LD_LIBRARY_PATH"
ERTS_LIB_DIR="$ERTS_DIR/../lib"
CUTTLEFISHCMD="$ERTS_DIR/bin/escript $RUNNER_BASE_DIR/bin/cuttlefish"

cd "$ROOTDIR"

if CUTTLEFISH_CONFIG=$($CUTTLEFISHCMD -e $RUNNER_ETC_DIR -d $RUNNER_BASE_DIR/generated.conf -s $RUNNER_BASE_DIR/share/schema/ -c $RUNNER_ETC_DIR/$CUTTLEFISH_CONF)
then
    CONFIG_FILES="$CUTTLEFISH_CONFIG"
else
    echo "Cuttlefish failed! Oh no!"
    exit 1
fi


# Parse out release and erts info
START_ERL=`cat $RUNNER_BASE_DIR/releases/start_erl.data`
ERTS_VSN=${START_ERL% *}
APP_VSN=${START_ERL#* }

# TODO: look in the release otherwise use which
ESCRIPT=escript
NODETOOL_PATH=$RUNNER_BASE_DIR/bin
NODETOOL=$NODETOOL_PATH/nodetool
# Setup command to control the node
NODETOOL="$ESCRIPT $NODETOOL $NAME_ARG $COOKIE_ARG"

ensure_node_running()
{
    # Make sure the local node IS running
    if ! relx_nodetool "ping"; then
        echo "Node is not running!"
        exit 1
    fi
}

cluster_admin()
{
    case "$1" in
        join)
            if [ $# -eq 2 ]; then
                ensure_node_running
                relx_nodetool rpc {{ release_name }}_peer_service join_p "$2"
            else
                echo "Usage: $SCRIPT cluster join <node>"
                exit 1
            fi
            ;;
        leave)
            if [ $# -eq 2 ]; then
                ensure_node_running
                relx_nodetool rpc {{ release_name }}_peer_service leave_p "$2"
            else
                echo "Usage: $SCRIPT cluster leave <node>"
                exit 1
            fi
            ;;
        members)
            if [ $# -eq 1 ]; then
                ensure_node_running
                relx_nodetool rpc {{ release_name }}_peer_service members_p
            else
                echo "Usage: $SCRIPT cluster members"
                exit 1
            fi
            ;;
        connections)
            if [ $# -eq 1 ]; then
                ensure_node_running
                relx_nodetool rpc {{ release_name }}_peer_service connections_p
            else
                echo "Usage: $SCRIPT cluster members"
                exit 1
            fi
            ;;
        *)
            echo "\
Usage: $SCRIPT cluster <command>

The following commands stage changes to cluster membership. These commands
do not take effect immediately. After staging a set of changes, the staged
plan must be committed to take effect:

   join <node>                    Join node to the cluster containing <node>
   leave <node>                   Have this node leave <node>
   members                        List cluster members
   connections                    List cluster connections
"
    esac
}

# Check the first argument for instructions
case "$1" in

    cluster)
        shift
        cluster_admin "$@"
        ;;

    *)
        echo "Usage: $SCRIPT { cluster }"
        exit 1
        ;;
esac
