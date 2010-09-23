cd $(dirname $0)
PATH=$(./sbcl-path.sh):$PATH
SBCL_HOME=$(./sbcl-home.sh)
LISP_CORE=$1
shift

PATH=$PATH SBCL_HOME=$SBCL_HOME \
sbcl --core $LISP_CORE \
--noinform --no-userinit --no-sysinit $@
