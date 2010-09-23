START_PATH=$(pwd)
LISP_LIBS=$1
COMPILED_LIBS="dynserv-compiled-libs"
shift
ASDF_SYSTEMS=$@

cd $(dirname $0) \
&& START_PATH=$START_PATH ./sbcl.sh $(./sbcl-home.sh)/sbcl.core \
--load sbclrc-for-daemon.lisp --load provide-dynserv.lisp \
$LISP_LIBS $COMPILED_LIBS :restas :swank $ASDF_SYSTEMS
