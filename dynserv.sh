cd $(dirname $0)
SBCL_HOME=$(./sbcl-home.sh)
if [ -f $SBCL_HOME/dynserv.core ]
then
    echo "Now using dynserv.core"
    LISP_CORE=$SBCL_HOME/dynserv.core
else
    LISP_CORE=$SBCL_HOME/sbcl.core
fi

./sbcl.sh $LISP_CORE $@
