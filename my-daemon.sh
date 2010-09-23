DEFAULT_DAEMON_CONF=default-daemon.conf
CL_SOURCE_REGISTRY=./
if [ ! $DAEMON_CONF ]
then
    DAEMON_CONF=$DEFAULT_DAEMON_CONF
fi
START_PATH=$START_PATH CL_SOURCE_REGISTRY=$CL_SOURCE_REGISTRY \
./dynserv.sh --load sbclrc-for-daemon.lisp --load my-restas-daemon.lisp $DAEMON_CONF $1
