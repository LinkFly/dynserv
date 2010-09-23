#DYNSERV = $(ls dynserv)
if [ -f dynserv ] 
then
    ./dynserv $@
else
    ./sbcl $@
fi