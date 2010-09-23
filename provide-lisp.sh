#!/bin/sh
rm -rf temp \
&& mkdir temp \
&& tar -xjvf lisp-dists/sbcl-1.0.42-x86-linux-binary.tar.bz2 --directory temp \
&& tar -xjvf lisp-dists/sbcl-1.0.42-source.tar.bz2 --directory temp \
&& ln -s ../output/sbcl.core temp/sbcl-1.0.42-x86-linux/contrib/sbcl.core \
&& cd temp/sbcl-1.0.42/ \
&& PATH=../sbcl-1.0.42-x86-linux/src/runtime/:$PATH SBCL_HOME=../sbcl-1.0.42-x86-linux/contrib sh make.sh \
&& INSTALL_ROOT=$PWD/../../lisp sh install.sh \
&& cd ../../ \
&& rm -rf temp \
#&& PATH=lisp/bin:$PATH SBCL_HOME=lisp/lib/sbcl sbcl \

