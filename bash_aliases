#! /bin/bash
#
# Standard BASH aliases
#
#
#

alias purge="/bin/rm *~"
alias ls='ls -F'
alias wopr='ssh -YA wopr'

# Show all environment variables (with their values) which
# contain the argument string.  (pth PATH)
evl()
{
  printenv | grep ${1} | sort
}

# Echo a PATH type variable and split by colons for a nicer display
pth()
{
  echo ${1} | tr ':' '\n'
}

# Go up some levels in the dir structure
u()     { cd ..; }
uu()    { cd ../..; }
uuu()   { cd ../../..; }
uuuu()  { cd ../../../..; }
uuuuu() { cd ../../../../..; }

################ PATH Manipulation ###################################

# Move a path member from a random place in the path to the end
addpath-clean()
{
    delpath $* 
    eval "$1=\$$1:$2"
}
# Move a path member from a random place in the path to the beginning
addpath2-clean()
{
    delpath $* 
    eval "$1=$2:\$$1"
}
# add to end of path
addpath()
{
    if ! eval test -z \"\${$1##*:$2:*}\" -o -z \"\${$1%%*:$2}\" -o -z \"\${$1##$2:*}\" ; then
      eval "$1=\$$1:$2"
    fi
}
# add to front of path
addpath2()
{
    if ! eval test -z \"\${$1##*:$2:*}\" -o -z \"\${$1%%*:$2}\" -o -z \"\${$1##$2:*}\" ; then
      eval "$1=$2:\$$1"
    fi
}
# delete from path
delpath()
{
    eval "$1=\$(echo \$$1 | /bin/sed -e s%^$2\:%% -e s%:$2\:%:%g -e s%:$2\\\$%%)"
}


####################### DEV environments ###################
goDevel()
{
    source ~/.environment/zoox_devel
    cd ${WORKSPACE}
}

goClams()
{
    source ~/.environment/zoox_clams
    cd ${WORKSPACE}
}

goInfra()
{
    source ~/.environment/zoox_infra $*
    cd ${INFRA_ROOT}
}

goBazel()
{
    source ~/.environment/zoox_driving_bazel
    cd ${WORKSPACE}
}

