#! /bin/bash
#
# Standard BASH aliases
#
#
#

alias purge="rm -f *~"





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

alias goDevel="source ~/.environment/zoox_devel;cd ~/zoox_ros_ws/"
alias goClams="source ~/.environment/zoox_clams;cd ~/zoox_clams_ws/"

alias goTest="source ~/.environment/zoox_test;cd ~/test_catkin_ws/"

