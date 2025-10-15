

################################################################
# this is a template for environment variables that need to be set
# this vary by host name, host type, username, etc

#
#  Also remember to copy $LISP_LIB/SBCLRC to your home directory
#  as .sbclrc if you intend to use SBCL as a standalone
#

# enable loading of astro lisp setup upon lisp startup
export ASTROLISP=yes 

# ideally LISP_LIB should be defined earlier but here are fallbacks
if [ -z $LISP_LIB ]; then
    if [ `uname` == "Darwin" ] ; then
	LISP_LIB=/Users/Shared/Software/lisp-lib/
    fi
    if [ `uname` == "Linux" ] ; then
	LISP_LIB=/usr/local/Software/lisp-lib/
    fi
fi

if [ ! -d $LISP_LIB ] ; then
    echo "ERROR - LISP_LIB = $LISP_LIB is not a valid directory.  Cannot initialize Lisp"
    exit 1
fi



# sometimes terapix is spread among several directories because some
# programs are custom compiled
TERAPIX_DIRECTORY=/usr/local/bin/:/usr/bin/:/opt/local/bin
VIZQUERY_PROGRAM=/opt/local/bin/vizquery
VIZQUERY_SITE=cfa
DS9_PROGRAM=/usr/local/bin/ds9

# please get and use your own STSci PSPS password
#  http://panstarrs.stsci.edu/PSI/login.php
PSPS_USERNAME=kleyna
PSPS_PASSWORD=2CI8D9S1

export LISP_LIB TERAPIX_DIRECTORY VIZQUERY_PROGRAM VIZQUERY_SITE\
       DS9_PROGRAM PSPS_USERNAME PSPS_PASSWORD

# the following variables enable lisp scripting
if [ -z "${SBCLRC+x}" ]; then
    PATH=$PATH:$LISP_LIB/astro/Scripts ; export PATH
fi

# Try the MacOS MacPorts version first			      
if [ -f /opt/local/bin/sbcl ] ; then
    export SBCL=/opt/local/bin/sbcl
# look in custom installs
elif [ -f /usr/local/bin/sbcl ] ; then
    export SBCL=/usr/local/bin/sbcl
# linux standard
elif [ -f /usr/bin/sbcl ] ; then
    export SBCL=/usr/bin/sbcl
else
    export SBCL=`which sbcl`
    if [ ! -f $SBCL ] ; then
	echo "Warning: SBCL not found"
    fi
fi
    
    
SBCLRC=$LISP_LIB/INIT-FILES/SBCLRC  ; export SBCLRC

################################################################
