
A package for setting up a data directory for a given lisp system, so
data doesn't have to be saved in git tree.

By default, data is put into

${LISP_LIB}/SYSTEM-DATA/<package-name>

But it can be overridden by environment variable $LISP_LIB_DATADIR  as
${LISP_LIB_DATADIR}/<package-name>

The directory is ordinary created with :GROUP rwx permission
but variable JK-DATADIR:*DEFAULT-PERMISSION* can be :ALL, :GROUP, :USER

The method

  (JK-DATADIR:GET-DATADIR-FOR-SYSTEM <system> :create t :permission :all)
    or
  (JK-DATADIR:GET-DATADIR-FOR-SYSTEM "system-name" ...)
  
will get (and bu default create) the data dir for an asdf system or
system name.
