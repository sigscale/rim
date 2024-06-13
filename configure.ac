## configure.ac
## vim: ts=3
##
## Process this file with autoconf to produce a configure script.
##
## Boilerplate:  standard boilerplate code
##
AC_INIT([sigscale_im], [2.0.5], [issues@sigscale.org])
AC_COPYRIGHT([Copyright (c) 2018-2024 SigScale Global Inc.])
AC_PREREQ(2.63)
AC_CONFIG_SRCDIR([src/im_app.erl])
AC_CONFIG_HEADERS([config.h])
AC_LANG([Erlang])
AM_INIT_AUTOMAKE([-Wno-portability])

##
## Options:  control options to configure, support code for the options
##
AC_ARG_ENABLE([debug],
		[AS_HELP_STRING([--enable-debug], [enable debugging (default is no)])])
PKG_PROG_PKG_CONFIG
AC_ARG_WITH([systemdsystemunitdir],
		[AS_HELP_STRING([--with-systemdsystemunitdir=DIR], [Directory for systemd service files])],,
		[with_systemdsystemunitdir=auto])
AS_IF([test "x$with_systemdsystemunitdir" = "xyes" -o "x$with_systemdsystemunitdir" = "xauto"],
		[def_systemdsystemunitdir=$($PKG_CONFIG --variable=systemdsystemunitdir systemd)
		AS_IF([test "x$def_systemdsystemunitdir" = "x"],
				[AS_IF([test "x$with_systemdsystemunitdir" = "xyes"],
						[AC_MSG_ERROR([systemd support requested but pkg-config unable to query systemd package])],
						[with_systemdsystemunitdir=no])],
				[with_systemdsystemunitdir="$def_systemdsystemunitdir"])])
AS_IF([test "x$with_systemdsystemunitdir" != "xno"],
		[AC_SUBST([systemdsystemunitdir], [$with_systemdsystemunitdir])])
AM_CONDITIONAL([HAVE_SYSTEMD], [test "x$with_systemdsystemunitdir" != "xno"])


##
## Programs:  check for programs needed by the configure process,
##            the build process, or by one of the programs being built
##
AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC
if test -z "${ERLCFLAGS}" ; then
	ERLCFLAGS="-W -v"
fi
if test "x$enable_debug" == xyes ; then
	ERLCFLAGS="$ERLCFLAGS +debug_info"
fi
AC_ERLANG_SUBST_LIB_DIR
if test -z "${ERLANG_INSTALL_LIB_DIR}" ; then
	ERLANG_INSTALL_LIB_DIR=$ERLANG_LIB_DIR
fi
AC_ERLANG_SUBST_ROOT_DIR
AC_PROG_MKDIR_P
AC_PROG_INSTALL
AC_PROG_LN_S
AC_ERLANG_SUBST_INSTALL_LIB_DIR
appdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION
erldir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/src
beamdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/ebin
eincludedir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/include
docdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/doc
schemadir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/priv/schema
examplesdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/priv/examples
wwwdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/priv/www
scriptdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/priv/bin
reldir=$ERLANG_INSTALL_LIB_DIR/../releases
envdir=/etc/default
AC_SUBST(appdir)
AC_SUBST(erldir)
AC_SUBST(beamdir)
AC_SUBST(eincludedir)
AC_SUBST(privdir)
AC_SUBST(schemadir)
AC_SUBST(examplesdir)
AC_SUBST(wwwdir)
AC_SUBST(scriptdir)
AC_SUBST(reldir)
AC_SUBST(envdir)


##
## Libraries:  check for libraries
##
AC_ERLANG_SUBST_ERTS_VER
AC_ERLANG_CHECK_LIB([kernel], [], [AC_MSG_ERROR([kernel was not found!])])
AC_ERLANG_CHECK_LIB([stdlib], [], [AC_MSG_ERROR([stdlib was not found!])])
AC_ERLANG_CHECK_LIB([sasl], [], [AC_MSG_ERROR([sasl was not found!])])
AC_ERLANG_CHECK_LIB([mnesia], [], [AC_MSG_ERROR([mnesia was not found!])])
AC_ERLANG_CHECK_LIB([crypto], [], [AC_MSG_ERROR([crypto was not found!])])
AC_ERLANG_CHECK_LIB([asn1], [], [AC_MSG_ERROR([asn1 was not found!])])
AC_ERLANG_CHECK_LIB([public_key], [], [AC_MSG_ERROR([public_key was not found!])])
AC_ERLANG_CHECK_LIB([ssl], [], [AC_MSG_ERROR([ssl was not found!])])
AC_ERLANG_CHECK_LIB([inets], [], [AC_MSG_ERROR([inets was not found!])])
AC_ERLANG_CHECK_LIB([xmerl], [], [AC_MSG_ERROR([xmerl was not found!])])
AC_ERLANG_CHECK_LIB([parsetools], [], [AC_MSG_ERROR([parsetools was not found!])])
AC_ERLANG_CHECK_LIB([zj], [], [AC_MSG_ERROR([zj was not found!])])


##
## Headers:  check for header files
##


##
## Typedefs & Structures:  check for typedefs, structures,
##                         and compiler characteristics.
##


##
## Functions:  check for library functions
##


##
## Output:  create output files
##

AC_OUTPUT([Makefile src/Makefile include/Makefile ebin/Makefile
		doc/Makefile priv/Makefile priv/schema/Makefile
		priv/examples/Makefile priv/www/Makefile priv/www/package.json
		test/Makefile sys.config $PACKAGE_NAME.rel
		scripts/im scripts/im.service])

