AC_INIT([ecs], [0.1.0], [bug-automake@gnu.org])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_PROG_CC
AC_PROG_YACC
AC_PROG_LEX([noyywrap])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([
 Makefile
 src/Makefile
 test/Makefile
 ])
AC_CHECK_HEADERS([gc.h],[], [AC_MSG_ERROR([required libraries are missing])])
AC_CHECK_HEADERS([X11/Xlib.h],[], [AC_MSG_ERROR([required libraries are missing])])
AC_CHECK_HEADERS([X11/Xutil.h],[], [AC_MSG_ERROR([required libraries are missing])])
AC_OUTPUT
