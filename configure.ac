qdnl Process this with autoconf to create configure
AC_INIT([kawa],[3.1.1]) dnl Also update bin/configure.ac and build.xml.
AC_CONFIG_SRCDIR([doc/kawa.texi])
AC_CONFIG_AUX_DIR([build-aux])

AM_MAINTAINER_MODE([enable])
AC_CANONICAL_HOST

AM_INIT_AUTOMAKE([info-in-builddir foreign])

AC_ARG_WITH(docbook-stylesheets,
  AS_HELP_STRING(--with-docbook-stylesheets=path,Building nicely-formatted documentation))

dnl AC_ARG_WITH(classpath,[  --with-classpath          Path to Java classes], classpath=$withval, classpath=$$CLASSPATH)
AC_ARG_ENABLE(kawa-frontend,
  [  --enable-kawa-frontend  Build "kawa" front-end program using readline],
  enable_kawa_frontend=$enableval, enable_kawa_frontend=no)

AC_ARG_ENABLE(xml,
  AS_HELP_STRING(--disable-xml,Don't build support for XML processing),
  enable_xml=$enableval, enable_xml=yes)

AC_ARG_WITH(servlet,
  AS_HELP_STRING(--with-servlet@<:@=servlet-api.jar@:>@,Build support for generating servlets))
WITH_SERVLET_ARG="${with_servlet}"

AC_ARG_WITH(android,
  AS_HELP_STRING(--with-android@<:@=android.jar@:>@,Build for the Android platform))

AC_ARG_WITH(javafx,
 AS_HELP_STRING(--with-javafx@<:@=javafx-sdk-home@:>@,Enable support for JavaFX))
WITH_JAVAFX_ARG="${with_javafx}"

AC_ARG_WITH(lsp4j,
  AS_HELP_STRING(--with-lsp4j=<:@=lsp4j.jar@>))
AC_ARG_WITH(lsp4j.jsonrpc,
  AS_HELP_STRING(--with-lsp4j.jsonrpc=<:@=lsp4j.jsonrpc.jar@>))
AC_ARG_WITH(gson,
  AS_HELP_STRING(--with-gson=<:@=gson.jar@>))

AC_ARG_WITH(domterm,
 AS_HELP_STRING(--with-domterm=<:@=domterm_home:>@,Build support for DomTerm))
 WITH_DOMTERM_ARG="${with_domterm}"

AC_ARG_WITH(jline3,
 AS_HELP_STRING(--with-jline3=<:@=jline3.jar@:>@,Build support JLine3 input editing))

AC_ARG_WITH(XQTS,
  AS_HELP_STRING(--with-XQTS@<:@=XQTS-path@:>@,Support XQuery Test Suite))

AC_ARG_ENABLE(echo2,
  AS_HELP_STRING(--enable-echo2,Enable support for the Echo2 web toolkit))

AC_ARG_ENABLE(jemacs,
  AS_HELP_STRING(--enable-jemacs,Build JEmacs))

AC_ARG_ENABLE(xquery,
  AS_HELP_STRING(--enable-xquery,Build XQuery language support (Qexo) (default on)))

AC_ARG_ENABLE(brl,
  AS_HELP_STRING(--enable-brl,Build BRL emulation and KRL))

AC_ARG_WITH(httpserver,
  AS_HELP_STRING(--with-httpserver,Make use of JDK6's builtin http server))

AC_ARG_WITH(swing,
  AS_HELP_STRING(--with-swing,Assume Swing is available (default on)))

AC_ARG_WITH(swt,
  AS_HELP_STRING(--with-swt,Assume SWT is available))

AC_ARG_WITH(awt,
  AS_HELP_STRING(--with-awt,Assume AWT is available (default on)))

AC_ARG_WITH(sax2,
  AS_HELP_STRING(--with-sax2, Assume SAX2 is available (default on)))

AC_ARG_WITH(javax.script,
  AS_HELP_STRING(--with-javax.script, Use javax.script interfaces))

AC_ARG_WITH(java-source,
  AS_HELP_STRING(--with-java-source=NUM,Assume functionality of given Java/JDK version))

test -z "${with_java_source}" -a -n "${with_android}" \
  && with_java_source=android
test -z "${with_swing}" -a "$with_awt" == "no" \
  && with_swing=no

with_java_source=`echo ${with_java_source} | tr A-Z a-z`
# Java 8 is the (current) default
test "${with_java_source}" = "" && with_java_source=8
case "/${with_java_source}/" in
  /1/|/1.1*/) # JDK 1.1 - not supported
    JAVA_SOURCE=1; JAVA_SOURCE_SELECTOR='%java1';;
  /2/|/1.2*/|/1.3*/) # JDK 1.2+, GCJ-3.x - not supported
    JAVA_SOURCE=2; JAVA_SOURCE_SELECTOR='%java2' ;;
  /4/|/1.4*/) # JDK 1.4, GCJ-4.x, without JAXP - not supported
    JAVA_SOURCE=4; JAVA_SOURCE_SELECTOR='%java4' ;;
  /4x/|/1.4*+jaxp1.3/) # JDK 1.4, GCJ-4.x with JAXP 1.3 - not supported
    JAVA_SOURCE=4x; JAVA_SOURCE_SELECTOR='%java4x' ;;
  /5/|/1.5*/) # JDK 1.5
    JAVA_SOURCE=5; JAVA_SOURCE_SELECTOR='%java5' ;;
  /6/|/1.6*/) # JDK 1.6
    JAVA_SOURCE=6; JAVA_SOURCE_SELECTOR='%java6' ;;
  /6compat5/) # JDK 1.5 with some Java6 features.
    # Requires building with Java6, but runs on Java5 (without those features)
    # or on Java6 (with those features).
    JAVA_SOURCE=6compat5; JAVA_SOURCE_SELECTOR='%java6compat5' ;;
  /7/|/1.7*/) # JDK 1.7
    JAVA_SOURCE=7; JAVA_SOURCE_SELECTOR='%java7' ;;
  /8/|/1.8*/) # Java 8
    JAVA_SOURCE=8; JAVA_SOURCE_SELECTOR='%java8' ;;
  /9/|/1.9*/) # Java 9
    JAVA_SOURCE=9; JAVA_SOURCE_SELECTOR='%java9' ;;
  [/[1-9][0-9]/]) # Java 10-
    JAVA_SOURCE=${with_java_source};
    JAVA_SOURCE_SELECTOR="%java${with_java_source}"
    ;;
  /android*/)
        JAVA_SOURCE=android; JAVA_SOURCE_SELECTOR='%android'
        test -z "$with_swing" && with_swing="no"
        test -z "$with_awt" && with_awt="no"
        ;;
  /*/) AC_MSG_ERROR([Invalid version for --with-java-version.
  (Must be 5, 6, ..., or a JDK version such as 1.5.)]) ;;
esac
AC_SUBST(JAVA_SOURCE)
AC_SUBST(JAVA_SOURCE_SELECTOR)

if test -z "$with_sax2" ; then
  case "$JAVA_SOURCE" in
  1|2|3) with_sax2="no";;
  esac
fi

if test -z "$with_javax_script"; then
  case "$JAVA_SOURCE" in
  6compat5|6|7|8|9) with_javax_script="yes";;
  esac
fi

if test -z "$with_httpserver"; then
  case "$JAVA_SOURCE" in
  6compat5|6|7|8|9)
    test "$with_android" != "" -a "$with_android" != "no" \
      || with_httpserver="yes";;
  esac
fi

AC_PROG_INSTALL
AC_PROG_LN_S

TOP_BUILDDIR=`pwd`

JAVA=${JAVA-java}
JAVAC=${JAVAC-javac}
dnl needed to match _AM_DEPENDENCIES

JAVACFLAGS=${JAVACFLAGS--g}
case "$JAVA_SOURCE" in
  6compat5) JAVACFLAGS="${JAVACFLAGS} -target 1.5 -source 1.5" ;;
esac

filesep='/'

# Partly for compatibility (--enable/--disable-servlet was the old option);
# partly to allow --with-servlet=path-to-jar --disable-servlet.
# we essentially default enable-servlet to with-servlet. But we don't
# want to clutter up configure --help with --enable-servlet.
enable_servlet=${enable_servlet-${with_servlet}}

# Windows has a non-suitable find command.
AC_CACHE_CHECK([for Posix find command], [ac_cv_path_FIND],
            [AC_PATH_PROGS_FEATURE_CHECK([FIND], [find],
              [[findout=`$ac_path_FIND . -maxdepth 0 -type d -exec echo '{}' ';'`
                test "x$findout" = x"." \
                && ac_cv_path_FIND=$ac_path_FIND ac_path_FIND_found=:]],
              [AC_MSG_WARN([could not find Posix find]); ac_cv_path_FIND=find])])
          AC_SUBST([FIND], [$ac_cv_path_FIND])

AM_CONDITIONAL(ENABLE_ECHO2, test "$enable_echo2" = "yes")
AM_CONDITIONAL(WITH_ANDROID,
  test "$with_android" != "" -a "$with_android" != "no")
AM_CONDITIONAL(ENABLE_BRL, test "$enable_brl" = "yes")
AM_CONDITIONAL(ENABLE_JEMACS, test "$enable_jemacs" = "yes")
AM_CONDITIONAL(ENABLE_XQUERY, test "$enable_xquery" != "no")
AM_CONDITIONAL(ENABLE_XML, test "$enable_xml" = "yes")
AM_CONDITIONAL(ENABLE_SERVLET,
  test "$enable_servlet" != "" -a "$enable_servlet" != "no")
AM_CONDITIONAL(WITH_SWING, test "$with_swing" != "no")
AM_CONDITIONAL(WITH_LSP4J,
  test "$with_lsp4j" != "" -a "$with_lsp4j" != "no")
AM_CONDITIONAL(WITH_JLINE3,
  test "$with_jline3" != "" -a "$with_jline3" != "no")
AM_CONDITIONAL(WITH_DOMTERM,
  test "$with_domterm" != "" -a "$with_domterm" != "no")
AM_CONDITIONAL(WITH_JAVAFX,
  test "$with_javafx" != "" -a "$with_javafx" != "no")
AM_CONDITIONAL(WITH_JAVAFX_MODULE,
  test "$with_javafx" != "" -a "$with_javafx" != "no" -a "$with_javafx" != "yes")

AM_CONDITIONAL(WITH_SWT, test "$with_swt" = "yes")
AM_CONDITIONAL(WITH_AWT, test "$with_awt" != "no")
AM_CONDITIONAL(WITH_SAX2, test "$with_sax2" != "no")
AM_CONDITIONAL(WITH_JAVAX_SCRIPT, test "$with_javax_script" = "yes")
AM_CONDITIONAL(WITH_HTTPSERVER, test "$with_httpserver" = "yes")
AM_CONDITIONAL(WITH_DOCBOOK_STYLESHEETS, test "$with_docbook_stylesheets" != "" -a "$with_docbook_stylesheets" != "no")
AM_CONDITIONAL(WITH_XQTS, test "$with_XQTS" != "" -a "$with_XQTS" != "no")
AM_CONDITIONAL(ENABLE_KAWA_FRONTEND, test "$enable_kawa_frontend" = "yes")
AM_CONDITIONAL(ON_WINDOWS, test "$CLASSPATH_SEPARATOR" = ";")

if test -z "${ON_WINDOWS_FALSE}${ENABLE_KAWA_FRONTEND_FALSE}"
then
  KAWA_SCRIPT=kawa
else
  KAWA_SCRIPT=kawa.sh
fi
AC_SUBST(KAWA_SCRIPT)
AS_IF([test "${KAWA_SCRIPT}" == "kawa"],
  AC_CONFIG_FILES([bin/kawa:bin/kawa.sh.in],[chmod +x bin/kawa]),
  AC_CONFIG_FILES([bin/kawa.sh:bin/kawa.sh.in],[chmod +x bin/kawa.sh]))

if test "$with_docbook_stylesheets" == "" -o "$with_docbook_stylesheets" == "no" -o  "$with_docbook_stylesheets" == "yes"
then
DOCBOOK_STYLESHEETS_PATH='$(TOP_BUILDDIR)/docbook-xsl'
else
DOCBOOK_STYLESHEETS_PATH="${with_docbook_stylesheets}"
fi
AM_CONDITIONAL(WITH_DOCBOOK, test "$with_docbook_stylesheets" != "no")

if test "$with_jline3" != "" -a "$with_jline3" != "no"
then WITH_JLINE_PATH="${with_jline3}"
fi

if test "$with_gson" != "" -a "$with_gson" != "no"
then WITH_GSON_PATH="${with_gson}"
fi

if test "$with_lsp4j" != "" -a "$with_lsp4j" != "no"
then WITH_LSP4J_PATH="${with_lsp4j}"
fi

if test "$with_lsp4j_jsonrpc" != "" -a "$with_lsp4j_jsonrpc" != "no"
then WITH_LSP4J_JSONRPC_PATH="${with_lsp4j_jsonrpc}"
elif test -n "${WITH_LSP4J_PATH}"
then WITH_LSP4J_JSONRPC_PATH=`echo "${WITH_LSP4J_PATH}"|sed -e 's|\(org.eclipse.lsp4j\)\(.\)|\1.jsonrpc\2|g'`
fi

JEMACS_VERSION=`echo $VERSION | sed  -e 's/^1/0/'`
BYTECODE_VERSION="$VERSION"

m4_include(config.classpath)

case ${with_sax2} in
  yes) SAX2_PREPROCESS="+SAX2";;
  no) SAX2_PREPROCESS="-SAX2";;
  *) SAX2_PREPROCESS=""
esac

XQTS_PATH=${with_XQTS:-XQTS}
AC_SUBST(XQTS_PATH)
AC_SUBST(DOCBOOK_STYLESHEETS_PATH)
AC_SUBST(SAX2_PREPROCESS)
AC_SUBST(conf_classpath)
AC_SUBST(run_classpath)
AC_SUBST(CLASSPATH_SEPARATOR)
AC_SUBST(pathsep)
AC_SUBST(filesep)
AC_SUBST(WITH_SERVLET_ARG)
AC_SUBST(WITH_JAVAFX_ARG)
AC_SUBST(WITH_JAVAFX_MODULE)
AC_SUBST(WITH_JLINE3)
AC_SUBST(WITH_GSON_PATH)
AC_SUBST(WITH_JLINE_PATH)
AC_SUBST(WITH_LSP4J_PATH)
AC_SUBST(WITH_LSP4J_JSONRPC_PATH)
AC_SUBST(WITH_DOMTERM_ARG)

AC_SUBST(JAVA)
AC_SUBST(JAVAC)
AC_SUBST(JAVACFLAGS)
#These don't do anything, because Make-rules can't contain autoconf
#substitutions.  Perhaps fix when new automake is out.
AC_SUBST(TOP_BUILDDIR)
AC_SUBST(PACKAGE)
AC_SUBST(VERSION)
AC_SUBST(JEMACS_VERSION)
AC_SUBST(BYTECODE_VERSION)

if test -z "$KAWA_SHELL"; then
  if test -f /bin/bash; then KAWA_SHELL=/bin/bash
  elif test -f /bin/ksh; then KAWA_SHELL=/bin/ksh
  else KAWA_SHELL=${CONFIG_SHELL-/bin/sh}
  fi
fi
AC_SUBST(KAWA_SHELL)

Make_rules="$srcdir/Make-rules"
AC_SUBST_FILE(Make_rules)
AC_CONFIG_LINKS(
  testsuite/scribble.html:testsuite/scribble.html.in)

AC_CONFIG_SUBDIRS([bin])

AC_CONFIG_FILES([Makefile gnu/bytecode/Makefile
  gnu/Makefile gnu/kawa/servlet/Makefile
  gnu/brl/Makefile gnu/kawa/brl/Makefile
  kawa/lib/Makefile gnu/kawa/slib/Makefile
  gnu/commonlisp/lisp/Makefile gnu/commonlisp/testsuite/Makefile
  gnu/jemacs/lisp/Makefile gnu/jemacs/testsuite/Makefile
  gnu/q2/testsuite/Makefile
  gnu/xquery/testsuite/Makefile
  gnu/kawa/android/Makefile
  gnu/kawa/javafx/Makefile
  doc/Makefile testsuite/Makefile
  bin/kawa.bat])
AC_CONFIG_FILES([bin/qexo],[chmod +x bin/qexo])
AC_OUTPUT
