# Process this with automake to create Makefile.in

include $(top_srcdir)/source-list

CLASSPATH = .$(CLASSPATH_SEPARATOR)$(srcdir)$(CLASSPATH_SEPARATOR)@conf_classpath@$$CLASSPATH

FIXED_SUBDIRS = bin doc testsuite gnu/bytecode \
  gnu \
  kawa/lib gnu/kawa/slib gnu/kawa/servlet \
  gnu/commonlisp/lisp gnu/commonlisp/testsuite \
  gnu/q2/testsuite
ANDROID_SUBDIRS = gnu/kawa/android
BRL_SUBDIRS = gnu/brl gnu/kawa/brl
JAVAFX_SUBDIRS = gnu/kawa/javafx
JEMACS_SUBDIRS = gnu/jemacs/lisp gnu/jemacs/testsuite
XQUERY_SUBDIRS = gnu/xquery/testsuite

SUBDIRS = $(FIXED_SUBDIRS)
if ENABLE_JEMACS
  SUBDIRS += $(JEMACS_SUBDIRS)
endif
if ENABLE_XML
if ENABLE_BRL
SUBDIRS += $(BRL_SUBDIRS)
endif
if ENABLE_XQUERY
SUBDIRS += $(XQUERY_SUBDIRS)
endif
endif
if WITH_ANDROID
SUBDIRS += $(ANDROID_SUBDIRS)
endif
if WITH_JAVAFX
SUBDIRS += $(JAVAFX_SUBDIRS)
endif

DIST_SUBDIRS = $(FIXED_SUBDIRS) \
  $(ANDROID_SUBDIRS) \
  $(BRL_SUBDIRS) \
  $(JAVAFX_SUBDIRS) \
  $(JEMACS_SUBDIRS) \
  $(XQUERY_SUBDIRS)

javadir=$(datadir)/java

SED = sed
CHMOD = chmod
JAR = jar
FIND = @FIND@
KAWAJAR = kawa.jar
KAWALIB = $(datadir)/kawa/lib/$(KAWAJAR)
# external domterm-jar
DOMTERM_JAR = @WITH_DOMTERM_ARG@/share/domterm/domterm.jar

META_ScriptEngineFactory = META-INF/services/javax.script.ScriptEngineFactory
if WITH_JAVAX_SCRIPT
META_INF_FILES = $(META_ScriptEngineFactory)
endif

misc-classes.stamp: $(java_sources)
	if test -f misc-classes.stamp; then \
	  $(JAVAC) -d $(top_builddir) -classpath "$(CLASSPATH)" $(JAVACFLAGS) $?; \
	else \
	  $(MAKE) javac-all; \
	fi
	echo timestamp > misc-classes.stamp

javac-all: tmp-sources1.list
	$(JAVAC) -d $(top_builddir) -classpath "$(CLASSPATH)" $(JAVACFLAGS) @tmp-sources1.list

kawa-jar.stamp: misc-classes.stamp kawa/Version.class
	echo timestamp > kawa-jar.stamp

classes.list: ./kawa-jar.stamp
	$(FIND) gnu kawa -name '*.class' -print >classes.list

tmp-sources1.list: source-list Makefile.am
	(for f in $(sources_gnu_core); do echo $(srcdir)/$$f; done) >tmp-list
	(for f in $(sources_gnu_misc); do echo $(srcdir)/$$f; done) >>tmp-list
	(for f in $(sources_gnu_selected); do echo $(srcdir)/$$f; done) >>tmp-list
	(for f in $(sources_kawa_core); do echo $(srcdir)/$$f; done) >>tmp-list
	(for f in $(sources_kawa_selected); do echo $(srcdir)/$$f; done) >>tmp-list
	mv tmp-list $@

classes-rt.list: ./kawa-jar.stamp $(srcdir)/classes-to-skip-for-runtime 
	$(FIND) gnu kawa -name '*.class' -print | grep -v -f $(srcdir)/classes-to-skip-for-runtime >classes-rt.list

lib/kawa.jar: ./kawa-jar.stamp $(META_INF_FILES) classes.list
	mkdir -p lib
	$(JAR) cmf $(srcdir)/jar-manifest $@ $(META_INF_FILES) @classes.list

lib/kawart.jar: ./kawa-jar.stamp classes-rt.list
	mkdir -p lib
	$(JAR) cf $@ @classes-rt.list

$(META_ScriptEngineFactory): Makefile
	echo 'kawa.standard.SchemeScriptEngineFactory #Scheme' >tmp.ScriptEngineFactory
if ENABLE_XQUERY
	echo 'gnu.xquery.lang.XQueryScriptEngineFactory #XQuery' >>tmp.ScriptEngineFactory
endif
	mkdir -p META-INF/services
	mv tmp.ScriptEngineFactory $(META_ScriptEngineFactory)

kawa-doc.tar.gz: kawa-doc-$(VERSION).tar.gz

kawa-doc-$(VERSION).tar.gz:  doc/kawa.texi
	cd doc; $(MAKE) ../kawa-doc-$(VERSION).tar.gz

JLINE_VERSION_MAJOR=3

DEFAULT_JAVA_VERSION = 8

dist-kawa.jar: dist
	srcdir=`(cd $(srcdir); /bin/pwd)` \
	  && rm -rf tmpdir \
	  && mkdir -p tmpdir tmpdir/lib \
	  && cd tmpdir \
	  && tar xzf ../$(distdir).tar.gz \
	  && rm -f ./patch-source-list \
	  && ./kawa-$(VERSION)/configure --with-java-source=$(DEFAULT_JAVA_VERSION) \
	    --with-domterm="@WITH_DOMTERM_ARG@" \
	    --with-jline$(JLINE_VERSION_MAJOR)="@WITH_JLINE_PATH@" \
	    --with-servlet=@WITH_SERVLET_ARG@ --with-javafx=@WITH_JAVAFX_ARG@ \
	  && CLASSPATH=@conf_classpath@.$(CLASSPATH_SEPARATOR)$$CLASSPATH $(MAKE) all JAVAC="$(JAVAC) -source $(DEFAULT_JAVA_VERSION) -target $(DEFAULT_JAVA_VERSION)" \
	  && mv lib/kawa.jar ../dist-kawa.jar \
	  && cd .. && rm -rf tmpdir

ZIP_VERSION = $(VERSION)
kawa.zip: kawa-$(ZIP_VERSION).zip
kawa-snapshot.zip:
	$(MAKE) kawa.zip ZIP_VERSION=$(VERSION)-`date +%Y%m%d`
kawa-$(ZIP_VERSION).zip: dist-kawa.jar doc/kawa-manual.epub bin/kawa.bat
	mkdir -p kawa-$(VERSION) kawa-$(VERSION)/lib kawa-$(VERSION)/bin kawa-$(VERSION)/doc
	cd kawa-$(VERSION)/lib; \
	  $(LN_S) ../../dist-kawa.jar kawa.jar; \
	  $(LN_S) $(DOMTERM_JAR) domterm.jar; \
	  $(LN_S) @WITH_SERVLET_ARG@ servlet.jar; \
	  $(LN_S) @WITH_SERVLET_ARG@ servlet.jar; \
	  $(LN_S) @WITH_JLINE_PATH@ jline.jar
	docsrcdir="`cd $(srcdir); pwd`/doc"; \
	  cd kawa-$(VERSION)/doc; \
	  $(LN_S) ../../doc/kawa-manual.epub kawa-manual.epub; \
	  $(LN_S) $$docsrcdir/README-epub README-epub; \
	  $(LN_S) $$docsrcdir/browse-kawa-manual browse-kawa-manual; \
	  $(LN_S) $$docsrcdir/+default+ +default+
	cd kawa-$(VERSION)/bin; \
	  $(LN_S) ../../bin/@KAWA_SCRIPT@ kawa; \
	  $(LN_S) ../../bin/kawa.bat kawa.bat
	zip $@ -r kawa-$(VERSION)

upload-kawa.snapshot.zip:
	gnupload --replace --to ftp.gnu.org:kawa --symlink-regex kawa-$(VERSION)-`date +%Y%m%d`.zip

upload-kawa.release.zip:
	gnupload --to ftp.gnu.org:kawa --symlink-regex kawa-$(VERSION).tar.gz kawa-$(VERSION).zip

doc/kawa-manual.epub: doc/kawa.texi
	cd doc && $(MAKE) kawa-manual.epub

lib/jemacs.jar:
	srcdir=`(cd $(srcdir); /bin/pwd)` \
	  && rm -rf tmpdir \
	  && mkdir tmpdir \
	  && cd tmpdir \
	  && $$srcdir/configure --with-swing \
	  && $(MAKE) \
	  && $(JAR) cmf $$srcdir/gnu/jemacs/jar-manifest \
	      ../lib/jemacs.jar \
	      `$(FIND) . -name '*.class' -print` \
	  && cd .. && rm -rf tmpdir

lib/bytecode.jar: ./gnu/bytecode/java-classes.stamp
	$(JAR) cmf $(srcdir)/gnu/bytecode/jar-manifest \
	  lib/bytecode.jar \
	  `$(FIND) gnu/bytecode -name '*.class' -print` 

kawa-@VERSION@-src.zip: kawa-@VERSION@.tar.gz
	mkdir zip-temp
	cd zip-temp ;\
	  tar xzf ../kawa-@VERSION@.tar.gz; \
	  zip -q -r ../kawa-@VERSION@-src.zip kawa-@VERSION@
	rm -rf zip-temp

kawa-compiled.zip: kawa-@VERSION@-compiled.zip
kawa.jar: lib/kawa.jar
kawa-src.zip: kawa-@VERSION@-src.zip
jemacs.jar: lib/jemacs.jar
bytecode.jar: lib/bytecode.jar

EXTRA_DIST = debian/control debian/rules debian/changelog debian/dirs \
  build-aux/config.guess build-aux/config.sub build-aux/install-sh \
  patch-source-list selected-java-source \
  build.xml build.properties \
  $(MISC_EXTRA_DIST) \
  gnu/jemacs/jar-manifest gnu/jemacs/status.html \
  kawa/Version.java jar-manifest Make-rules classes-to-skip-for-runtime \
  bin/configure.ac bin/configure bin/Makefile.am bin/Makefile.in \
  bin/ChangeLog bin/kawa.bat.in bin/kawa.c bin/kawapath.c bin/cgi-servlet.c \
  bin/kawa.sh.in bin/qexo.in

MOSTLYCLEANFILES = tmp* *.o bin/*.o *.stamp */*.class */*/*.class */*/*/*.class classes.list classes-rt.list
CLEANFILES = *.jar lib/*.jar *.zip bin/qexo kawa/Version.java $(META_ScriptEngineFactory)
DISTCLEANFILES = selected-java-source bin/kawa.sh bin/kawa bin/kawa.bat
MAINTAINERCLEANFILES = \
  $(GENERATED_SIMPLEVECTOR_FILES) $(srcdir)/patch-source-list

HTML_HOME = $$HOME/public_html
JAVADOC_DIR = $$HOME/Kawa/api
JAVADOC_PACKAGES = \
  gnu.bytecode gnu.ecmascript gnu.expr gnu.mapping gnu.math gnu.text gnu.lists\
  gnu.xml gnu.kawa.reflect gnu.kawa.util gnu.kawa.lispexpr gnu.kawa.functions \
  gnu.kawa.servlet gnu.kawa.ant kawa kawa.lang kawa.standard kawa.lib \
  gnu.kawa.xml gnu.xquery.lang gnu.xquery.util gnu.brl gnu.kawa.brl \
  gnu.kawa.xslt gnu.kawa.sax gnu.commonlisp.lang gnu.q2.lang \
  gnu.kawa.models gnu.kawa.swingviews gnu.kawa.swtviews gnu.kawa.echo2 \
  gnu.jemacs.lang gnu.jemacs.buffer gnu.jemacs.swing gnu.jemacs.swt \
  gnu.kawa.android gnu.kawa.javafx

JAVADOC = javadoc
JAVADOC_FLAGS = -use
#JAVADOC_FLAGS = -J-Xmx60m

# NOTE:  To update javadoc:
#   make install-javadoc-html
# When satisfied:
#   make javadoc-cvs.log
# When satisfied:
#   cd ~/Kawa/api && cvs ci -m 'Re-run javadoc.'
javadoc-cvs.log:
	cd $(JAVADOC_DIR) \
	  && $(FIND) . -name CVS -prune -o -type f -exec echo rm '{}' ';'
	$(MAKE) install-javadoc-html >&LOG.JAVADOC
	(cd $(JAVADOC_DIR) && cvs -n update -d .>&/tmp/cvs-changes \
	  && (grep '^[?] ' /tmp/cvs-changes |sed -e 's|[?] ||') >/tmp/cvs-adds \
	  && (test '!' -s /tmp/cvs-adds || cvs add `cat /tmp/cvs-adds`) \
	  && (grep '^[U] ' /tmp/cvs-changes |sed -e 's|[U] ||') >/tmp/cvs-rems\
	  && (test '!' -s /tmp/cvs-rems || cvs remove `cat /tmp/cvs-rems`) \
	  && cvs update -d .) >cvs-update.log
	echo ExamineCheck ./cvs-update.log
	echo then: "(cd $(JAVADOC_DIR) && cvs ci -m 'Re-run javadoc.')".

ECLIPSE_PLUGINS_PATH = /opt/eclipse/plugins
JAVADOC_ANT_JARS = $(ECLIPSE_PLUGINS_PATH)/org.apache.ant_1.6.5/lib/ant.jar
JAVADOC_ECLIPSE_JARS = $(ECLIPSE_PLUGINS_PATH)/org.eclipse.jface_3.1.0.jar:$(ECLIPSE_PLUGINS_PATH)/org.eclipse.swt.gtk.linux.x86_3.1.0.jar
JAVADOC_EXTRA_PATH = $(JAVADOC_ECLIPSE_JARS):$(JAVADOC_ANT_JARS)

install-javadoc-html:
	-mkdir $(JAVADOC_DIR)
	-#rm `$(FIND) $(JAVADOC_DIR)/ -name CVS -prune -o '(' '!' -type d ')' -print`
	-rm `$(FIND) $(JAVADOC_DIR)/ -name '*.html'`
	CLASSPATH=$(JAVADOC_EXTRA_PATH)$(CLASSPATH_SEPARATOR)@conf_classpath@$$CLASSPATH \
	$(JAVADOC) $(JAVADOC_FLAGS) -sourcepath .:$(top_srcdir) -d $(JAVADOC_DIR) $(JAVADOC_PACKAGES)
	cp $(srcdir)/COPYING  $(JAVADOC_DIR)/COPYING

install-html: install-javadoc-html
	cd doc;  make install-html HTML_HOME="$(HTML_HOME)"

install-binSCRIPTS: $(bin_SCRIPTS)
	@$(NORMAL_INSTALL)
	test -z "$(bindir)" || $(mkdir_p) "$(DESTDIR)$(bindir)"
	$(INSTALL_SCRIPT) bin/qexo $(DESTDIR)$(bindir)/qexo
if ! ENABLE_KAWA_FRONTEND
	-rm -f $(DESTDIR)$(bindir)/kawa
	cd $(DESTDIR)$(bindir) && \
	  $(LN_S) $(datadir)/kawa/bin/kawa kawa
endif

install-data-am:  install-jar
	$(mkinstalldirs) $(DESTDIR)$(datadir)/kawa \
	  $(DESTDIR)$(datadir)/kawa/bin $(DESTDIR)$(datadir)/kawa/lib
	$(INSTALL_SCRIPT) bin/@KAWA_SCRIPT@ $(DESTDIR)$(datadir)/kawa/bin/kawa

install-jar:
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(datadir)/kawa \
	  $(DESTDIR)$(datadir)/kawa/bin $(DESTDIR)$(datadir)/kawa/lib
	$(INSTALL_DATA) lib/kawa.jar \
	  $(DESTDIR)$(datadir)/kawa/lib/$(KAWAJAR)
if WITH_DOMTERM
	-rm -f $(DESTDIR)$(datadir)/kawa/lib/domterm.jar
	cd $(DESTDIR)$(datadir)/kawa/lib && \
	  $(LN_S) $(DOMTERM_JAR) domterm.jar
endif
if ENABLE_SERVLET
	-rm -f $(DESTDIR)$(datadir)/kawa/lib/servlet.jar
	cd $(DESTDIR)$(datadir)/kawa/lib && \
	  $(LN_S) @WITH_SERVLET_ARG@ servlet.jar
endif
if WITH_JLINE3
	-rm -f $(DESTDIR)$(datadir)/kawa/lib/jline.jar
	cd $(DESTDIR)$(datadir)/kawa/lib && \
	  $(LN_S) @WITH_JLINE_PATH@ jline.jar
endif
if WITH_LSP4J
	-rm -f $(DESTDIR)$(datadir)/kawa/lib/lsp4j.jar
	cd $(DESTDIR)$(datadir)/kawa/lib && \
	  $(LN_S) @WITH_LSP4J_PATH@ lsp4j.jar
endif

install-bytecode-jar: lib/bytecode.jar
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(javadir)
	$(INSTALL_DATA) lib/bytecode.jar \
	  $(DESTDIR)$(javadir)/bytecode.jar

uninstall-local: uninstall-jar uninstall-exec-bindir uninstall-data
uninstall-jar:
	rm -f $(DESTDIR)$(datadir)/kawa/lib/$(KAWAJAR) \
	  $(DESTDIR)$(datadir)/kawa/lib/kawa.jar

uninstall-data:
	rm -f $(DESTDIR)$(datadir)/kawa/bin/kawa

uninstall-exec-bindir:
	rm -f $(DESTDIR)$(bindir)/kawa $(DESTDIR)$(bindir)/qexo

uninstall-bytecode-jar:
	rm -f $(DESTDIR)$(javadir)/bytecode.jar \
	  $(DESTDIR)$(javadir)/bytecode.jar

kawa/Version.java: $(srcdir)/kawa/Version.java.in $(srcdir)/configure.ac
	test -d $(srcdir)/.git \
	&& GIT_REVISION=`cd $(srcdir) && git describe --always --dirty --long --tags 2>/dev/null` \
	&& test -n "$$GIT_REVISION" \
	&& GIT_REVISION=" (git describe: $${GIT_REVISION})"; \
	sed -e 's|VERSION|@VERSION@'"$$GIT_REVISION|" <$(srcdir)/kawa/Version.java.in >$@

all-recursive:  kawa/Version.java selected-java-source $(GENERATED_SIMPLEVECTOR_FILES)
all: lib/kawa.jar lib/kawart.jar $(JLINE_LINK) $(DOMTERM_LINK) $(SERVLET_LINK) \
  $(GSON_LINK) $(LSP4J_LINK) $(LSP4J_JSONRPC_LINK)

if WITH_LSP4J
GSON_LINK = lib/gson.jar
LSP4J_LINK = lib/lsp4j.jar
LSP4J_JSONRPC_LINK = lib/lsp4j.jsonrpc.jar

lib/gson.jar:
	mkdir -p lib
	$(LN_S) @WITH_GSON_PATH@ lib/gson.jar
lib/lsp4j.jar:
	mkdir -p lib
	$(LN_S) @WITH_LSP4J_PATH@ lib/lsp4j.jar
lib/lsp4j.jsonrpc.jar:
	mkdir -p lib
	$(LN_S) @WITH_LSP4J_JSONRPC_PATH@ lib/lsp4j.jsonrpc.jar
endif
if WITH_JLINE3
JLINE_LINK = lib/jline.jar

lib/jline.jar:
	mkdir -p lib
	-rm -f lib/jline.jar
	$(LN_S) @WITH_JLINE_PATH@ lib/jline.jar
endif
if WITH_DOMTERM
DOMTERM_LINK = lib/domterm.jar
lib/domterm.jar:
	mkdir -p lib
	-rm -f lib/domterm.jar
	$(LN_S) $(DOMTERM_JAR) lib/domterm.jar
endif
if ENABLE_SERVLET
SERVLET_LINK = lib/servlet.jar
lib/servlet.jar:
	mkdir -p lib
	$(LN_S) @WITH_SERVLET_ARG@ lib/servlet.jar
endif

JAVA_SOURCE = @JAVA_SOURCE@

if WITH_ANDROID
PREPROCESS_ANDROID = +Android
else
PREPROCESS_ANDROID = -Android
endif
if WITH_HTTPSERVER
PREPROCESS_HTTPSERVER = +use:com.sun.net.httpserver
else
PREPROCESS_HTTPSERVER = -use:com.sun.net.httpserver
endif
if WITH_JLINE3
PREPROCESS_JLINE3 = +with:jline3
else
PREPROCESS_JLINE3 = -with:jline3
endif
if WITH_LSP4J
PREPROCESS_LSP4J = +with:lsp4j
else
PREPROCESS_LSP4J = -with:lsp4j
endif
if ENABLE_XML
PREPROCESS_XML = +enable:XML
else
PREPROCESS_XML = -enable:XML
endif
if WITH_AWT
PREPROCESS_AWT = +use:java.awt
else
PREPROCESS_AWT = -use:java.awt
endif

$(srcdir)/patch-source-list: $(top_srcdir)/configure
	(cd $(srcdir); \
	$(FIND) . -name '*.java' -exec grep -lE ' #endif ' '{}' ';') \
	  | sed -e 's|./||' | sort > $@

kawa/Version.class: kawa/Version.java
	$(JAVAC) -d . $(JAVACFLAGS) $^

gnu/kawa/util/PreProcess.class: gnu/kawa/util/PreProcess.java
	$(JAVAC) -d . $(JAVACFLAGS) $^

JAVA_PREPROCESS_OPTIONS = @JAVA_SOURCE_SELECTOR@ $(PREPROCESS_HTTPSERVER) $(PREPROCESS_XML) $(PREPROCESS_JLINE3) $(PREPROCESS_LSP4J) $(PREPROCESS_AWT) $(PREPROCESS_ANDROID)

selected-java-source: Makefile $(srcdir)/patch-source-list
	if test ! -e selected-java-source -o \
	   "$(JAVA_PREPROCESS_OPTIONS)" != "`cat selected-java-source`"; \
	   then $(MAKE) do-preprocess; \
	fi
	echo "$(JAVA_PREPROCESS_OPTIONS)" >selected-java-source

do-preprocess: gnu/kawa/util/PreProcess.class
	CLASSPATH=".$(CLASSPATH_SEPARATOR)$$CLASSPATH" $(JAVA) gnu.kawa.util.PreProcess \
	  $(JAVA_PREPROCESS_OPTIONS) \
	  `sed -e 's|\([^ ]*\)|$(srcdir)/\1|' < $(srcdir)/patch-source-list`

GENERATED_SIMPLEVECTOR_FILES =\
  $(srcdir)/gnu/lists/FVector.java \
  $(srcdir)/gnu/lists/BitVector.java \
  $(srcdir)/gnu/lists/ByteVector.java \
  $(srcdir)/gnu/lists/ShortVector.java \
  $(srcdir)/gnu/lists/IntVector.java \
  $(srcdir)/gnu/lists/LongVector.java \
  $(srcdir)/gnu/lists/F32Vector.java \
  $(srcdir)/gnu/lists/F64Vector.java \
  $(srcdir)/gnu/lists/S8Vector.java \
  $(srcdir)/gnu/lists/S16Vector.java \
  $(srcdir)/gnu/lists/S32Vector.java \
  $(srcdir)/gnu/lists/S64Vector.java \
  $(srcdir)/gnu/lists/U8Vector.java \
  $(srcdir)/gnu/lists/U16Vector.java \
  $(srcdir)/gnu/lists/U32Vector.java \
  $(srcdir)/gnu/lists/U64Vector.java

.PHONY: make-primvector-source
make-primvector-source: $(srcdir)/gnu/lists/PrimVector.template gnu/kawa/util/PreProcess.class
	CLASSPATH=".$(CLASSPATH_SEPARATOR)$$CLASSPATH" $(JAVA) gnu.kawa.util.PreProcess \
	  '%$(TAG)' '%UniformVector' -o $(OUT) $<

$(srcdir)/gnu/lists/FVector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=OBJECT
$(srcdir)/gnu/lists/BitVector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=BIT
$(srcdir)/gnu/lists/ByteVector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=BYTE
$(srcdir)/gnu/lists/ShortVector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=SHORT
$(srcdir)/gnu/lists/IntVector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=INT
$(srcdir)/gnu/lists/LongVector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=LONG
$(srcdir)/gnu/lists/F32Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=F32
$(srcdir)/gnu/lists/F64Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=F64
$(srcdir)/gnu/lists/S8Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=S8
$(srcdir)/gnu/lists/S16Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=S16
$(srcdir)/gnu/lists/S32Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=S32
$(srcdir)/gnu/lists/S64Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=S64
$(srcdir)/gnu/lists/U8Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=U8
$(srcdir)/gnu/lists/U16Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=U16
$(srcdir)/gnu/lists/U32Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=U32
$(srcdir)/gnu/lists/U64Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=U64

clean-generated-simplevector-files:
	-rm -f $(GENERATED_SIMPLEVECTOR_FILES)

if ON_WINDOWS
# kawa.sh is still needed for 'make check', even on Windows.
KAWASCRIPTS_KAWA = bin/kawa.bat bin/kawa.sh
else
KAWASCRIPTS_KAWA = bin/@KAWA_SCRIPT@
endif
noinst_SCRIPTS = bin/@KAWA_SCRIPT@
bin_SCRIPTS = bin/qexo $(KAWASCRIPTS_KAWA)

#bin/kawa.bat: $(srcdir)/bin/kawa.bat.in
#	sed -e 's|@VERSION@|$(VERSION)|' \
#	    -e 's|@DEFAULT_EXTRA_PATH@|'`echo '$(conf_classpath)'| sed -e 's|[\]|\\\\|g' -e 's|\&|\\&|g'`'|' \
#	  <$(srcdir)/bin/kawa.bat.in > bin/kawa.bat

STANDARD_NAMED_CHARS_URL = http://www.w3.org/2003/entities/2007/w3centities-f.ent

$(srcdir)/gnu/text/StandardNamedChars.java:
	wget -nv -O - $(STANDARD_NAMED_CHARS_URL) \
	| sed -n -e 's|&#38;|\&|' \
	      -e '/ENTITY/s/^.*ENTITY *\([^ ]*\) *"\([^"]*\)" *>\(.*\)$$/    t.put("\1"\2);\3/p' \
	| sed -e 's|<!--\(.*\)-->|  // \1|' \
	      -e 's|&#x\([0-9a-fA-F]*\);|, 0x\1|g' \
	      -e 's|&#0*\([0-9a-fA-F]*\);|, \1|g' \
	      -e '1i\' \
	      -e '// DO NOT EDIT: This file is automatically generated by gnu/text/Makefile.\' \
	      -e 'package gnu.text;\' \
	      -e 'public class StandardNamedChars {\' \
	      -e '  public static final NamedCharTable instance = new NamedCharTable();\' \
	      -e '  static { NamedCharTable t = instance;' \
	      -e '$$a\' \
	      -e '}}' \
	> $@

distall:  distcheck kawa.zip kawa-doc-$(VERSION).tar.gz
# Consider also:
# kawa-@VERSION@-src.zip jemacs-@JEMACS_VERSION@.jar bytecode-@BYTECODE_VERSION@.jar
