package kawa.standard;
import kawa.lang.*;
import kawa.Version;
import gnu.expr.*;
import gnu.lists.ImmutablePair;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Symbol;
import gnu.mapping.SimpleSymbol;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.nio.ByteOrder;

/** Implements the Scheme 'cond-expand' syntax.
 * Also provides various static methods relating to "features".
 */

public class IfFeature extends Syntax {

    public static final IfFeature condExpand = new IfFeature();
    static { condExpand.setName("cond-expand"); }

    @Override
    public void scanForm(Pair st, ScopeExp defs, Translator tr) {
        Object forms = evaluate(st.getCdr(), tr);
        tr.scanBody(forms, defs, false);
    }

    public Expression rewriteForm(Pair form, Translator tr) {
        Object forms = evaluate(form.getCdr(), tr);
        return tr.rewrite_body(forms);
    }

    public boolean evaluateConditionCar(Pair pair, Translator tr) {
        Object save = tr.pushPositionOf(pair);
        boolean r = evaluateCondition(pair.getCar(), tr);
        tr.popPositionOf(save);
        return r;
    }

    public boolean evaluateCondition(Object form, Translator tr) {
        form = tr.namespaceResolve(Translator.stripSyntax(form));
        if (form instanceof String || form instanceof SimpleSymbol)
            return hasFeature(form.toString());
        if (form instanceof Pair) {
            Pair pair = (Pair) form;
            Object keyword = Translator.stripSyntax(pair.getCar());
            if (keyword == orSymbol || keyword == andSymbol) {
                Object rest = pair.getCdr();
                while (rest instanceof Pair) {
                    pair = (Pair) rest;
                    boolean val = evaluateConditionCar(pair, tr);
                    if (val == (keyword == orSymbol))
                        return val;
                    rest = pair.getCdr();
                }
                tr.errorIfNonEmpty(rest);
                return keyword == andSymbol;
            }
            if (keyword == notSymbol) {
                Object rest = pair.getCdr();
                if (rest instanceof Pair) {
                    Pair pair2 = (Pair) rest;
                    if (pair2.getCdr() == LList.Empty)
                        return ! evaluateConditionCar(pair2, tr);
                }
                tr.errorWithPosition("'not' must be followed by a single condition", pair);
                return false;
            }
            if (keyword == librarySymbol) {
                Object rest = pair.getCdr();
                if (rest instanceof Pair) {
                    Pair pair2 = (Pair) rest;
                    if (pair2.getCdr() == LList.Empty)
                        return ImportFromLibrary.instance
                            .libraryExists(pair2.getCar(), tr);
                }
                tr.errorWithPosition("'library' must be followed by <library name>", pair);
                return false;
            }
        }
        tr.error('e', "unrecognized cond-expand expression");
        return false;
    }

    public Object evaluate(Object clauses, Translator tr) {
        while (clauses instanceof Pair) {
            Pair pclauses = (Pair) clauses;
            Object clause = pclauses.getCar();
            clauses = pclauses.getCdr();
            if (! (clause instanceof Pair))
                tr.errorWithPosition("cond-expand clauses is not a list",
                                     pclauses);
            Pair pclause = (Pair) clause;
            Object test = Translator.stripSyntax(pclause.getCar());
            if ((test == elseSymbol && clauses == LList.Empty)
                || evaluateConditionCar(pclause, tr))
                return pclause.getCdr();
        }
        tr.errorIfNonEmpty(clauses);
        return LList.Empty;
    }

    private static List<String> coreFeatures = new ArrayList<String>();
    private static void addCoreFeature(String feature) {
        coreFeatures.add(feature.intern());
    }
    static {
        addCoreFeature("kawa");
        addCoreFeature("kawa-"+Version.getVersion());

        addCoreFeature("complex");
        addCoreFeature("exact-complex");
        addCoreFeature("exact-closed");
        addCoreFeature("ieee-float");
        addCoreFeature("ratios");
        addCoreFeature("full-unicode");

        String javaVersion = System.getProperty("java.version");
        if (javaVersion != null && javaVersion.length() >= 1) {
            if (javaVersion.length() >= 3
                && javaVersion.charAt(0) == '1'
                && javaVersion.charAt(1) == '.')
                javaVersion = javaVersion.substring(2);
            int dot = javaVersion.indexOf('.');
            if (dot >= 0)
                javaVersion = javaVersion.substring(0, dot);
            int version;
            int minVersion = 6;
            try {
                version = Integer.parseInt(javaVersion);
                if (version > 20) { // sanity check
                    addCoreFeature("java-"+version);
                    version = 11;
                }
            } catch (Throwable ex) {
                version = 0;
            }
            for (int i = minVersion; i <= version; i++) {
                addCoreFeature("java-"+i);
            }
        }

        if (ByteOrder.nativeOrder() == ByteOrder.BIG_ENDIAN)
            addCoreFeature("big-endian");
        else
            addCoreFeature("little-endian");

        String osName =
            System.getProperty("os.name").toLowerCase(Locale.ENGLISH);
        // FIXME check for cygwin, bsd
        if (osName.indexOf("linux") >= 0) {
            addCoreFeature("posix");
            addCoreFeature("unix");
            addCoreFeature("linux");
            addCoreFeature("gnu-linux");
        }
        else if (osName.indexOf("win") >= 0) {
            addCoreFeature("windows");
        } else if (osName.indexOf("sunos") >= 0
                   || osName.indexOf("solaris") >= 0) {
            addCoreFeature("posix");
            addCoreFeature("unix");
            addCoreFeature("solaris");
        } else if (osName.indexOf("mac") >= 0
                   || osName.indexOf("darwin") >= 0) {
            addCoreFeature("posix");
            addCoreFeature("unix");
            addCoreFeature("darwin");
            addCoreFeature("macosx");
        } else if (osName.indexOf("bsd") >= 0) {
            addCoreFeature("bsd");
            addCoreFeature("posix");
            addCoreFeature("unix");
        } else if (osName.indexOf("nix") >= 0
                   || osName.indexOf("nux") >= 0
                   || osName.indexOf("aix") > 0) {
            addCoreFeature("posix");
            addCoreFeature("unix");
        }

        String archName =
            System.getProperty("os.arch").toLowerCase(Locale.ENGLISH);;
        if (archName.indexOf("amd64") >= 0
            || archName.indexOf("x86_64") >= 0) {
            addCoreFeature("x86-64");
        } else if (archName.indexOf("x86") >= 0
                   || archName.indexOf("i386") >= 0) {
            addCoreFeature("i386");
        } else if (archName.indexOf("ppc") >= 0
                   || archName.indexOf("powerpc") >= 0) {
            addCoreFeature("ppc");
        } else if (archName.indexOf("sparc") >= 0) {
            addCoreFeature("sparc");
        }
        addCoreFeature("jvm");

        addCoreFeature("r7rs");

        addCoreFeature("srfi-0"); // cond-expand
        // addCoreFeature("srfi-1"); // lists - only if require used.
        //if (name == "srfi-1") return true; // lists
        addCoreFeature("srfi-4"); // Homogeneous numeric vector datatypes
        addCoreFeature("srfi-6"); // Basic String Ports
        addCoreFeature("srfi-8"); // receive: Binding to multiple values
        addCoreFeature("srfi-9"); // Defining Record Types
        addCoreFeature("srfi-11"); // let-values, let*-values
        addCoreFeature("srfi-16"); // case-lambda
        addCoreFeature("srfi-17"); // Generalized set!
        addCoreFeature("srfi-23"); // Error reporting mechanism
        addCoreFeature("srfi-25"); // Multi-dimensional Array Primitives
        addCoreFeature("srfi-26"); // Notation for Specializing Parameters
        addCoreFeature("srfi-28"); // Basic Format Strings
        addCoreFeature("srfi-30"); // Nested Multi-line Comments.
        addCoreFeature("srfi-39"); // Parameter objects

        /* #ifdef use:java.text.Normalizer */
        /* #ifdef JAVA6COMPAT5 */
        // try {
        //     Class.forName("java.text.Normalizer");
        //     addCoreFeature("string-normalize-unicode");
        // }
        // catch (ClassNotFoundException ex) {
        // }
        /* #else */
        addCoreFeature("string-normalize-unicode");
        /* #endif */
        /* #endif */

        addCoreFeature("threads");
    }

    /** Check if we implement a named feature.
     * @param name an interned feature name
     */
    public static boolean hasFeature (String name) {
        for (int i = coreFeatures.size();  --i>= 0; ) {
            if (name == coreFeatures.get(i))
                return true;
        }
        if (name == "in-http-server" || name == "in-servlet") {
            int mflags = ModuleContext.getContext().getFlags();
            if (name == "in-http-server")
                return (mflags & ModuleContext.IN_HTTP_SERVER) != 0;
            if (name == "in-servlet")
                return (mflags & ModuleContext.IN_SERVLET) != 0
                    || Compilation.getCurrent().generatingServlet();
        }
    
        String classExistsPrefix = "class-exists:";
        if (name.startsWith(classExistsPrefix)) {
            name = name.substring(classExistsPrefix.length());
            try {
                Class.forName(name, false, IfFeature.class.getClassLoader());
                return true;
            } catch (ClassNotFoundException ex) {
                return false;
            }
        }

        Symbol provide_symbol = Symbol.valueOf(PROVIDE_PREFIX+name);
        Declaration decl = Compilation.getCurrent().lookup(provide_symbol, -1);
        if (decl!=null && ! decl.getFlag(Declaration.IS_UNKNOWN))
            return true;
        return false;
    }

    /** Return a (partial) list of features,
     * The result does not include "provide" feature names - though it should.
     * Feature names of the form class:CLASSNAME are not returned.
     */
    public static LList featureList() {
        LList result = LList.Empty;
        for (int i = coreFeatures.size();  --i>= 0; ) {
            String item = coreFeatures.get(i);
            result = new ImmutablePair(Symbol.valueOf(item), result);
        }
        return result;
    }

    public static final String PROVIDE_PREFIX = "%provide%";

    public static boolean isProvide(Declaration decl) {
        String name = decl.getName();
        return name == null ? false : name.startsWith(PROVIDE_PREFIX);
    }

    public static final SimpleSymbol andSymbol = Symbol.valueOf("and");
    public static final SimpleSymbol elseSymbol = Symbol.valueOf("else");
    public static final SimpleSymbol librarySymbol = Symbol.valueOf("library");
    public static final SimpleSymbol notSymbol = Symbol.valueOf("not");
    public static final SimpleSymbol orSymbol = Symbol.valueOf("or");
}
