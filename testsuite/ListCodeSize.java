import java.io.*;

import gnu.bytecode.*;
import org.objectweb.asm.*;
import org.objectweb.asm.commons.CodeSizeEvaluator;

/**
 * Application that lists the number of bytes in named methods. Useful for
 * regression testing of code generation and inlining.
 */

public class ListCodeSize
{
    public static void usage()
    {
        System.err.println("Usage: class methodname ...");
        System.exit(-1);
    }

    static void print(java.util.Map.Entry<String, Integer> entry, String cname)
    {
        System.out.print(cname + ".");
        System.out.print(entry.getKey());
        int size = entry.getValue();
        if (size == 0)
            System.out.print(": no code");
        else
        {
            System.out.print(": ");
            System.out.print(size);
            System.out.print(" bytes");
        }
        System.out.println();
    }

    public static final void main(String[] args)
    {
        if (args.length == 0)
            usage();
        String filename = args[0];
        try
        {
            java.io.InputStream inp = new FileInputStream(filename);

            java.util.Map<String, Integer> sizes = new java.util.LinkedHashMap<String, Integer>();
            ClassType[] ctype = new ClassType[1];
            new ClassReader(inp).accept(new ClassVisitor(Opcodes.ASM5) {
                @Override
                public void visit(int version, int access, String name, String signature,
                    String superName, String[] interfaces)
                {
                    ctype[0] = new ClassType(name);
                    ctype[0].setExisting(true);
                    super.visit(version, access, name, signature, superName, interfaces);
                }

                @Override
                public MethodVisitor visitMethod(int access, String name, String desc,
                    String signature, String[] exceptions)
                {
                    CodeSizeEvaluator[] cse = new CodeSizeEvaluator[1];
                    cse[0] = new CodeSizeEvaluator(new MethodVisitor(Opcodes.ASM5) {
                        @Override
                        public void visitEnd()
                        {
                            Method method = ctype[0].addMethod(name, desc, 0);
                            StringBuffer sbuf = new StringBuffer();
                            sbuf.append(method.getName());
                            method.listParameters(sbuf);
                            sbuf.append(method.getReturnType().getName());
                            sizes.put(sbuf.toString(), cse[0].getMinSize());
                            super.visitEnd();
                        }
                    });
                    return cse[0];
                }
            }, ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES);

            String cname = ctype[0].getName();
            if (args.length == 1)
            {
                for (java.util.Map.Entry<String, Integer> entry : sizes.entrySet())
                {
                    print(entry, cname);
                }
            } else
            {
                for (int i = 1; i < args.length; i++)
                {
                    for (java.util.Map.Entry<String, Integer> entry : sizes.entrySet())
                    {
                        String xname = entry.getKey();
                        if (xname.startsWith(args[i])
                            && (xname.indexOf("$check(") < 0 || args[i].endsWith("$check")))
                            print(entry, cname);
                    }
                }
            }

        } catch (java.io.FileNotFoundException e)
        {
            System.err.println("File " + filename + " not found");
            System.exit(-1);
        } catch (java.io.IOException e)
        {
            System.err.println(e);
            e.printStackTrace();
            System.exit(-1);
        }
    }
}
