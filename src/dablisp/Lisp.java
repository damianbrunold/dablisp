/*  dablisp - a kind of common lisp subset written in java
    Copyright 2008 Damian Brunold

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
package dablisp;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import dablisp.fn.*;

public class Lisp {

    public static Set<String> traces = new HashSet<String>();
    public static String tracelevel = "";

    private static boolean noninteractive = false;
    private static boolean runtests = false;
    private static List<File> loaddirs = new ArrayList<File>();
    private static List<String> loadfiles = new ArrayList<String>();

    public static VM vm = new VM();

    public static void main(String[] args) {
        for (String arg : args) {
            if (arg.equals("--test") || arg.equals("-t")) {
                runtests = true;
            } else if (arg.startsWith("--loaddir=")) {
                loaddirs.add(new File(arg.substring("--loaddir=".length())));
            } else if (arg.startsWith("--loadfile=")) {
                loadfiles.add(arg.substring("--loadfile=".length()));
            } else if (arg.equals("--noninteractive")) {
                noninteractive = true;
            }
        }
        if (runtests) {
            try {
                loadFile("!test.lisp", true);
            } catch (LispException e) {
                System.err.println(e);
                System.exit(1);
            }
        }
        if (!loaddirs.isEmpty()) {
            for (File loaddir : loaddirs) {
                loadDir(loaddir);
            }
        }
        if (!loadfiles.isEmpty()) {
            for (String loadfile : loadfiles) {
                loadFile(loadfile, false);
            }
        }
        if (!noninteractive) {
            repl(getStandardInput().getIn());
        }
    }

    static {
        init();
    }

    private static void init() {
        initEnv();
        initDenv();
        initFenv();
        initLisp();
    }

    private static List<Obj> readExprs(ReadStream stream) {
        try {
            prompt();
            StringBuilder b = new StringBuilder();
            String line = stream.readLine();
            while (line != null) {
                b.append(line);
                b.append('\n');
                try {
                    List<Obj> exprs = new ArrayList<Obj>();
                    Lexer lexer = new Lexer(new ReadStream(b.toString()));
                    Obj expr = Parser.parse(lexer);
                    while (expr != null) {
                        exprs.add(expr);
                        expr = Parser.parse(lexer);
                    }
                    return exprs;
                } catch (LispException e) {
                    // not yet complete... wait for more...
                }
                line = stream.readLine();
            }
            return new ArrayList<Obj>();
        } catch (IOException e) {
            throw new LispException(Symbol.GENERALCONDITION, "cannot read input", e);
        }
    }

    private static Obj repl(ReadStream stream) {
        Obj result = Symbol.NIL;
        while (true) {
            try {
                for (Obj expr : readExprs(stream)) {
                    expr = MacroExpander.expand(expr, vm);
                    tracelevel = "";
                    CompileEnv env = CompileEnv.createRootEnv(vm.denv.symbols(), vm.fenv.symbols());
                    CompiledCode code = Compiler.compile(expr, vm, env);
                    if (isFlagSet("*show-bytecode*")) System.out.println(code);
                    result = vm.run(code);
                }
                if (result.isMvals()) {
                    for (int i = 0; i < result.asMvals().size(); i++) {
                        getStandardOutput().print(result.asMvals().val(i));
                    }
                } else {
                    getStandardOutput().print(result);
                }
            } catch (QuitLispException e) {
                break;
            } catch (LispException e) {
                Strm err = getErrorOutput();
                err.print(e.tag.displayValue() + ": " + e.value.displayValue());
                err.print();
                e.printBacktrace(err.getOut());
                err.flush();
                //e.printStackTrace();
            }
        }
        return result;
    }

    public static boolean isFlagSet(String flag) {
        return vm.denv.get(Symbol.intern(flag)) != Symbol.NIL;
    }

    public static synchronized Obj eval(String code) throws LispException {
        return eval(new ReadStream(code));
    }

    public static synchronized Obj eval(ReadStream in) throws LispException {
        Lexer lexer = new Lexer(in);
        Obj result = Symbol.NIL;
        Obj expr = Parser.parse(lexer);
        while (expr != null) {
            expr = MacroExpander.expand(expr, vm);
            CompileEnv env = CompileEnv.createRootEnv(vm.denv.symbols(), vm.fenv.symbols());
            CompiledCode code = Compiler.compile(expr, vm, env);
            if (isFlagSet("*show-bytecode*")) System.out.println(code);
            result = vm.run(code);
            expr = Parser.parse(lexer);
        }
        return result;
    }

    private static void prompt() {
        Strm out = getStandardOutput();
        out.print();
        out.print("> ");
        out.flush();
    }

    private static void initEnv() {
        // empty global lex env
    }

    private static void initDenv() {
        ReadStream in = new ReadStream(System.in);
        PrintStream out = System.out;
        PrintStream err = System.err;
        bindSpecial(Symbol.intern("*show-bytecode*"), Symbol.NIL);
        bindSpecial(Symbol.intern("*vm-trace*"), Symbol.NIL);
        bindSpecial(Symbol.intern("*standard-input*"), new Strm(in));
        bindSpecial(Symbol.intern("*standard-output*"), new Strm(out));
        bindSpecial(Symbol.intern("*trace-output*"), new Strm(out));
        bindSpecial(Symbol.intern("*error-output*"), new Strm(err));
        bindSpecial(Symbol.intern("*debug-io*"), new Strm(in, out));
        bindSpecial(Symbol.intern("*query-io*"), new Strm(in, out));
        bindSpecial(Symbol.intern("*terminal-io*"), new Strm(in, out));
    }

    public static void bindSpecial(Symbol sym, Obj obj) {
        vm.denv.bind(sym, obj);
    }

    public static void mark() {
        vm.denv.mark();
    }

    public static void restore() {
        vm.denv.restore();
    }

    public static void bindConstant(Symbol sym, Obj obj) {
        vm.denv.bind(sym, obj);
        vm.constants.add(sym);
    }

    private static void initFenv() {
        addFunction(new FnQuit());
        addFunction(new FnPlus());
        addFunction(new FnMinus());
        addFunction(new FnMult());
        addFunction(new FnDiv());
        addFunction(new FnNumeq());
        addFunction(new FnNumnoteq());
        addFunction(new FnNumless());
        addFunction(new FnNumlesseq());
        addFunction(new FnNumgreater());
        addFunction(new FnNumgreatereq());
        addFunction(new FnEq());
        addFunction(new FnEql());
        addFunction(new FnCar());
        addFunction(new FnCdr());
        addFunction(new FnCons());
        addFunction(new FnList());
        addFunction(new FnRead());
        addFunction(new FnEval());
        addFunction(new FnPrint());
        addFunction(new FnPrinc());
        addFunction(new FnPrin1());
        addFunction(new FnLoad());
        addFunction(new FnApply());
        addFunction(new FnFuncall());
        addFunction(new FnGensym());
        addFunction(new FnMacroExpand());
        addFunction(new FnMacroExpand1());
        addFunction(new FnDisassemble());
        addFunction(new FnRplaca());
        addFunction(new FnRplacd());
        addFunction(new FnTypep());
        addFunction(new FnFormat());
        addFunction(new FnDescribe());
        addFunction(new FnCharpred("="));
        addFunction(new FnCharpred("/="));
        addFunction(new FnCharpred("<"));
        addFunction(new FnCharpred("<="));
        addFunction(new FnCharpred(">"));
        addFunction(new FnCharpred(">="));
        addFunction(new FnCharpred("equal"));
        addFunction(new FnCharpred("not-equal"));
        addFunction(new FnCharpred("lessp"));
        addFunction(new FnCharpred("greaterp"));
        addFunction(new FnCharpred("not-greaterp"));
        addFunction(new FnCharpred("not-lessp"));
        addFunction(new FnCharcode());
        addFunction(new FnCodechar());
        addFunction(new FnCharacter());
        addFunction(new FnBoundp());
        addFunction(new FnFboundp());
        addFunction(new FnMod());
        addFunction(new FnRem());
        addFunction(new FnSetchar());
        addFunction(new FnFloat());
        addFunction(new FnFloor());
        addFunction(new FnCeiling());
        addFunction(new FnTruncate());
        addFunction(new FnRound());
        addFunction(new FnLogop("ior"));
        addFunction(new FnLogop("xor"));
        addFunction(new FnLogop("and"));
        addFunction(new FnLognot());
        addFunction(new FnTerpri());
        addFunction(new FnReadchar());
        addFunction(new FnMakestringinputstream());
        addFunction(new FnClose());
        addFunction(new FnNativeNull());
        addFunction(new FnGetInternalRealTime());
        addFunction(new FnLispImplementationVersion());
        addFunction(new FnInvoke());
        addFunction(new FnNew());
        addFunction(new FnCoerceFromNative());
        addFunction(new FnCoerceToNative());
        addFunction(new FnTypeOf());
        addFunction(new FnIntern());
        addFunction(new FnMakeSymbol());
        addFunction(new FnSymbolName());
        addFunction(new FnTraces());
        addFunction(new FnRoom());
        addFunction(new FnSymbolValue());
        addFunction(new FnApropos());
        addFunction(new FnAproposList());
        addFunction(new FnSxhash());
        addFunction(new FnMakeHashTable());
        addFunction(new FnGetHash());
        addFunction(new FnSetHash());
        addFunction(new FnRemHash());
        addFunction(new FnStringEndsWith());
        addFunction(new FnStringStartsWith());
        addFunction(new FnMakeStringOutputStream());
        addFunction(new FnMakeFillPointerOutputStream());
        addFunction(new FnGetOutputStreamString());
        addFunction(new FnValues());
        addFunction(new FnAref());
        addFunction(new FnAset());
        addFunction(new FnVector());
        addFunction(new FnArrayRank());
        addFunction(new FnArrayDimension());
        addFunction(new FnMakeArray());
        addFunction(new FnArrayElementType());
        addFunction(new FnFillPointer());
        addFunction(new FnSetFillPointer());
        addFunction(new FnArrayHasFillPointerP());
    }

    public static synchronized void addFunction(Fn fn) {
        vm.rootFEnv.bind(Symbol.intern(fn.getName()), fn);
    }

    private static void initLisp() {
        loadFile("!library.lisp", true);
    }

    public static Strm getStandardOutput() {
        return vm.denv.get(Symbol.intern("*standard-output*")).asStrm();
    }

    public static Strm getErrorOutput() {
        return vm.denv.get(Symbol.intern("*error-output*")).asStrm();
    }

    public static Strm getStandardInput() {
        return vm.denv.get(Symbol.intern("*standard-input*")).asStrm();
    }

    public static Strm getTraceOutput() {
        return vm.denv.get(Symbol.intern("*trace-output*")).asStrm();
    }

    public static Strm getDebugIO() {
        return vm.denv.get(Symbol.intern("*debug-io*")).asStrm();
    }

    public static Strm getQueryIO() {
        return vm.denv.get(Symbol.intern("*query-io*")).asStrm();
    }

    public static Strm getTerminalIO() {
        return vm.denv.get(Symbol.intern("*terminal-io*")).asStrm();
    }

    public static void setStandardOutput(Strm strm) {
        vm.denv.set(Symbol.intern("*standard-output*"), strm);
    }

    public static void setErrorOutput(Strm strm) {
        vm.denv.set(Symbol.intern("*error-output*"), strm);
    }

    public static void setTraceOutput(Strm strm) {
        vm.denv.set(Symbol.intern("*trace-output*"), strm);
    }

    public static synchronized Obj loadFile(String fn, boolean failonerror) throws LispException {
        try {
            tracelevel = "";
            Obj loadfile = Parser.parse(new Lexer(new ReadStream("(load \"" + fn.replace("\\", "\\\\") + "\")")));
            CompileEnv env = CompileEnv.createRootEnv(vm.denv.symbols(), vm.fenv.symbols());
            CompiledCode code = Compiler.compile(loadfile, vm, env);
            return vm.run(code);
        } catch (LispException e) {
            if (failonerror) {
                throw e;
            } else {
                getErrorOutput().print(e.tag.displayValue() + ": " + e.value.displayValue());
                getErrorOutput().print();
                e.printBacktrace(getErrorOutput().getOut());
                e.printStackTrace(getErrorOutput().getOut());
                getErrorOutput().flush();
                return Symbol.NIL;
            }
        } catch (Exception e) {
            if (failonerror) {
                throw new LispException(Symbol.GENERALCONDITION, "caught exception while loading file", e);
            } else {
                e.printStackTrace(getErrorOutput().getOut());
                getErrorOutput().flush();
                return Symbol.NIL;
            }
        }
    }

    public static synchronized Obj loadDir(File dir) {
        Obj result = Symbol.NIL;
        File[] files = dir.listFiles(new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return name.endsWith(".lisp");
            }
        });
        if (files != null) {
            for (File file : files) {
                result = loadFile(file.getPath(), false);
            }
        }
        return result;
    }

    private static Obj getArgs(int argc, Stack stack) {
        Cons sentinel = new Cons(Symbol.NIL, Symbol.NIL);
        Cons c = sentinel;
        for (int i = 0; i < argc; i++) {
            Cons d = new Cons(stack.peek(i), Symbol.NIL);
            c.setCdr(d);
            c = d;
        }
        return sentinel.getCdr();
    }

    public static void enterTrace(Fn fn, int argc, Stack stack) {
        if (fn != null && traces.contains(fn.getName())) {
            Obj params = getArgs(argc, stack);
            getTraceOutput().print(tracelevel + "enter " + new Cons(Symbol.intern(fn.getName()), params));
            tracelevel = tracelevel + " ";
        }
    }

    public static void enterTrace(Fn fn, Stack stack) {
        if (fn != null && traces.contains(fn.getName())) {
            int argc = stack.popInt();
            Obj params = getArgs(argc, stack);
            stack.push(IntNum.fromLong(argc));
            getTraceOutput().print(tracelevel + "enter " + new Cons(Symbol.intern(fn.getName()), params));
            tracelevel = tracelevel + " ";
        }
    }

    public static void exitTrace(Fn fn, Stack stack) {
        if (fn != null && traces.contains(fn.getName())) {
            tracelevel = tracelevel.substring(0, tracelevel.length() - 1);
            getTraceOutput().print(tracelevel + "exit " + fn.getName() + " => " + stack.peek());
        }
    }

}
