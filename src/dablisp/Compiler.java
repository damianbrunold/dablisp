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

import java.util.*;

import dablisp.specialforms.*;

public class Compiler {

    private static Map<Symbol, SpecialForm> specialForms = new HashMap<Symbol, SpecialForm>();

    static {
        specialForms.put(Symbol.IF, new SpecialFormIf());
        specialForms.put(Symbol.QUOTE, new SpecialFormQuote());
        specialForms.put(Symbol.PROGN, new SpecialFormProgn());
        specialForms.put(Symbol.LET, new SpecialFormLet());
        specialForms.put(Symbol.LABELS, new SpecialFormLabels());
        specialForms.put(Symbol.FLET, new SpecialFormFlet());
        specialForms.put(Symbol.LETSTAR, new SpecialFormLetstar());
        specialForms.put(Symbol.FUNCTION, new SpecialFormFunction());
        specialForms.put(Symbol.SETQ, new SpecialFormSetq());
        specialForms.put(Symbol.THROW, new SpecialFormThrow());
        specialForms.put(Symbol.CATCH, new SpecialFormCatch());
        specialForms.put(Symbol.UNWINDPROTECT, new SpecialFormUnwindProtect());
        specialForms.put(Symbol.TAGBODY, new SpecialFormTagBody());
        specialForms.put(Symbol.GO, new SpecialFormGo());
        specialForms.put(Symbol.BLOCK, new SpecialFormBlock());
        specialForms.put(Symbol.RETURNFROM, new SpecialFormReturnFrom());
        specialForms.put(Symbol.MULTIPLEVALUECALL, new SpecialFormMultipleValueCall());

        specialForms.put(Symbol.CAR, new SpecialFormCar());
        specialForms.put(Symbol.CDR, new SpecialFormCdr());
        specialForms.put(Symbol.CONS, new SpecialFormCons());

        specialForms.put(Symbol.DEFUN, new SpecialFormDefun());
        specialForms.put(Symbol.DEFMACRO, new SpecialFormDefmacro());
        specialForms.put(Symbol.DEFCONSTANT, new SpecialFormDefconstant());
        specialForms.put(Symbol.DEFVAR, new SpecialFormDefvar());
        specialForms.put(Symbol.DEFPARAMETER, new SpecialFormDefparameter());
    }

    public static CompiledCode compile(Obj expr, VM vm, CompileEnv env) {
        CompiledCode code = new CompiledCode();
        compile(expr, code, vm, env);
        code.resolveAddresses();
        code.optimize();
        code.publish();
        return code;
    }

    public static void compileBlock(Symbol name, Obj block, CompiledCode code, VM vm, CompileEnv env) {
        specialForms.get(Symbol.BLOCK).compile(new Cons(name, block), code, vm, env);
    }

    public static void compileProgn(Obj body, CompiledCode code, VM vm, CompileEnv env) {
        if (body != Symbol.NIL) {
            compile(body.first(), code, vm, env);
            body = body.rest();
            while (body != Symbol.NIL) {
                code.emitPop();
                compile(body.first(), code, vm, env);
                body = body.rest();
            }
        } else {
            code.emitConst(Symbol.NIL);
        }
    }

    public static void compile(Obj expr, CompiledCode code, VM vm, CompileEnv env) {
        if (expr.isAtom()) {
            if (expr.isSymbol()) {
                Symbol s = expr.asSymbol();
                if (vm.constants.contains(s)) {
                    code.emitConst(vm.denv.get(s));
                } else if (s.isKeyword()) {
                    code.emitConst(s);
                } else if (env.isBound(s)) {
                    code.emitVar(s);
                } else {
                    code.emitDVar(s);
                }
            } else {
                code.emitConst(expr);
            }
        } else if (isConstExpr(expr, vm)) {
            compile(constExprValue(expr, vm), code, vm, env);
        } else if (expr.first() == Symbol.intern("1+")) {
            compile(expr.second(), code, vm, env);
            code.emitInc();
        } else if (expr.first() == Symbol.intern("1-")) {
            compile(expr.second(), code, vm, env);
            code.emitDec();
        } else if (expr.first() == Symbol.intern("=") && expr.rest() != Symbol.NIL && expr.rest().length() == 2) {
            compile(expr.second(), code, vm, env);
            compile(expr.third(), code, vm, env);
            code.emitNumEq();
        } else if (expr.first() == Symbol.intern("eq") && expr.rest() != Symbol.NIL && expr.rest().length() == 2) {
            compile(expr.second(), code, vm, env);
            compile(expr.third(), code, vm, env);
            code.emitEq();
        } else {
            Symbol s = expr.first().asSymbol();
            if (specialForms.containsKey(s)) {
                specialForms.get(s).compile(expr.rest(), code, vm, env);
            } else {
                compileCall(expr, code, vm, env);
            }
        }
    }

    private static boolean isConstExpr(Obj expr, VM vm) {
        if (expr.isNumber()) {
            return true;
        } else if (expr.isSymbol() && vm.constants.contains(expr) && vm.denv.get(expr.asSymbol()).isNumber()) {
            return true;
        } else if (expr.isCons()) {
            Obj fn = expr.first();
            Obj args = expr.rest();
            if (fn == Symbol.intern("+") || fn == Symbol.intern("-") || fn == Symbol.intern("*") || fn == Symbol.intern("/")) {
                while (args != Symbol.NIL) {
                    if (!isConstExpr(args.first(), vm)) return false;
                    args = args.rest();
                }
                return true;
            }
        }
        return false;
    }

    private static Obj constExprValue(Obj expr, VM vm) {
        if (expr.isNumber()) {
            return expr;
        } else if (expr.isSymbol()) {
            return vm.denv.get(expr.asSymbol());
        } else if (expr.isCons()) {
            Obj fn = expr.first();
            Obj args = expr.rest();
            if (fn == Symbol.intern("+")) {
                if (args == Symbol.NIL) {
                    return IntNum.ZERO;
                } else {
                    Number result = constExprValue(args.first(), vm).asNumber();
                    args = args.rest();
                    while (args != Symbol.NIL) {
                        result = result.add(constExprValue(args.first(), vm).asNumber());
                        args = args.rest();
                    }
                    return result;
                }
            } else if (fn == Symbol.intern("-")) {
                if (args == Symbol.NIL) {
                    return IntNum.ZERO;
                } else if (args.length() == 1) {
                    return IntNum.ZERO.subtract(constExprValue(args.first(), vm).asNumber());
                } else {
                    Number result = constExprValue(args.first(), vm).asNumber();
                    args = args.rest();
                    while (args != Symbol.NIL) {
                        result = result.subtract(constExprValue(args.first(), vm).asNumber());
                        args = args.rest();
                    }
                    return result;
                }
            } else if (fn == Symbol.intern("*")) {
                if (args == Symbol.NIL) {
                    return IntNum.ONE;
                } else {
                    Number result = constExprValue(args.first(), vm).asNumber();
                    args = args.rest();
                    while (args != Symbol.NIL) {
                        result = result.multiply(constExprValue(args.first(), vm).asNumber());
                        args = args.rest();
                    }
                    return result;
                }
            } else if (fn == Symbol.intern("/")) {
                if (args == Symbol.NIL) {
                    return IntNum.ONE;
                } else if (args.length() == 1) {
                    return IntNum.ONE.divide(constExprValue(args.first(), vm).asNumber());
                } else {
                    Number result = constExprValue(args.first(), vm).asNumber();
                    args = args.rest();
                    while (args != Symbol.NIL) {
                        result = result.divide(constExprValue(args.first(), vm).asNumber());
                        args = args.rest();
                    }
                    return result;
                }
            }
        }
        throw new LispException(Symbol.GENERALCONDITION, "error in evaluating constant expression");
    }

    private static void compileCall(Obj expr, CompiledCode code, VM vm, CompileEnv env) {
        Symbol fn = expr.first().asSymbol();
        int argc = 0;
        if (expr.rest() != Symbol.NIL) {
            LinkedList<Obj> a = new LinkedList<Obj>();
            Obj args = expr.rest();
            while (args != Symbol.NIL) {
                a.push(args.first());
                args = args.rest();
            }
            while (!a.isEmpty()) {
                compile(a.pop(), code, vm, env);
                argc++;
            }
        }
        code.emitArgc(argc);
        code.emitFCall(fn);
    }

    private static void bind(Symbol s, CompiledCode code, CompileEnv env) {
        if (env.isSpecial(s)) {
            code.emitDBind(s);
            env.sbind(s);
        } else {
            code.emitBind(s);
            env.bind(s);
        }
    }

    public static void compileLambdalist(Symbol name, Obj lambdalist, boolean destructure, CompiledCode code, VM vm, CompileEnv env) {
        code.emitPopReg();

        // bind required params
        int requiredcount = 0;
        Obj o = lambdalist;
        while (o != Symbol.NIL && (o.first().isCons() || !isKeyword(o.first().asSymbol()))) {
            requiredcount++;
            o = o.rest();
        }
        if (requiredcount > 0) code.emitMinArgs(requiredcount);
        o = lambdalist;
        while (o != Symbol.NIL && (o.first().isCons() || !isKeyword(o.first().asSymbol()))) {
            if (destructure && o.first().isCons()) {
                code.emitPushReg();
                code.emitSwap();
                code.emitSplitList();
                compileLambdalist(name, o.first(), true, code, vm, env);
                code.emitPopReg();
            } else {
                bind(o.first().asSymbol(), code, env);
            }
            o = o.rest();
        }
        if (requiredcount > 0) code.emitDecReg(requiredcount);

        List<Obj> optforms = new ArrayList<Obj>();
        List<Integer> optlabels = new ArrayList<Integer>();

        // bind optional params if needed
        if (o != Symbol.NIL && o.first() == Symbol.LLOPTIONAL) {
            o = o.rest();
            while (o != Symbol.NIL) {
                if (!o.first().isCons() && isKeyword(o.first().asSymbol())) break;
                int label = code.createLabel();
                optlabels.add(label);
                code.emitSubZReg(label);
                Symbol s;
                if (o.first().isCons()) {
                    optforms.add(o.first().second());
                    s = o.first().first().asSymbol();
                } else {
                    optforms.add(Symbol.NIL);
                    s = o.first().asSymbol();
                }
                bind(s, code, env);
                code.emitDecZReg();
                o = o.rest();
            }
        }

        boolean gotall = false;

        // bind rest list
        if (o != Symbol.NIL && (o.first() == Symbol.LLREST || o.first() == Symbol.LLBODY)) {
            o = o.rest();
            if (!o.first().isSymbol()) throw new LispException(Symbol.GENERALCONDITION, "expected rest parameter symbol instead of " + o.first());
            code.emitPushReg();
            code.emitRestList();
            bind(o.first().asSymbol(), code, env);
            gotall = true;
            o = o.rest();
        }

        // bind keywords
        if (o != Symbol.NIL && o.first() == Symbol.LLKEY) {
            o = o.rest();
            boolean allowotherkeys = false;
            while (o != Symbol.NIL) {
                Obj obj = o.first();
                if (obj == Symbol.intern("&allow-other-keys")) {
                    allowotherkeys = true;
                } else {
                    Symbol kw;
                    Obj val;
                    if (obj.isCons()) {
                        kw = obj.first().asSymbol();
                        val = obj.second();
                    } else {
                        kw = obj.asSymbol();
                        val = Symbol.NIL;
                    }
                    int deflabel = code.createLabel();
                    Symbol kword = Symbol.intern(":" + kw.getName());
                    code.emitKwVal(kword, deflabel);
                    bind(kw, code, env);
                    optlabels.add(deflabel);
                    optforms.add(val);
                }
                o = o.rest();
            }
            gotall = true; // we check ourselves for too many args...
            if (allowotherkeys) {
                removeRemainingArgs(code);
            } else {
                code.emitCheckNoArgs();
            }
        } else if (gotall) {
            removeRemainingArgs(code);
        }

        if (o != Symbol.NIL) throw new LispException(Symbol.GENERALCONDITION, "invalid lambda list " + lambdalist + " in function " + name);

        if (!gotall) code.emitCheckNoArgs();

        // the init forms for the optionals
        int end = code.createLabel();
        code.emitJump(end);
        for (int i = 0; i < optforms.size(); i++) {
            code.emitLabel(optlabels.get(i));
            compile(optforms.get(i), code, vm, env);
            code.emitSubRetV();
        }

        code.emitLabel(end);
    }

    private static void removeRemainingArgs(CompiledCode code) {
        int end = code.createLabel();
        int loop = code.emitLabel();
        code.emitJumpZReg(end);
        code.emitPop();
        code.emitDecReg(1);
        code.emitJump(loop);
        code.emitLabel(end);
    }

    private static boolean isKeyword(Symbol s) {
        return s == Symbol.LLOPTIONAL ||
               s == Symbol.LLREST ||
               s == Symbol.LLBODY ||
               s == Symbol.LLKEY;
    }

}
