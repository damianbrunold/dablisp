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

import java.util.List;

public class MacroExpander {

    public static Obj expand(Obj expr, VM vm) {
        if (canExpand(expr)) {
            return expandExpr(expr, vm, false);
        } else {
            return expr;
        }
    }

    public static Obj expand1(Obj expr, VM vm) {
        if (canExpand(expr)) {
            return expandExpr(expr, vm, true);
        } else {
            return expr;
        }
    }

    private static Obj expandExpr(Obj expr, VM vm, boolean once) {
        if (expr.isAtom()) {
            return expr;
        } else if (expr == Symbol.NIL) {
            return expr;
        } else if (expr.asCons().getCar() == Symbol.QUOTE) {
            return expr;
        } else {
            Object[] result = expandForm(expr.asCons(), vm);
            boolean changed = ((Boolean) result[1]).booleanValue();
            if (once) return (Obj) result[0];
            if (changed) {
                return expandExpr((Obj) result[0], vm, false);
            } else {
                return expandSubforms((Cons) result[0], vm);
            }
        }
    }

    private static Object[] expandForm(Cons form, VM vm) {
        Object[] result = new Object[2];
        result[0] = form;
        result[1] = false;
        if (form.getCar().isSymbol()) {
            Symbol s = form.getCar().asSymbol();
            if (vm.fenv.boundp(s)) {
                Fn fn = vm.fenv.get(s).asFn();
                if (fn.isMacro()) {
                    MacroDef macro = fn.asMacro();
                    if (form.rest() == Symbol.NIL) {
                        vm.stack.push(IntNum.ZERO);
                    } else {
                        List<Obj> args = form.rest().asCons().asJavaList();
                        for (int i = args.size() - 1; i >= 0; i--) {
                            vm.stack.push(args.get(i));
                        }
                        vm.stack.push(IntNum.fromLong(args.size()));
                    }
                    vm.ip++;
                    vm.pushControlStack(macro);
                    result[0] = vm.run();
                    result[1] = true;
                }
            }
        }
        return result;
    }

    private static Obj expandSubforms(Obj form, VM vm) {
        if (form == Symbol.NIL) return Symbol.NIL;
        if (form.rest() == Symbol.NIL || form.rest().isCons()) {
            return new Cons(expandExpr(form.first(), vm, false), expandSubforms(form.rest(), vm));
        } else {
            return new Cons(expandExpr(form.first(), vm, false), expandExpr(form.rest(), vm, false));
        }
    }

    private static boolean canExpand(Obj expr) {
        if (expr.isAtom()) return true;
        if (expr == Symbol.NIL) return true;
        if (expr.first().isSymbol()) {
            Symbol s = expr.first().asSymbol();
            return !s.getName().equals("defmacro");
        }
        return true;
    }

}
