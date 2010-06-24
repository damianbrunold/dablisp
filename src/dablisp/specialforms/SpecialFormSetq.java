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
package dablisp.specialforms;

import dablisp.CompileEnv;
import dablisp.CompiledCode;
import dablisp.Compiler;
import dablisp.Lisp;
import dablisp.LispException;
import dablisp.Obj;
import dablisp.Strm;
import dablisp.Symbol;
import dablisp.VM;

public class SpecialFormSetq extends SpecialForm {

    @Override
    public void compile(Obj args, CompiledCode code, VM vm, CompileEnv env) {
        // (setq s1 val1 s2 val2 ...)
        checkArgsMin(args, 0);
        while (args != Symbol.NIL) {
            Symbol s = args.first().asSymbol();
            Obj expr = args.rest().first();
            Compiler.compile(expr, code, vm, env);
            if (args.rest().rest() == Symbol.NIL) {
                code.emitDup();
            }
            if (Lisp.vm.constants.contains(s)) {
                throw new LispException(Symbol.GENERALCONDITION, "cannot change constant " + s);
            }
            if (env.isBound(s)) {
                code.emitSetVar(s);
            } else {
                if (!env.isSpecial(s)) {
                    Strm out = Lisp.getErrorOutput();
                    out.print();
                    out.print("WARN: " + s + " is undefined");
                    code.emitDBind(s);
                    env.sbind(s);
                } else {
                    code.emitSetDVar(s);
                }
            }
            args = args.rest().rest();
        }
    }

}
