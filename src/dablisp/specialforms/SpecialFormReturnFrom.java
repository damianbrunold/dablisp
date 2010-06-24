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
import dablisp.LispException;
import dablisp.Obj;
import dablisp.Symbol;
import dablisp.VM;

public class SpecialFormReturnFrom extends SpecialForm {

    @Override
    public void compile(Obj args, CompiledCode code, VM vm, CompileEnv env) {
        // (return-from name &optional result)
        checkArgs(args, 1, 2);
        Symbol name = args.first().asSymbol();
        Obj result = Symbol.NIL;
        if (args.length() == 2) {
            result = args.second();
        }
        if (!env.hasBlock(name)) throw new LispException(Symbol.GENERALCONDITION, "no block named " + name + " found");
        if (result != Symbol.NIL) {
            int resultlabel = code.createLabel();
            code.emitReturnFrom(name, resultlabel);
            code.emitLabel(resultlabel);
            Compiler.compile(result, code, vm, env);
            code.emitBRetV(name);
        } else {
            code.emitReturnFrom(name, -1);
        }
    }

}
