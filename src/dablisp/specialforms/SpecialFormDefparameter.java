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
import dablisp.Obj;
import dablisp.Symbol;
import dablisp.VM;

public class SpecialFormDefparameter extends SpecialForm {

    @Override
    public void compile(Obj args, CompiledCode code, VM vm, CompileEnv env) {
        // (defparameter name value &optional documentation)
        checkArgs(args, 2, 3);
        Symbol name = args.first().asSymbol();
        Compiler.compile(args.second(), code, vm, env);
        if (env.isSpecial(name)) {
            code.emitSetDVar(name);
        } else {
            code.emitDBind(name);
        }
        code.emitConst(name);
        env.sbind(name);
    }

}
