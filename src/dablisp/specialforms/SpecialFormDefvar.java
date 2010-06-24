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

public class SpecialFormDefvar extends SpecialForm {

    @Override
    public void compile(Obj args, CompiledCode code, VM vm, CompileEnv env) {
        // (defvar name &optional value documentation)
        checkArgs(args, 1, 3);
        Symbol name = args.first().asSymbol();
        if (!env.isSpecial(name)) {
            if (args.length() > 1) {
                Compiler.compile(args.second(), code, vm, env);
            } else {
                code.emitConst(Symbol.NIL);
            }
            code.emitDBind(name);
            env.sbind(name);
        }
        code.emitConst(name);
    }

}
