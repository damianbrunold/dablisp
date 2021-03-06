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

import java.util.List;

import dablisp.CompileEnv;
import dablisp.CompiledCode;
import dablisp.Compiler;
import dablisp.IntNum;
import dablisp.Obj;
import dablisp.Symbol;
import dablisp.VM;

public class SpecialFormMultipleValueCall extends SpecialForm {

    @Override
    public void compile(Obj args, CompiledCode code, VM vm, CompileEnv env) {
        // (multiple-value-call function-form &rest forms)
        checkArgsMin(args, 1);
        Compiler.compile(args.first(), code, vm, env);
        code.emitConst(IntNum.ZERO);
        if (args.rest() != Symbol.NIL) {
            List<Obj> forms = args.rest().asCons().asJavaList();
            for (int i = forms.size() - 1; i >= 0; i--) {
                Compiler.compile(forms.get(i), code, vm, env);
                code.emitMvalSplit();
            }
        }
        code.emitDup();
        code.emitInc();
        code.emitBubble();
        code.emitCall();
    }

}
