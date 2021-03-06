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
import dablisp.VM;

public class SpecialFormUnwindProtect extends SpecialForm {

    @Override
    public void compile(Obj args, CompiledCode code, VM vm, CompileEnv env) {
        // (unwind-protect form &rest cleanupforms)
        checkArgsMin(args, 1);
        int cleanupLabel = code.createLabel();
        int endLabel = code.createLabel();
        code.emitAddUnwindProtect(cleanupLabel);
        Compiler.compile(args.first(), code, vm, env);
        code.emitSub(cleanupLabel);
        code.emitJump(endLabel);
        code.emitLabel(cleanupLabel);
        Compiler.compileProgn(args.rest(), code, vm, env);
        code.emitSubRet();
        code.emitLabel(endLabel);
        code.emitPopHandler();
    }

}
