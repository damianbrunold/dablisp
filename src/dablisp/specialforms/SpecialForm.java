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

import dablisp.*;

public abstract class SpecialForm {

    public abstract void compile(Obj args, CompiledCode code, VM vm, CompileEnv env);

    protected void checkArgs(Obj args, int min, int max) {
        int argc = args == Symbol.NIL ? 0 : args.asCons().length();
        if (argc < min || max < argc) {
            String s = min == max ?
                "form needs " + min + " args but has " + argc :
                "form needs between " + min + " and " + max + " args but has " + argc;
            throw new LispException(Symbol.SYNTAXCONDITION, s);
        }
    }

    protected void checkArgsMin(Obj args, int min) {
        int argc = args == Symbol.NIL ? 0 : args.asCons().length();
        if (argc < min) {
            String s = "form needs min " + min + " args but has " + argc;
            throw new LispException(Symbol.SYNTAXCONDITION, s);
        }
    }

}
