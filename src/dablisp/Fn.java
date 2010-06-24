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

public abstract class Fn extends Atom {

    public void applyfn(int argc, Stack stack, Env env, Env fenv) {
        Lisp.enterTrace(this, argc, stack);
        apply(argc, stack, env, fenv);
        Lisp.exitTrace(this, stack);
    }

    public abstract void apply(int argc, Stack stack, Env env, Env fenv);

    public boolean isPrimitive() {
        return true;
    }

    @Override
    public String toString() {
        return "#<fn " + getName() + ">";
    }

    @Override
    public String displayValue() {
        return toString();
    }

    public abstract String getDocstring();

    public void checkArgCountRange(int argc, int min, int max) {
        if (min <= argc && argc <= max) return;
        throw new LispException(Symbol.FNCONDITION, "expected between " + min + " and " + max + " args but got " + argc);
    }

    public void checkArgCountMin(int argc, int min) {
        if (min > argc) {
            throw new LispException(Symbol.FNCONDITION, "expected min " + min + " args but got " + argc);
        }
    }

    public void checkArgCountMax(int argc, int max) {
        if (argc > max) {
            throw new LispException(Symbol.FNCONDITION, "expected max " + max + " args but got " + argc);
        }
    }

    public void checkArgCount(int argc, int expected) {
        if (argc != expected) {
            throw new LispException(Symbol.FNCONDITION, "expected " + expected + " args but got " + argc);
        }
    }

    @Override
    public boolean isMacro() { return false; }

    @Override
    public boolean isFn() { return true; }

    @Override
    public Fn asFn() { return this; }

    public abstract String getName();

    @Override
    public Obj types() { return new Cons(Symbol.FUNCTION, super.types()); }

    @Override
    public IntNum sxhash() { return getName() != null ? IntNum.fromLong(getName().hashCode()) : IntNum.ZERO; }

    public void setEnv(Env env, Env fenv) {}
}
