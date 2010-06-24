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

public class Mvals extends Obj {

    private Obj[] values;

    public Mvals(Obj... values) {
        this.values = values;
    }

    @Override
    public boolean isAtom() { return values[0].isAtom(); }
    @Override
    public boolean isCons() { return values[0].isCons(); }

    @Override
    public boolean isKeyword() { return values[0].isKeyword(); }
    @Override
    public boolean isSymbol() { return values[0].isSymbol(); }
    @Override
    public boolean isChar() { return values[0].isChar(); }
    @Override
    public boolean isNumber() { return values[0].isNumber(); }
    @Override
    public boolean isIntNum() { return values[0].isIntNum(); }
    @Override
    public boolean isFloatNum() { return values[0].isFloatNum(); }
    @Override
    public boolean isStr() { return values[0].isStr(); }
    @Override
    public boolean isFn() { return values[0].isFn(); }
    @Override
    public boolean isMacro() { return values[0].isMacro(); }
    @Override
    public boolean isStrm() { return values[0].isStrm(); }
    @Override
    public boolean isNative() { return values[0].isNative(); }
    @Override
    public boolean isHashtable() { return values[0].isHashtable(); }
    @Override
    public boolean isMvals() { return true; }
    @Override
    public boolean isArray() { return values[0].isArray(); }
    @Override
    public boolean isVector() { return values[0].isVector(); }
    @Override
    public boolean isHandler() { return values[0].isHandler(); }
    @Override
    public boolean isCatcher() { return values[0].isCatcher(); }
    @Override
    public boolean isUnwindProtect() { return values[0].isUnwindProtect(); }
    @Override
    public boolean isBlock() { return values[0].isBlock(); }
    @Override
    public boolean isTagBody() { return values[0].isTagBody(); }

    @Override
    public Char asChar() { return values[0].asChar(); }
    @Override
    public Vector asStr() { return values[0].asStr(); }
    @Override
    public Number asNumber() { return values[0].asNumber(); }
    @Override
    public IntNum asIntNum() { return values[0].asIntNum(); }
    @Override
    public FloatNum asFloatNum() { return values[0].asFloatNum(); }
    @Override
    public Symbol asSymbol() { return values[0].asSymbol(); }
    @Override
    public Cons asCons() { return values[0].asCons(); }
    @Override
    public Fn asFn() { return values[0].asFn(); }
    @Override
    public Strm asStrm() { return values[0].asStrm(); }
    @Override
    public Native asNative() { return values[0].asNative(); }
    @Override
    public Mvals asMvals() { return this; }
    @Override
    public Array asArray() { return values[0].asArray(); }
    @Override
    public Vector asVector() { return values[0].asVector(); }
    @Override
    public Handler asHandler() { return values[0].asHandler(); }
    @Override
    public Catcher asCatcher() { return values[0].asCatcher(); }
    @Override
    public UnwindProtect asUnwindProtect() { return values[0].asUnwindProtect(); }
    @Override
    public Block asBlock() { return values[0].asBlock(); }
    @Override
    public TagBody asTagBody() { return values[0].asTagBody(); }

    @Override
    public String toString() { return values.length == 0 ? "nil" : values[0].toString(); }
    @Override
    public String displayValue() { return values.length == 0 ? "nil" : values[0].displayValue(); }

    @Override
    public IntNum sxhash() { return values[0].sxhash(); }

    @Override
    public Obj copy() {
        return this;
    }

    @Override
    public Obj types() { return new Cons(Symbol.T, Symbol.NIL); }

    @Override
    public int size() {
        return values.length;
    }

    @Override
    public Obj val(int i) {
        return values[i];
    }

    @Override
    public Obj val() {
        return val(0);
    }

}
