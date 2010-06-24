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

public abstract class Obj {

    public abstract boolean isAtom();
    public abstract boolean isCons();

    public boolean isKeyword() { return false; }
    public boolean isSymbol() { return false; }
    public boolean isChar() { return false; }
    public boolean isNumber() { return false; }
    public boolean isIntNum() { return false; }
    public boolean isFloatNum() { return false; }
    public boolean isStr() { return false; }
    public boolean isFn() { return false; }
    public boolean isMacro() { return false; }
    public boolean isStrm() { return false; }
    public boolean isNative() { return false; }
    public boolean isHashtable() { return false; }
    public boolean isMvals() { return false; }
    public boolean isArray() { return false; }
    public boolean isVector() { return false; }
    public boolean isHandler() { return false; }
    public boolean isCatcher() { return false; }
    public boolean isUnwindProtect() { return false; }
    public boolean isBlock() { return false; }
    public boolean isTagBody() { return false; }

    public Char asChar() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a char"); }
    public Vector asStr() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a str"); }
    public Number asNumber() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a number"); }
    public IntNum asIntNum() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a intnum"); }
    public FloatNum asFloatNum() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a floatnum"); }
    public Symbol asSymbol() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a symbol"); }
    public Cons asCons() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a cons"); }
    public Fn asFn() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a fn"); }
    public MacroDef asMacro() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a macro"); }
    public Strm asStrm() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a strm"); }
    public Native asNative() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a native object"); }
    public Mvals asMvals() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a mval"); }
    public Array asArray() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not an array"); }
    public Vector asVector() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a vector"); }
    public Handler asHandler() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a handler"); }
    public Catcher asCatcher() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a catcher"); }
    public UnwindProtect asUnwindProtect() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a unwindprotect"); }
    public Block asBlock() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a block"); }
    public TagBody asTagBody() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a tagbody"); }
    public Hashtable asHashtable() { throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a hashtable"); }

    @Override
    public abstract String toString();

    public abstract String displayValue();

    public Obj first() { return asCons().first(); }
    public Obj rest() { return asCons().rest(); }
    public Obj second() { return asCons().second(); }
    public Obj third() { return asCons().third(); }
    public Obj fourth() { return asCons().fourth(); }
    public int length() { return asCons().length(); }

    public abstract IntNum sxhash();

    public Obj copy() {
        return this;
    }

    public Obj types() { return new Cons(Symbol.T, Symbol.NIL); }

    public int size() {
        return 1;
    }

    public Obj val(int index) {
        if (index == 0) {
            return this;
        } else {
            return Symbol.NIL;
        }
    }

    public Obj val() {
        return this;
    }

}


