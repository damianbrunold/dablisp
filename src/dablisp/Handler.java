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

public class Handler extends Obj {

    @Override
    public boolean isCatcher() { return false; }

    @Override
    public boolean isUnwindProtect() { return false; }

    @Override
    public boolean isBlock() { return false; }

    @Override
    public boolean isTagBody() { return false; }

    @Override
    public Catcher asCatcher() { throw new RuntimeException("not a catcher"); }

    @Override
    public UnwindProtect asUnwindProtect() { throw new RuntimeException("not an unwindprotect"); }

    @Override
    public Block asBlock() { throw new RuntimeException("not a block"); }

    @Override
    public TagBody asTagBody() { throw new RuntimeException("not a tagbody"); }

    @Override
    public String displayValue() {
        return toString();
    }

    @Override
    public boolean isAtom() {
        return true;
    }

    @Override
    public boolean isCons() {
        return false;
    }

    @Override
    public IntNum sxhash() {
        return IntNum.ZERO;
    }

    @Override
    public String toString() {
        return "#<handler>";
    }

    @Override
    public boolean isHandler() { return true; }

    @Override
    public Handler asHandler() { return this; }

}
