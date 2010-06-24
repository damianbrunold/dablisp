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

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;

public class StringOutputStrm extends Strm {

    private ByteArrayOutputStream baos = new ByteArrayOutputStream();

    public StringOutputStrm() {
        try {
            out = new PrintStream(baos, true, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            // ignore, cannot happen
        }
    }

    @Override
    public String toString() {
        return "#<string-output-strm>";
    }

    @Override
    public String displayValue() {
        return toString();
    }

    @Override
    public boolean isStrm() { return true; }

    @Override
    public Strm asStrm() { return this; }

    @Override
    public Obj types() { return new Cons(Symbol.intern("string-output-stream"), new Cons(Symbol.STREAM, super.types())); }

    @Override
    public IntNum sxhash() { return IntNum.ZERO; }

    public void reset() {
        baos.reset();
    }

    public String getOutput() {
        if (out != null) out.flush();
        try {
            return baos.toString("UTF-8");
        } catch (UnsupportedEncodingException e) {
            // ignore, cannot happen
            return "";
        }
    }

}
