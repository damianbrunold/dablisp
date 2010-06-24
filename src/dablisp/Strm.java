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

import java.io.IOException;
import java.io.PrintStream;

public class Strm extends Atom {

    protected ReadStream in;
    protected PrintStream out;

    public Strm(ReadStream in) {
        this.in = in;
    }

    public Strm(PrintStream out) {
        this.out = out;
    }

    public Strm(ReadStream in, PrintStream out) {
        this.in = in;
        this.out = out;
    }

    protected Strm() {}

    public ReadStream getIn() {
        if (in == null) throw new LispException(Symbol.STREAMCONDITION, "not an input stream");
        return in;
    }

    public PrintStream getOut() {
        if (out == null) throw new LispException(Symbol.STREAMCONDITION, "not an output stream");
        return out;
    }

    public void print(Object val) {
    	getOut().println();
    	getOut().print(val);
    }

    public void prin1(Obj val) {
        getOut().print(val);
    }

    public void princ(Obj val) {
        getOut().print(val.displayValue());
    }

    public void print() {
    	getOut().println();
    }

    public void flush() {
    	getOut().flush();
    }

    public void close() {
        try {
            if (out != null)
                out.close();
            if (in != null)
                in.close();
        } catch (IOException e) {
            throw new LispException(Symbol.IOCONDITION, "could not close stream: " + e.getMessage());
        }
    }

    @Override
    public String toString() {
        return "#<strm>";
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
    public Obj types() { return new Cons(Symbol.STREAM, super.types()); }

    @Override
    public IntNum sxhash() { return IntNum.ZERO; }

}
