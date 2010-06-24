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

public class Block extends Handler {

    public Symbol name;
    public int target;
    public int stackpointer;
    public Env env;
    public Env fenv;
    public int denvlevel;

    public Block(Symbol name, int target, int stackpointer, Env env, Env fenv, int denvlevel) {
        this.name = name;
        this.target = target;
        this.stackpointer = stackpointer;
        this.env = env;
        this.fenv = fenv;
        this.denvlevel = denvlevel;
    }

    @Override
    public boolean isBlock() { return true; }

    @Override
    public Block asBlock() { return this; }

    @Override
    public String toString() {
        return "#<block " + name + " " + target + ">";
    }

}
