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

import java.util.LinkedList;

public class State {

    public Env env;
    public Env fenv;
    public int ip;
    public int reg;
    public Fn fn;
    public CompiledCode code;
    public int denvlevel;
    public LinkedList<Handler> handlers;

    public State(Env env, Env fenv, int ip, int reg, Fn fn, CompiledCode code, int denvlevel, LinkedList<Handler> handlers) {
        this.env = env;
        this.fenv = fenv;
        this.ip = ip;
        this.reg = reg;
        this.fn = fn;
        this.code = code;
        this.denvlevel = denvlevel;
        this.handlers = handlers;
    }

}
