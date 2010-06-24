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
package dablisp.fn;

import java.util.ArrayList;
import java.util.List;

import dablisp.Env;
import dablisp.Fn;
import dablisp.Obj;
import dablisp.Stack;

public class FnApply extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCountMin(argc, 1);
        Fn fn = stack.pop().asFn();
        int n = argc - 1;
        if (n > 0) {
            if (stack.peek(n - 1).isCons()) {
                List<Obj> args = new ArrayList<Obj>(n - 1);
                for (int i = 0; i < n - 1; i++) {
                    args.add(stack.pop());
                }
                List<Obj> last = stack.pop().asCons().asJavaList();
                for (int i = last.size() - 1; i >= 0; i--) {
                    stack.push(last.get(i));
                }
                for (int i = n - 2; i >= 0; i--) {
                    stack.push(args.get(i));
                }
                n = n - 1 + last.size();
            }
        }
        fn.applyfn(n, stack, env, fenv);
    }

    @Override
    public String getName() {
        return "apply";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " fn &rest args) - function -  applies fn to args and returns the result value";
    }

}
