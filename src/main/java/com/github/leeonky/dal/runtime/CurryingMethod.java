package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.Suppressor;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

public class CurryingMethod {
    private final Object instance;
    private final Method method;
    private final List<Object> args = new ArrayList<>();

    public CurryingMethod(Object instance, Method method) {
        this.instance = instance;
        this.method = method;
    }

    public Object call(Object arg) {
        return method.getParameters().length != getArgs().size() + 1 ? currying(arg)
                : Suppressor.get(() -> method.invoke(instance, new ArrayList<Object>() {{
            addAll(getArgs());
            add(arg);
        }}.toArray()));
    }

    private CurryingMethod currying(Object arg) {
        CurryingMethod curryingMethod = new CurryingMethod(instance, method);
        curryingMethod.args.addAll(args);
        curryingMethod.args.add(arg);
        return curryingMethod;
    }

    public Method getMethod() {
        return method;
    }

    public List<Object> getArgs() {
        return args;
    }
}
