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
        if (method.getParameters().length == args.size() + 1) {
            ArrayList<Object> args = new ArrayList<>(this.args);
            args.add(arg);
            return Suppressor.get(() -> method.invoke(instance, args.toArray()));
        }
        CurryingMethod curryingMethod = new CurryingMethod(instance, method);
        curryingMethod.args.addAll(args);
        curryingMethod.args.add(arg);
        return curryingMethod;
    }
}
