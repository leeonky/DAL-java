package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.Converter;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static com.github.leeonky.util.Suppressor.get;
import static java.util.stream.Collectors.toList;

public class CurryingMethod {
    private final Object instance;
    private final Method method;
    private final List<Object> args = new ArrayList<>();

    public CurryingMethod(Object instance, Method method) {
        this.method = method;
        if (Modifier.isStatic(method.getModifiers())) {
            this.instance = null;
            args.add(instance);
        } else
            this.instance = instance;
    }

    public Object call(Object arg, Converter converter) {
        CurryingMethod curryingMethod = new CurryingMethod(instance, method);
        curryingMethod.args.clear();
        curryingMethod.args.addAll(args);
        curryingMethod.args.add(converter.tryConvert(method.getParameters()[args.size()].getType(), arg));
        return curryingMethod.resolve();
    }

    public Method getMethod() {
        return method;
    }

    public List<Object> getArgs() {
        return args;
    }

    public String parameterInfo() {
        List<String> parameters = Arrays.stream(method.getParameters()).map(Parameter::toString).collect(toList());
        if (parameters.size() > 0) parameters.set(args.size(), "> " + parameters.get(args.size()));
        return parameters.stream().collect(Collectors.joining(",\n",
                String.format("%s.%s(\n", method.getDeclaringClass().getName(), method.getName()), "\n)"));
    }

    public Object resolve() {
        return args.size() == method.getParameterCount() ? get(() -> method.invoke(instance, args.toArray())) : this;
    }
}
