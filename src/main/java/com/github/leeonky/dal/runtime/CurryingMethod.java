package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.Converter;
import com.github.leeonky.util.Suppressor;

import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toList;

public class CurryingMethod {
    private final Object instance;
    private final Method method;
    private final List<Object> args = new ArrayList<>();

    public CurryingMethod(Object instance, Method method) {
        this.instance = instance;
        this.method = method;
    }

    public CurryingMethod(Object instance, Method method, int i) {
        this.instance = null;
        this.method = method;
        args.add(instance);
    }

    public Object call(Object arg, Converter converter) {
        Object convertedArg = convertArg(arg, converter);
        return enoughArgs() ? Suppressor.get(() -> method.invoke(instance, new ArrayList<Object>() {{
            addAll(getArgs());
            add(convertedArg);
        }}.toArray())) : currying(convertedArg);
    }

    private boolean enoughArgs() {
        return method.getParameters().length == getArgs().size() + 1;
    }

    private CurryingMethod currying(Object arg) {
        CurryingMethod curryingMethod = new CurryingMethod(instance, method);
        curryingMethod.args.addAll(args);
        curryingMethod.args.add(arg);
        return curryingMethod;
    }

    private Object convertArg(Object arg, Converter converter) {
        return converter.tryConvert(method.getParameters()[args.size()].getType(), arg);
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
        if (args.size() == method.getParameterCount())
            return Suppressor.get(() -> method.invoke(instance, args.toArray()));
        return this;
    }
}
