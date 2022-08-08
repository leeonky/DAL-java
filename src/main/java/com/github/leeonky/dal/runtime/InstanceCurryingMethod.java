package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.Converter;

import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.*;
import java.util.function.BiFunction;

import static com.github.leeonky.util.Suppressor.get;
import static java.lang.String.format;
import static java.util.Collections.emptySet;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

class InstanceCurryingMethod implements CurryingMethod {
    protected final Object instance;
    protected final Method method;
    private final List<Object> args = new ArrayList<>();

    protected InstanceCurryingMethod(Object instance, Method method) {
        this.method = method;
        this.instance = instance;
    }

    @Override
    public CurryingMethod call(Object arg, Converter converter) {
        InstanceCurryingMethod curryingMethod = clone();
        curryingMethod.args.addAll(args);
        curryingMethod.args.add(converter.tryConvert(method.getParameters()[args().size()].getType(), arg));
        return curryingMethod;
    }

    @Override
    protected InstanceCurryingMethod clone() {
        InstanceCurryingMethod curryingMethod;
        curryingMethod = new InstanceCurryingMethod(instance, method);
        return curryingMethod;
    }

    private String parameterInfo() {
        List<String> parameters = Arrays.stream(method.getParameters()).map(Parameter::toString).collect(toList());
        int argPosition = args().size();
        if (parameters.size() > 0) parameters.set(argPosition, "> " + parameters.get(argPosition));
        return parameters.stream().collect(joining(",\n", format("%s.%s(\n", method.getDeclaringClass().getName(),
                method.getName()), "\n)"));
    }

    @Override
    public Object resolve() {
        List<Object> args = args();
        return args.size() == method.getParameterCount() ? get(() -> method.invoke(instance, args.toArray())) : this;
    }

    protected List<Object> args() {
        return args;
    }

    @Override
    public Set<Object> fetchArgRange(RuntimeContextBuilder runtimeContextBuilder) {
        BiFunction<Object, List<Object>, List<Object>> rangeFactory = runtimeContextBuilder.fetchCurryingMethodArgRange(method);
        if (rangeFactory != null)
            return new LinkedHashSet<>(rangeFactory.apply(instance, args));
        System.err.printf("No arg range for %s, give the range or use `:`%n", parameterInfo());
        return emptySet();
    }

    @Override
    public String toString() {
        return String.format("instance: %s\nmethod: %s\nargs: %s", instance, method, args);
    }
}
