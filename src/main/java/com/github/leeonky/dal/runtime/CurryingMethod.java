package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.Converter;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.util.*;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

import static com.github.leeonky.util.Suppressor.get;
import static java.util.Collections.emptySet;
import static java.util.stream.Collectors.toList;

public class CurryingMethod {
    private final Object instance;
    private final Method method;
    private final List<Object> args = new ArrayList<>();

    public CurryingMethod(Object instance, Method method) {
        this.method = method;
        if (isaStatic()) {
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

    private String parameterInfo() {
        List<String> parameters = Arrays.stream(method.getParameters()).map(Parameter::toString).collect(toList());
        if (parameters.size() > 0) parameters.set(args.size(), "> " + parameters.get(args.size()));
        return parameters.stream().collect(Collectors.joining(",\n",
                String.format("%s.%s(\n", method.getDeclaringClass().getName(), method.getName()), "\n)"));
    }

    public Object resolve() {
        return args.size() == method.getParameterCount() ? get(() -> method.invoke(instance, args.toArray())) : this;
    }

    private List<Object> fetchArgRange(BiFunction<Object, List<Object>, List<Object>> rangeFactory) {
        if (isaStatic())
            return rangeFactory.apply(args.get(0), args.subList(1, args.size()));
        return rangeFactory.apply(instance, args);
    }

    private boolean isaStatic() {
        return Modifier.isStatic(method.getModifiers());
    }

    public Set<Object> fetchArgRange(RuntimeContextBuilder runtimeContextBuilder) {
        BiFunction<Object, List<Object>, List<Object>> rangeFactory = runtimeContextBuilder.fetchCurryingMethodArgRange(method);
        if (rangeFactory != null)
            return new LinkedHashSet<>(fetchArgRange(rangeFactory));
        System.err.printf("No arg range for %s, give the range or use `:`%n", parameterInfo());
        return emptySet();
    }
}
