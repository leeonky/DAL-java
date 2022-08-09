package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.Converter;

import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Predicate;

import static com.github.leeonky.util.Suppressor.get;
import static java.lang.String.format;
import static java.util.Collections.emptySet;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

class InstanceCurryingMethod implements CurryingMethod {
    protected final Object instance;
    protected final Method method;
    @Deprecated
    private final List<Object> args = new ArrayList<>();
    protected final List<ParameterValue> parameterValues = new ArrayList<>();

    protected InstanceCurryingMethod(Object instance, Method method, Converter converter) {
        this.method = method;
        this.instance = instance;
    }

    @Override
    public InstanceCurryingMethod call(Object arg) {
        InstanceCurryingMethod curryingMethod = clone();
        curryingMethod.args.addAll(args);
        curryingMethod.args.add(arg);
        curryingMethod.parameterValues.addAll(parameterValues);
        curryingMethod.parameterValues.add(new ParameterValue(method.getParameters()[parameterValues.size() + parameterOffset()], arg));
        return curryingMethod;
    }

    protected int parameterOffset() {
        return 0;
    }

    @Override
    protected InstanceCurryingMethod clone() {
        return new InstanceCurryingMethod(instance, method, converter);
    }

    private String parameterInfo() {
        List<String> parameters = Arrays.stream(method.getParameters()).map(Parameter::toString).collect(toList());
        int argPosition = parameterValues.size() + parameterOffset();
        if (parameters.size() > 0)
            parameters.set(argPosition, "> " + parameters.get(argPosition));
        return parameters.stream().collect(joining(",\n", format("%s.%s(\n", method.getDeclaringClass().getName(),
                method.getName()), "\n)"));
    }

    public boolean allParamsSameType() {
        return testParameterTypes(ParameterValue::isSameType);
    }

    private boolean testParameterTypes(Predicate<ParameterValue> checking) {
        return isArgCountEnough() && parameterValues.stream().allMatch(checking);
    }

    private boolean isArgCountEnough() {
        return method.getParameterCount() - parameterOffset() == parameterValues.size();
    }

    public boolean allParamsBaseType() {
        return testParameterTypes(ParameterValue::isSuperType);
    }

    public boolean allParamsConvertible(Converter converter) {
        return testParameterTypes(parameterValue -> parameterValue.isConvertibleType(converter));
    }

    @Override
    public Object resolve(Converter converter) {
        return get(() -> method.invoke(instance, parameterValues.stream().map(parameterValue ->
                parameterValue.getArg(converter)).collect(toList()).toArray()));
    }

    protected List<Object> args() {
        return args;
    }

    //    TODO arg range for type instant ******************************
    @Override
    public Set<Object> fetchArgRange(RuntimeContextBuilder runtimeContextBuilder) {
        BiFunction<Object, List<Object>, List<Object>> rangeFactory = runtimeContextBuilder.fetchCurryingMethodArgRange(method);
        if (rangeFactory != null)
            return new LinkedHashSet<>(rangeFactory.apply(instance, args()));
        System.err.printf("No arg range for %s, give the range or use `:`%n", parameterInfo());
        return emptySet();
    }

    @Override
    public String toString() {
        return String.format("instance: %s\nmethod: %s\nargs: %s", instance, method, args());
    }
}
