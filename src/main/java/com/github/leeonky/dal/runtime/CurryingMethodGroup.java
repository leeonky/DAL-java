package com.github.leeonky.dal.runtime;

import com.github.leeonky.interpreter.FunctionUtil;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.joining;

class CurryingMethodGroup implements CurryingMethod {
    private final List<InstanceCurryingMethod> curryingMethods;

    CurryingMethodGroup(List<InstanceCurryingMethod> curryingMethods) {
        this.curryingMethods = curryingMethods;
    }

    @Override
    public CurryingMethodGroup call(Object arg) {
        return new CurryingMethodGroup(curryingMethods.stream().map(curryingMethod ->
                curryingMethod.call(arg)).collect(Collectors.toList()));
    }

    @Override
    public Object resolve() {
        Optional<InstanceCurryingMethod> curryingMethod = FunctionUtil.oneOf(
                () -> selectCurryingMethod(InstanceCurryingMethod::allParamsSameType),
                () -> selectCurryingMethod(InstanceCurryingMethod::allParamsBaseType),
                () -> selectCurryingMethod(InstanceCurryingMethod::allParamsConvertible));
        return curryingMethod.isPresent() ? curryingMethod.get().resolve() : this;
    }

    private Optional<InstanceCurryingMethod> selectCurryingMethod(Predicate<InstanceCurryingMethod> predicate) {
        List<InstanceCurryingMethod> methods = curryingMethods.stream().filter(predicate).collect(Collectors.toList());
        if (methods.size() > 1)
            throw new IllegalStateException("More than one currying method:\n" + methods.stream().map(
                    instanceCurryingMethod -> "  " + instanceCurryingMethod.toString()).collect(joining("\n")));
        return methods.stream().findFirst();
    }

    @Override
    public Set<Object> fetchArgRange(RuntimeContextBuilder runtimeContextBuilder) {
//        TODO  **********************
        return curryingMethods.get(0).fetchArgRange(runtimeContextBuilder);
    }

    @Override
    public String toString() {
//        TODO  **********************
        return curryingMethods.get(0).toString();
    }
}
