package com.github.leeonky.dal.runtime;

import com.github.leeonky.interpreter.FunctionUtil;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static java.util.Optional.of;
import static java.util.stream.Collectors.joining;

class CurryingMethodGroup implements CurryingMethod {
    private final Optional<CurryingMethodGroup> parent;
    private final List<InstanceCurryingMethod> curryingMethods;
    private Optional<InstanceCurryingMethod> resolvedCurryingMethod = Optional.empty();

    CurryingMethodGroup(List<InstanceCurryingMethod> curryingMethods, CurryingMethodGroup parent) {
        this.curryingMethods = curryingMethods;
        this.parent = Optional.ofNullable(parent);
    }

    @Override
    public CurryingMethodGroup call(Object arg) {
        return new CurryingMethodGroup(curryingMethods.stream().map(curryingMethod ->
                curryingMethod.call(arg)).collect(Collectors.toList()), this);
    }

    @Override
    public Object resolve() {
        Optional<InstanceCurryingMethod> curryingMethod = FunctionUtil.oneOf(
                () -> selectCurryingMethod(InstanceCurryingMethod::allParamsSameType),
                () -> selectCurryingMethod(InstanceCurryingMethod::allParamsBaseType),
                () -> selectCurryingMethod(InstanceCurryingMethod::allParamsConvertible));
        return curryingMethod.isPresent() ? setResolveCurryingMethod(curryingMethod.get()).resolve() : this;
    }

    private InstanceCurryingMethod setResolveCurryingMethod(InstanceCurryingMethod curryingMethod) {
        parent.ifPresent(p -> p.setResolveCurryingMethod(curryingMethod));
        resolvedCurryingMethod = of(curryingMethod);
        return curryingMethod;
    }

    private Optional<InstanceCurryingMethod> selectCurryingMethod(Predicate<InstanceCurryingMethod> predicate) {
        List<InstanceCurryingMethod> methods = curryingMethods.stream().filter(predicate).collect(Collectors.toList());
        if (methods.size() > 1) {
            List<InstanceCurryingMethod> sameInstanceTypeMethods = methods.stream()
                    .filter(InstanceCurryingMethod::isSameInstanceType).collect(Collectors.toList());
            if (sameInstanceTypeMethods.size() == 1)
                return sameInstanceTypeMethods.stream().findFirst();
            throw new IllegalStateException("More than one currying method:\n" + methods.stream().map(
                    instanceCurryingMethod -> "  " + instanceCurryingMethod.toString()).sorted().collect(joining("\n")));
        }
        return methods.stream().findFirst();
    }

    @Override
    public Set<Object> fetchArgRange(RuntimeContextBuilder runtimeContextBuilder) {
        return resolvedCurryingMethod.flatMap(m -> curryingMethods.stream()
                .filter(method -> method.method.equals(m.method)).findFirst()
                .map(method -> method.fetchArgRange(runtimeContextBuilder))).orElseGet(Collections::emptySet);
    }
}
