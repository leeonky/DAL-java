package com.github.leeonky.dal.runtime;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;

import static com.github.leeonky.util.function.Extension.getFirstPresent;
import static java.util.Optional.of;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

class CurryingMethodGroup implements CurryingMethod {
    private final CurryingMethodGroup parent;
    private final List<InstanceCurryingMethod> curryingMethods;
    private InstanceCurryingMethod resolvedCurryingMethod;

    CurryingMethodGroup(List<InstanceCurryingMethod> curryingMethods, CurryingMethodGroup parent) {
        this.curryingMethods = curryingMethods;
        this.parent = parent;
    }

    @Override
    public CurryingMethodGroup call(Object arg) {
        return new CurryingMethodGroup(curryingMethods.stream().map(curryingMethod ->
                curryingMethod.call(arg)).collect(toList()), this);
    }

    @Override
    public Object resolve() {
        Optional<InstanceCurryingMethod> curryingMethod = getFirstPresent(
                () -> selectCurryingMethod(InstanceCurryingMethod::allParamsSameType),
                () -> selectCurryingMethod(InstanceCurryingMethod::allParamsBaseType),
                () -> selectCurryingMethod(InstanceCurryingMethod::allParamsConvertible));
        return curryingMethod.isPresent() ? setResolveCurryingMethod(curryingMethod.get()).resolve() : this;
    }

    private InstanceCurryingMethod setResolveCurryingMethod(InstanceCurryingMethod curryingMethod) {
        if (parent != null)
            parent.setResolveCurryingMethod(curryingMethod);
        resolvedCurryingMethod = curryingMethod;
        return curryingMethod;
    }

    private Optional<InstanceCurryingMethod> selectCurryingMethod(Predicate<InstanceCurryingMethod> predicate) {
        List<InstanceCurryingMethod> methods = curryingMethods.stream().filter(predicate).collect(toList());
        if (methods.size() > 1) {
            List<InstanceCurryingMethod> sameInstanceTypeMethods = methods.stream()
                    .filter(StaticCurryingMethod.class::isInstance).collect(toList());
            return of(getFirstPresent(() -> getOnlyOne(sameInstanceTypeMethods), () -> getOnlyOne(sameInstanceTypeMethods.stream()
                    .filter(InstanceCurryingMethod::isSameInstanceType).collect(toList())))
                    .orElseThrow(() -> new InvalidPropertyException("More than one currying method:\n"
                            + methods.stream().map(instanceCurryingMethod -> "  " + instanceCurryingMethod.toString())
                            .sorted().collect(joining("\n")))));
        }
        return methods.stream().findFirst();
    }

    private Optional<InstanceCurryingMethod> getOnlyOne(List<InstanceCurryingMethod> list) {
        if (list.size() == 1)
            return of(list.get(0));
        return Optional.empty();
    }

    @Override
    public Set<Object> fetchArgRange(RuntimeContextBuilder runtimeContextBuilder) {
        return queryResolvedMethod().map(method ->
                method.fetchArgRange(runtimeContextBuilder)).orElseGet(Collections::emptySet);
    }

    private Optional<InstanceCurryingMethod> queryResolvedMethod() {
        return curryingMethods.stream().filter(method -> method.method.equals(resolvedCurryingMethod.method)).findFirst();
    }

    @Override
    public Object convertToArgType(Object obj) {
        return queryResolvedMethod().map(method -> method.convertToArgType(obj)).orElse(obj);
    }
}
