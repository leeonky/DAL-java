package com.github.leeonky.dal.runtime;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

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
//    TODO refactor *****************************
    public Object resolve() {
        Optional<InstanceCurryingMethod> sameType = curryingMethods.stream()
                .filter(InstanceCurryingMethod::allParamsSameType).findFirst();
        if (sameType.isPresent())
            return sameType.get().resolve();

        Optional<InstanceCurryingMethod> baseType = curryingMethods.stream()
                .filter(InstanceCurryingMethod::allParamsBaseType).findFirst();
        if (baseType.isPresent())
            return baseType.get().resolve();

        Optional<InstanceCurryingMethod> converted = curryingMethods.stream()
                .filter(InstanceCurryingMethod::allParamsConvertible).findFirst();
        if (converted.isPresent())
            return converted.get().resolve();
        return this;
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
