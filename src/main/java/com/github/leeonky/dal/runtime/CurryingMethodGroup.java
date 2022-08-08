package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.Converter;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

class CurryingMethodGroup implements CurryingMethod {
    private final CurryingMethodGroup parent;
    private final List<InstanceCurryingMethod> curryingMethods;

    CurryingMethodGroup(CurryingMethodGroup parent, List<InstanceCurryingMethod> curryingMethods) {
        this.parent = parent;
        this.curryingMethods = curryingMethods;
    }

    @Override
    public CurryingMethodGroup call(Object arg, Converter converter) {
        return new CurryingMethodGroup(this, curryingMethods.stream().map(curryingMethod ->
                curryingMethod.call(arg, converter)).collect(Collectors.toList()));
    }

    @Override
//    TODO refactor *****************************
    public Object resolve(Converter converter) {
        Optional<InstanceCurryingMethod> sameType = curryingMethods.stream()
                .filter(InstanceCurryingMethod::allParamsSameType).findFirst();
        if (sameType.isPresent())
            return sameType.get().resolve(converter);

        Optional<InstanceCurryingMethod> baseType = curryingMethods.stream()
                .filter(InstanceCurryingMethod::allParamsBaseType).findFirst();
        if (baseType.isPresent())
            return baseType.get().resolve(converter);

        Optional<InstanceCurryingMethod> converted = curryingMethods.stream()
                .filter(instanceCurryingMethod -> instanceCurryingMethod.allParamsConvertible(converter)).findFirst();
        if (converted.isPresent())
            return converted.get().resolve(converter);
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
