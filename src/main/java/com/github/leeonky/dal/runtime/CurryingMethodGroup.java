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
        Optional<InstanceCurryingMethod> instanceCurryingMethod = curryingMethods.stream()
                .filter(curryingMethod -> curryingMethod.allParamsTypeMatches(converter)).findFirst();
        if (instanceCurryingMethod.isPresent())
            return instanceCurryingMethod.get().resolve(converter);
//        TODO  **********************
        Object resolve = curryingMethods.get(0).resolve(converter);
        return resolve instanceof CurryingMethod ? this : resolve;
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
