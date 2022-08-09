package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.ConvertException;
import com.github.leeonky.util.Converter;

import java.lang.reflect.Parameter;

import static com.github.leeonky.util.NumberType.boxedClass;

class ParameterValue {
    private final Parameter parameter;
    private final Object value;

    public ParameterValue(Parameter parameter, Object value) {
        this.parameter = parameter;
        this.value = value;
    }

    public boolean isSameType() {
        return value != null && boxedClass(value.getClass()).equals(boxedClass(parameter.getType()));
    }

    public boolean isSuperType() {
        return value != null && boxedClass(parameter.getType()).isInstance(value);
    }

    boolean isConvertibleType(Converter converter) {
        try {
            converter.convert(parameter.getType(), value);
            return true;
        } catch (ConvertException ignore) {
            return false;
        }
    }

    Object getArg(Converter converter) {
        return converter.convert(parameter.getType(), value);
    }
}
