package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.NoSuchAccessorException;

import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

import static java.lang.String.format;

public class JavaClassPropertyAccessor<T> implements PropertyAccessor<T> {
    protected final RuntimeContextBuilder runtimeContextBuilder;
    private final BeanClass<T> beanClass;

    public JavaClassPropertyAccessor(RuntimeContextBuilder runtimeContextBuilder, BeanClass<T> type) {
        this.runtimeContextBuilder = runtimeContextBuilder;
        beanClass = type;
    }

    @Override
    public Object getValue(T instance, Object property) {
        try {
            return beanClass.getPropertyValue(instance, (String) property);
        } catch (NoSuchAccessorException ignore) {
            throw new InvalidPropertyException(format("Method or property `%s` does not exist in `%s`", property, instance.getClass().getName()));
        }
    }

    @Override
    public Set<Object> getPropertyNames(T instance) {
        return new LinkedHashSet<>(beanClass.getPropertyReaders().keySet());
    }

    @Override
    public boolean isNull(T instance) {
        return Objects.equals(instance, null);
    }
}
