package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.NoSuchAccessorException;

import java.util.Objects;
import java.util.Set;

public class JavaClassPropertyAccessor<T> implements PropertyAccessor<T> {
    private final RuntimeContextBuilder runtimeContextBuilder;
    private final BeanClass<T> beanClass;

    public JavaClassPropertyAccessor(RuntimeContextBuilder runtimeContextBuilder, BeanClass<T> type) {
        this.runtimeContextBuilder = runtimeContextBuilder;
        beanClass = type;
    }

    @Override
    public Object getValue(T instance, String name) {
        try {
            return beanClass.getPropertyValue(instance, name);
        } catch (NoSuchAccessorException ignore) {
            try {
                return beanClass.getType().getMethod(name).invoke(instance);
            } catch (IllegalAccessException | NoSuchMethodException e) {
                return runtimeContextBuilder.invokeExtensionMethod(instance, name);
            } catch (Exception e) {
                throw new IllegalStateException(e);
            }
        }
    }

    @Override
    public Set<String> getPropertyNames(Object instance) {
        return beanClass.getPropertyReaders().keySet();
    }

    @Override
    public boolean isNull(Object instance) {
        return Objects.equals(instance, null);
    }
}
