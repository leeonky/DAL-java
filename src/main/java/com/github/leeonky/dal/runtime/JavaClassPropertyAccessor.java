package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.NoSuchAccessorException;

import java.util.LinkedHashSet;
import java.util.Set;

import static java.lang.String.format;

public class JavaClassPropertyAccessor<T> implements PropertyAccessor<T> {
    private final BeanClass<T> beanClass;

    public JavaClassPropertyAccessor(BeanClass<T> type) {
        beanClass = type;
    }

    @Override
    public Object getValue(T instance, Object property) {
        try {
            return beanClass.getPropertyValue(instance, (String) property);
        } catch (NoSuchAccessorException ignore) {
            throw new InvalidPropertyException(format("Method or property `%s` does not exist in `%s`", property,
                    instance.getClass().getName()));
        }
    }

    @Override
    public Set<Object> getPropertyNames(T instance) {
        return new LinkedHashSet<>(beanClass.getPropertyReaders().keySet());
    }
}
