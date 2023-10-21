package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.*;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.NoSuchAccessorException;

import java.util.Set;

import static com.github.leeonky.dal.runtime.Order.BUILD_IN;
import static java.lang.String.format;

@Order(BUILD_IN)
public class MetaProperties implements Extension {
    private static Object size(MetaData metaData) {
        Data data = metaData.getData();
        if (data.isList())
            return data.sizeOfList();
        throw new IllegalStateException(format("Invalid meta property `size` for: %s", data.dumpAll()));
    }

    private static Object throw_(MetaData metaData) {
        Throwable error = metaData.catchError();
        if (error == null)
            throw new AssertionFailure("Expecting an error to be thrown, but nothing was thrown",
                    metaData.getSymbolNode().getPositionBegin());
        return error;
    }

    private static Object object_(MetaData metaData) {
        return metaData.getData().getInstance() == null ? null : new OriginalJavaObject(metaData.getData());
    }

    private static Object keys(MetaData metaData) {
        return metaData.getData().getFieldNames();
    }

    @Override
    public void extend(DAL dal) {
        dal.getRuntimeContextBuilder()
                .registerMetaProperty("size", MetaProperties::size)
                .registerMetaProperty("throw", MetaProperties::throw_)
                .registerMetaProperty("object", MetaProperties::object_)
                .registerMetaProperty("keys", MetaProperties::keys)
                .registerPropertyAccessor(OriginalJavaObject.class, new PropertyAccessor<OriginalJavaObject>() {
                    @Override
                    public Object getValue(OriginalJavaObject javaObject, Object property) {
                        return javaObject.getValue(property);
                    }

                    @Override
                    public Set<Object> getPropertyNames(OriginalJavaObject javaObject) {
                        return null;
                    }

                    @Override
                    public boolean isNull(OriginalJavaObject instance) {
                        return false;
                    }
                })
        ;
    }

    static class OriginalJavaObject {
        private final Data data;

        public OriginalJavaObject(Data data) {
            this.data = data;
        }

        public Object getValue(Object property) {
            try {
                Object instance = data.getInstance();
                return BeanClass.createFrom(instance).getPropertyValue(instance, property.toString());
            } catch (NoSuchAccessorException ignore) {
                return data.getValue(property).getInstance();
            }
        }
    }
}
