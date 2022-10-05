package com.github.leeonky.dal;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.JavaClassPropertyAccessor;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.Assertions.expect;
import static com.github.leeonky.util.BeanClass.create;

public class MetaThrowTest {

    @Test
    void should_catch_exception_thrown_in_property_accessor() {
        DAL dal = new DAL().extend();
        RuntimeContextBuilder builder = dal.getRuntimeContextBuilder();
        builder.registerPropertyAccessor(Bean.class, new JavaClassPropertyAccessor<Bean>(create(Bean.class)) {
            @Override
            public Object getValueByData(Data data, Object property) {
                throw new java.lang.RuntimeException("hello");
            }
        });

        expect(new Bean()).use(dal).should("any::throw.message= hello");
    }

    @Test
    void should_catch_exception_thrown_in_method() {
        expect(new Bean()).should("raise::throw.message= raise-error");
    }

    public static class Bean {
        public Object raise() {
            throw new java.lang.RuntimeException("raise-error");
        }
    }
}
