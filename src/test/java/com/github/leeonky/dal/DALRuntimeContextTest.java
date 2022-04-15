package com.github.leeonky.dal;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.SchemaType;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertThrows;

class DALRuntimeContextTest {

    @Test
    void should_raise_error_when_property_accessor_not_register() {
        RuntimeContextBuilder.DALRuntimeContext runtimeContext = new RuntimeContextBuilder().build(null);
        assertThrows(IllegalArgumentException.class, () -> runtimeContext.getPropertyValue(new Data(new Object(),
                new RuntimeContextBuilder().build(null), SchemaType.createRoot()), "anyc"));
    }

    @Test
    void invoke_static_extended_method_through_object_implicit_data() {
        DAL dal = new DAL();
        RuntimeContextBuilder runtimeContextBuilder = dal.getRuntimeContextBuilder();
        runtimeContextBuilder.registerStaticMethodExtension(StaticMethods.class);
        runtimeContextBuilder.registerImplicitData(Obj.class, obj -> "obj-string");

        org.assertj.core.api.Assertions.assertThat((Object) dal.evaluate(new Obj(), "objString")).isEqualTo("obj-string");
    }


    public static class Obj {

    }

    public static class StaticMethods {

        public static String objString(String string) {
            return string;
        }
    }
}