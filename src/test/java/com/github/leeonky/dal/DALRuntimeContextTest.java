package com.github.leeonky.dal;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.ListAccessor;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.SchemaType;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
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

    @Nested
    class IsList {
        private final RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder();

        @Test
        void return_false_when_checking_return_false() {
            runtimeContextBuilder.registerListAccessor(DynamicList.class, new ListAccessor<DynamicList>() {
                @Override
                public Iterable<?> toIterable(DynamicList instance) {
                    return null;
                }

                @Override
                public boolean isList(DynamicList instance) {
                    return instance.isList();
                }
            });
            RuntimeContextBuilder.DALRuntimeContext context = runtimeContextBuilder.build(null);

            assertThat(context.isRegisteredList(new DynamicList().setList(true))).isTrue();
            assertThat(context.isRegisteredList(new DynamicList().setList(false))).isFalse();
        }
    }

    @Getter
    @Setter
    @Accessors(chain = true)
    public static class DynamicList {
        private boolean list;
    }

    public static class Obj {

    }

    public static class StaticMethods {

        public static String objString(String string) {
            return string;
        }
    }
}