package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.ListAccessor;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.RuntimeException;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;

import static com.github.leeonky.dal.ast.InputNode.INSTANCE;
import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static java.util.Collections.emptyList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class PropertyNodeTest {

    @Test
    void support_first_index_of_list() {
        RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext = new RuntimeContextBuilder()
                .registerListAccessor(ArrayList.class, new ListAccessor<ArrayList<?>>() {
                    @Override
                    public Iterable<?> toIterable(ArrayList<?> instance) {
                        return instance;
                    }

                    @Override
                    public int firstIndex() {
                        return 1;
                    }
                }).build(new ArrayList<>(Arrays.asList(1, 2)));

        assertThat(new PropertyNode(INSTANCE, 1, BRACKET).evaluate(DALRuntimeContext)).isEqualTo(1);
        assertThat(new PropertyNode(INSTANCE, -1, BRACKET).evaluate(DALRuntimeContext)).isEqualTo(2);
    }

    public static class CustomizedList {
        public int value = 100;

        public boolean isEmpty() {
            return true;
        }
    }

    public static class CustomizedListStaticExtensionMethod {
        public static String method(CustomizedList customizedList) {
            return "extension";
        }
    }

    @Test
    void access_customized_list_property() {
        RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext = new RuntimeContextBuilder()
                .registerListAccessor(CustomizedList.class, instance -> emptyList())
                .build(new CustomizedList());

        assertThat(new PropertyNode(INSTANCE, "value", BRACKET).evaluate(DALRuntimeContext)).isEqualTo(100);
    }

    @Test
    void access_customized_list_method() {
        RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext = new RuntimeContextBuilder()
                .registerListAccessor(CustomizedList.class, instance -> emptyList())
                .build(new CustomizedList());

        assertThat(new PropertyNode(INSTANCE, "isEmpty", BRACKET).evaluate(DALRuntimeContext)).isEqualTo(true);
    }

    @Test
    void access_customized_list_static_extension_method() {
        RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext = new RuntimeContextBuilder()
                .registerListAccessor(CustomizedList.class, instance -> emptyList())
                .registerStaticMethodExtension(CustomizedListStaticExtensionMethod.class)
                .build(new CustomizedList());

        assertThat(new PropertyNode(INSTANCE, "method", BRACKET).evaluate(DALRuntimeContext)).isEqualTo("extension");
    }

    public static class BaseBean {
    }

    public static class Bean extends BaseBean {
    }

    public static class BeanMethods {
        public static int getIntFromBean(Bean bean) {
            return 100;
        }

        public static int getIntFromBase(BaseBean bean) {
            return 200;
        }

        public static int getInt(Bean bean) {
            return 300;
        }

        public static int getInt(BaseBean bean) {
            return 400;
        }
    }

    public static class SameStaticBeanMethods {
        public static int getInt(Bean bean) {
            return 300;
        }
    }

    @Nested
    class StaticMethodExtension {
        RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext = new RuntimeContextBuilder()
                .registerStaticMethodExtension(BeanMethods.class)
                .build(new Bean());

        @Test
        void support_static_method_extension() {
            assertThat(new PropertyNode(INSTANCE, "getIntFromBean", BRACKET).evaluate(DALRuntimeContext)).isEqualTo(100);
        }

        @Test
        void invoke_from_base_instance() {

            assertThat(new PropertyNode(INSTANCE, "getIntFromBase", BRACKET).evaluate(DALRuntimeContext)).isEqualTo(200);
        }

        @Test
        void should_invoke_by_instance_type() {
            assertThat(new PropertyNode(INSTANCE, "getInt", BRACKET).evaluate(DALRuntimeContext)).isEqualTo(300);
        }

        @Test
        void raise_error_when_more_than_one_method() {
            RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext = new RuntimeContextBuilder()
                    .registerStaticMethodExtension(BeanMethods.class)
                    .registerStaticMethodExtension(SameStaticBeanMethods.class)
                    .build(new Bean());

            assertThrows(RuntimeException.class, () -> new PropertyNode(INSTANCE, "getInt", BRACKET)
                    .evaluate(DALRuntimeContext));
        }
    }
}
