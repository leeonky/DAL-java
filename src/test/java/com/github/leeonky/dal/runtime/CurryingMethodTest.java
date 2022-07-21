package com.github.leeonky.dal.runtime;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.List;

import static com.github.leeonky.util.BeanClass.getConverter;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

class CurryingMethodTest {

    @Nested
    class GetArgRanges {
        private RuntimeContextBuilder.DALRuntimeContext build = new RuntimeContextBuilder()
                .registerStaticMethodExtension(StaticMethod.class).build(null);

        @Test
        void get_range_from_instance_method() {
            Currying instance = new Currying();
            Data data = build.wrap(instance);
            List list = mock(List.class);

            assertThat(((CurryingMethod) data.currying("instanceMethod").get().call("arg1", getConverter())).fetchArgRange((o, objects) -> {
                assertThat(o).isSameAs(instance);
                assertThat(objects).containsExactly("arg1");
                return list;
            })).isSameAs(list);
        }

        @Test
        void get_range_from_static_method() {
            Currying instance = new Currying();
            Data data = build.wrap(instance);
            List list = mock(List.class);

            assertThat(((CurryingMethod) data.currying("staticMethod").get().call("arg1", getConverter())).fetchArgRange((o, objects) -> {
                assertThat(o).isSameAs(instance);
                assertThat(objects).containsExactly("arg1");
                return list;
            })).isSameAs(list);
        }
    }

    public static class Currying {
        public String instanceMethod(String str, String str2) {
            return str;
        }
    }

    public static class StaticMethod {
        public static String staticMethod(Currying currying, String str, String str2) {
            return str;
        }
    }
}