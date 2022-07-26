package com.github.leeonky.dal.runtime;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.List;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;

class CurryingMethodTest {

    @Nested
    class GetArgRanges {
        RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder().registerStaticMethodExtension(StaticMethod.class);

        @Test
        void get_range_from_instance_method() {
            Currying instance = new Currying();
            List<Object> list = asList("fake", "range");

            runtimeContextBuilder.registerCurryingMethodRange(Currying.class, "instanceMethod", (o, objects) -> {
                assertThat(o).isSameAs(instance);
                assertThat(objects).containsExactly("arg1");
                return list;
            });

            Data data = runtimeContextBuilder.build(null).wrap(instance);

            assertThat(data.getValue("instanceMethod").getValue("arg1").getFieldNames()).containsExactly("fake", "range");
        }

        @Test
        void get_range_from_static_method() {
            Currying instance = new Currying();
            List<Object> list = asList("fake", "range");

            runtimeContextBuilder.registerCurryingMethodRange(Currying.class, "staticMethod", (o, objects) -> {
                assertThat(o).isSameAs(instance);
                assertThat(objects).containsExactly("arg1");
                return list;
            });

            Data data = runtimeContextBuilder.build(null).wrap(instance);

            assertThat(data.getValue("staticMethod").getValue("arg1").getFieldNames()).containsExactly("fake", "range");
        }

        @Test
        void currying_method_to_string() {
            String str = "hello";
            assertThat(runtimeContextBuilder.build(null).wrap(str).currying("charAt").get().toString()).isEqualTo("instance: hello\n" +
                    "method: public char java.lang.String.charAt(int)\n" +
                    "args: []");
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