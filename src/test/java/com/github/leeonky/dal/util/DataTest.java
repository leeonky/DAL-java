package com.github.leeonky.dal.util;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.cucumber.JSONArrayDALCollectionFactory;
import com.github.leeonky.dal.cucumber.JSONObjectAccessor;
import com.github.leeonky.dal.runtime.*;
import com.github.leeonky.dal.type.FieldAlias;
import com.github.leeonky.dal.type.FieldAliases;
import com.github.leeonky.dal.type.Partial;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.Converter;
import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import lombok.experimental.Accessors;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Set;
import java.util.stream.Stream;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class DataTest {
    private final Converter converter = Converter.getInstance();

    @Test
    void check_null_for_customer_schema() {
        RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder().registerPropertyAccessor(AlwaysNull.class, new PropertyAccessor<AlwaysNull>() {
            @Override
            public Object getValue(AlwaysNull instance, Object name) {
                return null;
            }

            @Override
            public Set<Object> getPropertyNames(AlwaysNull instance) {
                return null;
            }

            @Override
            public boolean isNull(AlwaysNull instance) {
                return true;
            }
        });

        assertTrue(runtimeContextBuilder.build(null).wrap(new AlwaysNull()).isNull());
        assertTrue(runtimeContextBuilder.build(null).wrap(null).isNull());
        assertFalse(runtimeContextBuilder.build(null).wrap(new Object()).isNull());
    }

    private static class AlwaysNull {
    }

    @Getter
    @Setter
    @Accessors(chain = true)
    public static class Bean {
        private int intValue;
    }

    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfAge", field = "age"),
    })
    public static class User {
    }

    @Nested
    class GetPropertyOrIndexValue {
        RuntimeContextBuilder runtimeContextBuilder = new DAL().extend().getRuntimeContextBuilder()
                .registerPropertyAccessor(JSONObject.class, new JSONObjectAccessor())
                .registerDALCollectionFactory(JSONArray.class, new JSONArrayDALCollectionFactory());

        @Test
        void access_java_class_property() {
            assertDataAccess(new Bean().setIntValue(1), 1, "intValue");
        }

        @Test
        void access_map_element() {
            assertDataAccess(new HashMap<String, Object>() {{
                put("intValue", 1);
            }}, 1, "intValue");
        }

        @SneakyThrows
        @Test
        void access_via_property_accessor() {
            assertDataAccess(new JSONObject().put("intValue", 1), 1, "intValue");
        }

        @Test
        void get_list_size() {
            assertListSize(emptyList(), 0);
            assertListSize(new String[]{"a"}, 1);
            assertListSize(new JSONArray().put(100).put(200), 2);
        }

        @Test
        void get_list_element_from_array() {
            assertDataAccess(new String[]{"a", "b"}, "b", 1);
        }

        @Test
        void get_list_element_from_iterable() {
            assertDataAccess(asList("a", "b"), "b", 1);
        }

        @Test
        void get_list_element_from_accessor() {
            assertDataAccess(new JSONArray().put("a").put("b"), "b", 1);
        }

        @Test
        void should_raise_error_when_index_out_of_range() {
            assertThrows(PropertyAccessException.class, () ->
                    runtimeContextBuilder.build(null).wrap(new String[0]).getValue(0));
        }

        @Test
        void support_get_value_via_property_chain() {
            assertDataAccess(new JSONArray().put("a").put(new Bean().setIntValue(100)), 100, 1, "intValue");
        }

        @Test
        void support_invoke_bean_no_args_method() {
            assertDataAccess(new Bean().setIntValue(100), 100, "getIntValue");
        }

        @Test
        void support_stream_size_as_list() {
            assertListSize(Stream.of(1, 2), 2);
        }

        @Test
        void support_get_value_via_field_alias() {
            assertThat(new Data(new HashMap<String, Object>() {{
                put("age", 100);
            }}, runtimeContextBuilder.build(null), SchemaType.create(BeanClass.create(User.class)))
                    .getValue("aliasOfAge").instance()).isEqualTo(100);
        }

        private void assertDataAccess(Object object, Object expected, Object... properties) {
            assertThat(runtimeContextBuilder.build(null).wrap(object).getValue(asList(properties)).instance())
                    .isEqualTo(expected);
        }

        private void assertListSize(Object object, int size) {
            assertThat(runtimeContextBuilder.build(null).wrap(object).list().size()).isEqualTo(size);
        }
    }

    @Nested
    class DumpData {
        private RuntimeContextBuilder.DALRuntimeContext build = new RuntimeContextBuilder().build(null);

        @Test
        void dump_null_value() {
            assertThat(build.wrap(null).dumpValue()).isEqualTo("null");
        }
    }

    @Nested
    class CurringMethodArgs {
        private RuntimeContextBuilder.DALRuntimeContext build = new RuntimeContextBuilder().build(null);

        @Test
        void return_null_when_property_is_not_string() {
            Data data = build.wrap(new Object());

            assertThat(data.currying(1)).isEmpty();
        }

        @Test
        void return_currying_method_with_property() {
            Data data = build.wrap(new Currying());

            assertThat(data.currying("currying1").get().call("hello").resolve()).isEqualTo("hello");
        }

        @Test
        void currying_of_currying() {
            Data data = build.wrap(new Currying());
            CurryingMethod currying = data.currying("currying2").get();

            assertThat(((CurryingMethod) currying.call(2).resolve()).call("hello").resolve()).isEqualTo("hello2");
        }

        @Test
        void should_choose_min_parameter_size_method() {
            Data data = build.wrap(new Currying());
            CurryingMethod currying = data.currying("overrideMethod").get();

            assertThat(currying.call(2).resolve()).isEqualTo(2);
        }
    }

    @Nested
    class StaticCurringMethodArgs {
        private RuntimeContextBuilder.DALRuntimeContext build = new RuntimeContextBuilder()
                .registerStaticMethodExtension(StaticMethod.class).build(null);

        @Test
        void return_currying_method_with_property() {
            Data data = build.wrap(new Currying());

            assertThat(data.currying("staticCurrying1").get().call("hello").resolve()).isEqualTo("hello");
        }

        @Test
        void return_currying_method_with_property_in_super_instance_type() {
            Data data = build.wrap(new Currying());

            assertThat(data.currying("baseMatchCurrying").get().call("hello").resolve()).isEqualTo("hello");
        }

        @Test
        void currying_of_currying() {
            Data data = build.wrap(new Currying());
            CurryingMethod currying = data.currying("staticCurrying2").get();

            assertThat(((CurryingMethod) currying.call(2).resolve()).call("hello").resolve()).isEqualTo("hello2");
        }

        @Test
        void should_choose_min_parameter_size_method() {
            Data data = build.wrap(new Currying());
            CurryingMethod currying = data.currying("staticOverrideMethod").get();

            assertThat(currying.call(2).resolve()).isEqualTo(2);
        }

        @Test
        void use_same_instance_type_first_when_more_than_one_candidate() {
            Data data = build.wrap(new Currying());
            CurryingMethod currying = data.currying("baseCurrying").get();

            assertThat(currying.call("a").resolve()).isEqualTo("A");
        }
    }

    public static class BaseCurrying {

    }

    public static class Currying extends BaseCurrying {
        public Object unexpected(String str) {
            return null;
        }

        public Object currying1(String str) {
            return str;
        }

        public Object currying2(int i, String str) {
            return str + i;
        }

        public Object overrideMethod(int i, String str) {
            return str + i;
        }

        public Object overrideMethod(int i) {
            return i;
        }

        public Object invalidCurrying(String str) {
            return null;
        }

        public Object invalidCurrying(int str) {
            return null;
        }
    }

    public static class StaticMethod {
        public static Object staticCurrying1(Currying currying, String str) {
            return str;
        }

        public static Object staticCurrying2(Currying currying, int i, String str) {
            return str + i;
        }

        public static Object staticOverrideMethod(Currying currying, int i, String str) {
            return str + i;
        }

        public static Object staticOverrideMethod(Currying currying, int i) {
            return i;
        }

        public static Object baseCurrying(Currying currying, String str) {
            return str.toUpperCase();
        }

        public static Object baseCurrying(BaseCurrying currying, String str) {
            return str;
        }

        public static Object invalidStaticCurrying(Currying currying, String str) {
            return null;
        }

        public static Object invalidStaticCurrying(Currying currying, int str) {
            return null;
        }

        public static Object baseMatchCurrying(BaseCurrying currying, String str) {
            return str;
        }
    }
}
