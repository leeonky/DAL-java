package com.github.leeonky.dal.e2e;

import com.github.leeonky.dal.format.PositiveInteger;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.util.PropertyAccessor;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import static java.util.Arrays.asList;

class VerifySchema extends Base {

    public static final Set<String> PROPERTY_NAMES = new HashSet<>(asList("f1", "f2"));

    @Test
    void should_support_register_customer_object_type() throws JSONException {
        dataAssert.getCompilingContextBuilder().registerPropertyAccessor(JSONObject.class, new PropertyAccessor<JSONObject>() {
            @Override
            public Object getValue(JSONObject instance, String name) {
                return "mocked return value of " + name;
            }

            @Override
            public Set<String> getPropertyNames(JSONObject instance) {
                return PROPERTY_NAMES;
            }
        });

        dataAssert.getCompilingContextBuilder().registerSchema("Bean",
                wrappedObject -> wrappedObject.getPropertyReaderNames() == PROPERTY_NAMES);

        assertPass(new JSONObject("{\"f1\": 1, \"f2\": 1}"), "is Bean which .f1='mocked return value of f1'");
        assertPass(null, "= null");
        assertPass(JSONObject.NULL, "= null");
    }

    public static class RightFieldAndType {
        public PositiveInteger id;
    }

    public static class AllowNullField {
        @AllowNull
        public PositiveInteger id;
    }

    public static class NestedType {
        public RightFieldAndType type;
    }

    @Nested
    class RegisterSchemaType {
        @BeforeEach
        void registerJson() {
            dataAssert.getCompilingContextBuilder().registerPropertyAccessor(JSONObject.class, new PropertyAccessor<JSONObject>() {
                @Override
                public Object getValue(JSONObject instance, String name) {
                    try {
                        return instance.get(name);
                    } catch (JSONException e) {
                        return JSONObject.NULL;
                    }
                }

                @Override
                public Set<String> getPropertyNames(JSONObject instance) {
                    Set<String> set = new HashSet<>();
                    Iterator iterator = instance.keys();
                    while (iterator.hasNext())
                        set.add(iterator.next().toString());
                    return set;
                }
            });
            dataAssert.getCompilingContextBuilder()
                    .registerSchema(RightFieldAndType.class)
                    .registerSchema(AllowNullField.class)
                    .registerSchema(NestedType.class);
        }

        @Test
        void should_verify_fields_and_type() throws JSONException {
            assertPass(new JSONObject("{\"id\": 1}"), "is RightFieldAndType");
        }

        @Test
        void has_an_unexpected_field() throws JSONException {
            assertFailed(new JSONObject("{\"id\": 1, \"unexpected field\":2}"), "is RightFieldAndType");
        }

        @Test
        void missing_field() throws JSONException {
            assertFailed(new JSONObject("{}"), "is RightFieldAndType");
        }

        @Test
        void field_has_incorrect_type() throws JSONException {
            assertFailed(new JSONObject("{\"id\": 0}"), "is RightFieldAndType");
        }

        @Test
        void should_allow_null_on_field() throws JSONException {
            assertPass(new JSONObject("{}"), "is AllowNullField");
            assertPass(new JSONObject("{\"id\": null}"), "is AllowNullField");
        }

        @Test
        void should_verify_nested_schema() throws JSONException {
            assertPass(new JSONObject("{\"type\":{\"id\": 1}}"), "is NestedType");
            assertFailed(new JSONObject("{\"type\":{\"id\": 0}}"), "is NestedType");
        }
        //TODO list, map, nested list, nested map, polymorphic, polymorphic list
    }
}
