package com.github.leeonky.dal.e2e;

import com.github.leeonky.dal.format.PositiveInteger;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.util.ListAccessor;
import com.github.leeonky.dal.util.PropertyAccessor;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.*;

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

    public static class NestedList {
        public List<RightFieldAndType> list;
    }

    public static class NestedNestedList {
        public List<List<RightFieldAndType>> list;
    }

    public static class NestedMap {
        public Map<String, RightFieldAndType> map;
    }

    public static class NestedNestedMap {
        public Map<String, Map<String, RightFieldAndType>> map;
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
            }).registerListAccessor(JSONArray.class, new ListAccessor<JSONArray>() {
                @Override
                public Object get(JSONArray jsonArray, int index) {
                    try {
                        return jsonArray.get(index);
                    } catch (JSONException e) {
                        throw new IllegalStateException(e);
                    }
                }

                @Override
                public int size(JSONArray jsonArray) {
                    return jsonArray.length();
                }
            })
                    .registerSchema(RightFieldAndType.class)
                    .registerSchema(AllowNullField.class)
                    .registerSchema(NestedType.class)
                    .registerSchema(NestedList.class)
                    .registerSchema(NestedNestedList.class)
                    .registerSchema(NestedMap.class)
                    .registerSchema(NestedNestedMap.class)
            ;
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

        @Test
        void should_support_verify_nested_list_schema() throws JSONException {
            assertPass(new JSONObject("{\"list\": [{\"id\": 1}]}"), "is NestedList");
            assertFailed(new JSONObject("{\"list\": [{\"id\": 0}]}"), "is NestedList");
        }

        @Test
        void should_support_verify_nested_nested_list_schema() throws JSONException {
            assertPass(new JSONObject("{\"list\": [[{\"id\": 1}]]}"), "is NestedNestedList");
            assertFailed(new JSONObject("{\"list\": [[{\"id\": 0}]]}"), "is NestedNestedList");
        }

        @Test
        void should_support_verify_nested_map_schema() throws JSONException {
            assertPass(new JSONObject("{\"map\": {\"str\": {\"id\": 1}}}"), "is NestedMap");
            assertFailed(new JSONObject("{\"map\": {\"str\": {\"id\": 0}}}"), "is NestedMap");

            assertPass(new JSONObject("{\"map\": {\"str\": {\"str\": {\"id\": 1}}}}"), "is NestedNestedMap");
            assertFailed(new JSONObject("{\"map\": {\"str\": {\"str\": {\"id\": 0}}}}"), "is NestedNestedMap");
        }
        //TODO polymorphic, polymorphic list
    }
}
