package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.format.PositiveInteger;
import com.github.leeonky.dal.format.URL;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.type.SubTypeViaString;
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
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

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

    @SubTypeViaString(property = "type", types = {
            @SubTypeViaString.Type(value = "V1", type = V1.class),
            @SubTypeViaString.Type(value = "V2", type = V2.class)
    })
    public static abstract class Abstract {
        public String type;
    }

    public static abstract class V1 extends Abstract {
        public PositiveInteger id;
    }

    public static abstract class V2 extends Abstract {
        public URL url;
    }

    public static class V {
        public Abstract v;
    }

    public static class VList {
        public List<Abstract> vs;
    }

    public static class NestedSubType {
        public String type;
    }

    @SubTypeViaString(property = "type.type", types = {
            @SubTypeViaString.Type(value = "V1", type = NestedV1.class),
            @SubTypeViaString.Type(value = "V2", type = NestedV2.class)
    })
    public static class NestedAbstract {
        public NestedSubType type;
    }

    public static class NestedV1 extends NestedAbstract {
        public PositiveInteger id;
    }

    public static class NestedV2 extends NestedAbstract {
        public URL url;
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
                    .registerSchema(Abstract.class)
                    .registerSchema(V.class)
                    .registerSchema(VList.class)
                    .registerSchema(NestedAbstract.class)
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

        @Test
        void should_match_specific_sub_schema() throws JSONException {
            assertPass(new JSONObject("{\"v\": {\"id\": 1, \"type\": \"V1\"}}"), "is V");
            assertPass(new JSONObject("{\"v\": {\"url\": \"http://www.google.com\", \"type\": \"V2\"}}"), "is V");

            assertFailed(new JSONObject("{\"v\": {\"id\": 1, \"type\": V2}}"), "is V");
            assertFailed(new JSONObject("{\"v\": {\"id\": 0, \"type\": V1}}"), "is V");
        }

        @Test
        void should_match_specific_sub_schema_in_list() throws JSONException {
            assertPass(new JSONObject("{\"vs\": [{\"id\": 1, \"type\": \"V1\"}]}"), "is VList");
            assertPass(new JSONObject("{\"vs\": [{\"url\": \"http://www.google.com\", \"type\": \"V2\"}]}"), "is VList");

            assertFailed(new JSONObject("{\"vs\": [{\"id\": 1, \"type\": V2}]}"), "is VList");
            assertFailed(new JSONObject("{\"vs\": [{\"id\": 0, \"type\": V1}]}"), "is VList");
        }

        @Test
        void should_match_specific_sub_schema_through_nested_property() throws JSONException {
            assertPass(new JSONObject("{\"type\": {\"type\": \"V1\"}, \"id\": 1}"), "is NestedAbstract");
            assertPass(new JSONObject("{\"type\": {\"type\": \"V2\"}, \"url\": \"http://www.google.com\"}"), "is NestedAbstract");
        }

        @Test
        void should_error_when_no_type_property() {
            RuntimeException runtimeException = assertThrows(RuntimeException.class,
                    () -> dataAssert.assertData(new JSONObject("{\"v\": {\"id\": 1}}"), "is V"));
            assertThat(runtimeException).hasMessage("Cannot guess sub type through property type value[null]");
        }

        @Test
        void should_error_when_no_matched_subtype() {
            RuntimeException runtimeException = assertThrows(RuntimeException.class,
                    () -> dataAssert.assertData(new JSONObject("{\"v\": {\"id\": 1, \"type\": V3}}"), "is V"));
            assertThat(runtimeException).hasMessage("Cannot guess sub type through property type value[V3]");
        }
    }
}

