package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.cucumber.JSONArrayDALCollectionFactory;
import com.github.leeonky.dal.cucumber.JSONObjectAccessor;
import com.github.leeonky.dal.format.Formatters;
import com.github.leeonky.dal.runtime.DalException;
import com.github.leeonky.dal.runtime.PropertyAccessor;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.type.Partial;
import com.github.leeonky.dal.type.Schema;
import com.github.leeonky.dal.type.SubType;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.github.leeonky.dal.runtime.NameStrategy.SIMPLE_NAME_WITH_PARENT;
import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class VerifySchema extends Base {

    private static final Set<Object> PROPERTY_NAMES = new HashSet<>(asList("f1", "f2"));

    @Test
    void should_support_register_customer_object_type() throws JSONException {
        dal.getRuntimeContextBuilder().registerPropertyAccessor(JSONObject.class, new PropertyAccessor<JSONObject>() {
            @Override
            public Object getValue(JSONObject instance, Object name) {
                return "mocked return value of " + name;
            }

            @Override
            public Set<Object> getPropertyNames(JSONObject instance) {
                return PROPERTY_NAMES;
            }

            @Override
            public boolean isNull(JSONObject instance) {
                return instance == null || instance.equals(JSONObject.NULL);
            }
        });

        dal.getRuntimeContextBuilder().registerSchema("Bean", (data, context) -> data.fieldNames() == PROPERTY_NAMES);

        assertPass(new JSONObject("{\"f1\": 1, \"f2\": 1}"), "is Bean which .f1='mocked return value of f1'");
        assertPass(null, "= null");
        assertPass(JSONObject.NULL, "= null");
    }

    public static class RightFieldAndType implements Schema {
        public Formatters.PositiveInteger id;
    }

    @Partial
    public static class IgnoreUnexpectedField implements Schema {
        public Formatters.PositiveInteger id;
    }

    public static class AllowNullField implements Schema {
        @AllowNull
        public Formatters.PositiveInteger id;
    }

    public static class NestedType implements Schema {
        public RightFieldAndType type;
    }

    public static class NestedList implements Schema {
        public List<RightFieldAndType> list;
    }

    public static class NestedArray implements Schema {
        public RightFieldAndType[] array;
    }

    public static class NestedNestedList implements Schema {
        public List<List<RightFieldAndType>> list;
    }

    public static class NestedMap implements Schema {
        public Map<String, RightFieldAndType> map;
    }

    public static class NestedNestedMap implements Schema {
        public Map<String, Map<String, RightFieldAndType>> map;
    }

    @Getter
    @Setter
    @Accessors(chain = true)
    public static class ClassObject {
        private int id;
    }

    @SubType(property = "type", types = {
            @SubType.Type(value = "V1", type = V1.class),
            @SubType.Type(value = "V2", type = V2.class)
    })
    public static abstract class Abstract implements Schema {
        public String type;
    }

    public static class V1 extends Abstract implements Schema {
        public Formatters.PositiveInteger id;
    }

    public static class V2 extends Abstract implements Schema {
        public Formatters.URL url;
    }

    public static class V implements Schema {
        public Abstract v;
    }

    public static class VList implements Schema {
        public List<Abstract> vs;
    }

    public static class NestedSubType implements Schema {
        public String type;
    }

    @SubType(property = "type.type", types = {
            @SubType.Type(value = "V1", type = NestedV1.class),
            @SubType.Type(value = "V2", type = NestedV2.class)
    })
    public static class NestedAbstract implements Schema {
        public NestedSubType type;
    }

    public static class NestedV1 extends NestedAbstract implements Schema {
        public Formatters.PositiveInteger id;
    }

    public static class NestedV2 extends NestedAbstract implements Schema {
        public Formatters.URL url;
    }

    public static class FieldValue implements Schema {
        public Formatters.Number integer;

        public FieldValue() {
        }

        public FieldValue(int i) {
            integer = Formatters.Number.equalTo(i);
        }
    }

    public static class SchemaWithInstance implements Schema {
        public FieldValue fieldValue = new FieldValue(1);
    }

    @Nested
    class RegisterSchemaType {
        @BeforeEach
        void registerJson() {
            dal.getRuntimeContextBuilder()
                    .registerPropertyAccessor(JSONObject.class, new JSONObjectAccessor())
                    .registerDALCollectionFactory(JSONArray.class, new JSONArrayDALCollectionFactory())
                    .registerSchema(RightFieldAndType.class)
                    .registerSchema(AllowNullField.class)
                    .registerSchema(NestedType.class)
                    .registerSchema(NestedList.class)
                    .registerSchema(NestedArray.class)
                    .registerSchema(NestedNestedList.class)
                    .registerSchema(NestedMap.class)
                    .registerSchema(NestedNestedMap.class)
                    .registerSchema(Abstract.class)
                    .registerSchema(V.class)
                    .registerSchema(VList.class)
                    .registerSchema(NestedAbstract.class)
                    .registerSchema(NestedSubType.class)
            ;
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
        void should_support_verify_nested_array_schema() throws JSONException {
            assertPass(new JSONObject("{\"array\": [{\"id\": 1}]}"), "is NestedArray");
            assertFailed(new JSONObject("{\"array\": [{\"id\": 0}]}"), "is NestedArray");
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
                    () -> dal.evaluate(new JSONObject("{\"v\": {\"id\": 1}}"), "is V"));
            assertThat(runtimeException).hasMessage("Cannot guess sub type through property type value[null]");
        }

        @Test
        void should_error_when_no_matched_subtype() {
            RuntimeException runtimeException = assertThrows(RuntimeException.class,
                    () -> dal.evaluate(new JSONObject("{\"v\": {\"id\": 1, \"type\": V3}}"), "is V"));
            assertThat(runtimeException).hasMessage("Cannot guess sub type through property type value[V3]");
        }

        @Test
        void should_support_with_parent_type_name() throws JSONException {
            dal.getRuntimeContextBuilder().registerSchema(SIMPLE_NAME_WITH_PARENT, RightFieldAndType.class);

            assertPass(new JSONObject("{\"id\": 1}"), "is VerifySchema.RightFieldAndType");
        }

        @Test
        void should_support_partially_field_assertion() throws JSONException {
            dal.getRuntimeContextBuilder().registerSchema(SIMPLE_NAME_WITH_PARENT, IgnoreUnexpectedField.class);

            assertPass(new JSONObject("{\"id\": 1, \"unexpected\": 2}"), "is VerifySchema.IgnoreUnexpectedField");
        }

        @Test
        void should_support_assertion_schema_list_opt_and_result_true() throws JSONException {
            dal.getRuntimeContextBuilder()
                    .registerSchema(SIMPLE_NAME_WITH_PARENT, IgnoreUnexpectedField.class)
                    .registerSchema(SIMPLE_NAME_WITH_PARENT, RightFieldAndType.class);

            assertPass(new JSONObject("{\"id\": 1}"),
                    "is VerifySchema.RightFieldAndType / VerifySchema.IgnoreUnexpectedField");
        }

        @Test
        void should_support_assertion_schema_list_opt_and_result_false() throws JSONException {
            dal.getRuntimeContextBuilder()
                    .registerSchema(SIMPLE_NAME_WITH_PARENT, IgnoreUnexpectedField.class)
                    .registerSchema(SIMPLE_NAME_WITH_PARENT, RightFieldAndType.class);

            assertFailed(new JSONObject("{\"id\": 1, \"unexpected\": 2}"),
                    "is VerifySchema.RightFieldAndType / VerifySchema.IgnoreUnexpectedField");
        }
    }

    @Nested
    class VerifySchemaWithInstance {

        @BeforeEach
        void registerJson() {
            dal.getRuntimeContextBuilder()
                    .registerPropertyAccessor(JSONObject.class, new JSONObjectAccessor())
                    .registerDALCollectionFactory(JSONArray.class, new JSONArrayDALCollectionFactory())
                    .registerSchema(FieldValue.class)
                    .registerSchema(SchemaWithInstance.class)
            ;
        }

        @Test
        void should_support_verify_field_in_schema_type_with_instance() throws JSONException {
            assertPass(new JSONObject("{\"fieldValue\": {\"integer\": 1}}"), "is SchemaWithInstance");
            DalException failure = assertFailed(new JSONObject("{\"fieldValue\": {\"integer\": 2}}"), "is SchemaWithInstance");
            assertThat(failure).hasMessage("Expected to match schema `SchemaWithInstance` but was not\n" +
                    "    Expected field `.fieldValue.integer` to be formatter `Number equal to [1]`\n" +
                    "    Actual: java.lang.Integer\n" +
                    "    <2>");
        }
    }
}
