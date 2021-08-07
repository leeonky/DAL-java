package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContextBuilder;
import com.github.leeonky.dal.util.PropertyAccessor;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

class PropertyNodeTest {
    private final RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder();

    @Test
    void access_property_through_public_field() {
        assertProperty(new Bean().setField(1), "field", 1);
    }

    @Test
    void access_property_through_public_get_method() {
        assertProperty(new Bean().setMethod(1), "method", 1);
        assertProperty(new Bean().setBooleanValue(true), "booleanValue", true);
    }

    @Test
    void access_map_value() {
        assertProperty(new HashMap<String, String>() {{
            put("key", "value");
        }}, "key", "value");
    }

    @Test
    void access_customer_type_property() throws JSONException {
        runtimeContextBuilder.registerPropertyAccessor(JSONObject.class, new PropertyAccessor<JSONObject>() {
            @Override
            public Object getValue(JSONObject instance, String name) {
                try {
                    return instance.get(name);
                } catch (JSONException e) {
                    throw new IllegalStateException(e);
                }
            }

            @Override
            public Set<String> getPropertyNames(JSONObject instance) {
                return null;
            }

            @Override
            public boolean isNull(JSONObject instance) {
                return instance == null || instance.equals(JSONObject.NULL);
            }
        });

        assertProperty(new JSONObject("{\"key\": \"value\"}"), "key", "value");
    }

    private void assertProperty(Object instance, String property, Object expected) {
        Object value = new PropertyNode(new ConstNode(instance), property)
                .evaluate(runtimeContextBuilder.build(null));

        assertThat(value).isEqualTo(expected);
    }

    @Setter
    @Accessors(chain = true)
    public static class Bean {
        public int field;

        @Getter
        private int method;

        @Getter
        private Bean subBean;

        @Getter
        private boolean booleanValue;
    }
}