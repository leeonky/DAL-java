package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContextBuilder;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;

class PropertyNodeTest {
    private final CompilingContextBuilder compilingContextBuilder = new CompilingContextBuilder();

    @Test
    void access_property_through_public_field() {
        assertProperty(new Bean().setField(1), Collections.singletonList("field"), 1);
    }

    @Test
    void access_property_through_public_get_method() {
        assertProperty(new Bean().setMethod(1), Collections.singletonList("method"), 1);
        assertProperty(new Bean().setBooleanValue(true), Collections.singletonList("booleanValue"), true);
    }

    @Test
    void access_property_list() {
        assertProperty(new Bean().setSubBean(new Bean().setField(1)), asList("subBean", "field"), 1);
    }

    @Test
    void access_map_value() {
        assertProperty(new HashMap<String, String>() {{
            put("key", "value");
        }}, Collections.singletonList("key"), "value");
    }

    @Test
    void access_customer_type_property() throws JSONException {
        compilingContextBuilder.registerPropertyAccessor(JSONObject.class, JSONObject::get);

        assertProperty(new JSONObject("{\"key\": \"value\"}"), Collections.singletonList("key"), "value");
    }

    private void assertProperty(Object instance, List<String> properties, Object expected) {
        Object value = new PropertyNode(new ConstNode(instance), properties)
                .evaluate(compilingContextBuilder.build(null));

        assertThat(value).isEqualTo(expected);
    }

    @Setter
    @Accessors(chain = true)
    static class Bean {
        public int field;

        @Getter
        private int method;

        @Getter
        private Bean subBean;

        @Getter
        private boolean booleanValue;
    }
}