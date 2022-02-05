package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.runtime.PropertyAccessor;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class BasicVerify extends Base {

    @Nested
    class Basic {

        @Test
        void verify_expression_return_type_should_be_boolean() {
            IllegalStateException illegalStateException = assertThrows(IllegalStateException.class,
                    () -> dal.assertTrue(1, ""));
            assertThat(illegalStateException).hasMessage("Verification result should be boolean but 'java.lang.Integer'");
        }
    }

    @Nested
    class AccessProperty {

        @Test
        void should_support_register_customer_getter() throws JSONException {
            dal.getRuntimeContextBuilder().registerPropertyAccessor(JSONObject.class, new PropertyAccessor<JSONObject>() {
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
            assertTrue(new JSONObject("{\"field\": true}"), ".field");
        }
    }
}
