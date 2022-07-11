package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.runtime.PropertyAccessor;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.Set;

class BasicVerify extends Base {

    @Nested
    class AccessProperty {

        @Test
        void should_support_register_customer_getter() throws JSONException {
            dal.getRuntimeContextBuilder().registerPropertyAccessor(JSONObject.class, new PropertyAccessor<JSONObject>() {
                @Override
                public Object getValue(JSONObject instance, Object name) {
                    try {
                        return instance.get((String) name);
                    } catch (JSONException e) {
                        throw new IllegalStateException(e);
                    }
                }

                @Override
                public Set<Object> getPropertyNames(JSONObject instance) {
                    return null;
                }

                @Override
                public boolean isNull(JSONObject instance) {
                    return instance == null || instance.equals(JSONObject.NULL);
                }
            });
            assertPass(new JSONObject("{\"field\": true}"), ".field: true");
        }
    }
}
