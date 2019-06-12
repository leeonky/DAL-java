package com.github.leeonky.dal.e2e;

import lombok.Setter;
import lombok.experimental.Accessors;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

class VerifySchema extends VerifyBase {

    @BeforeEach
    void registerBeanSchema() {
        dataAssert.getCompilingContextBuilder().registerSchema(Bean.class);
    }

    @Test
    void should_check_get_class_for_class_schema() {
        assertPass(new Bean(), "is Bean");
    }

    @Test
    void should_pass_when_object_has_same_properties() {
        assertPass(new AnotherBean(), "is Bean");
    }

    @Test
    void should_pass_when_object_has_less_properties() {
        assertPass(new BeanWithLessProperty(), "is Bean");
    }

    @Test
    void should_failed_when_object_has_more_properties() {
        assertFailed(new BeanWithMoreProperty(), "is Bean");
    }

    @Test
    void should_not_pass_when_object_is_null() {
        assertFailed(null, "is Bean");
    }

    @Test
    void should_support_verify_map_via_key_set() {
        assertPass(new HashMap<String, Object>() {{
            put("f1", 1);
            put("f2", 1);
        }}, "is Bean");
    }

    @Test
    void should_support_register_customer_object_type() throws JSONException {
        dataAssert.getCompilingContextBuilder().registerPropertyCollector(JSONObject.class, j -> {
            Set<String> set = new HashSet<>();
            Iterator iterator = j.keys();
            while (iterator.hasNext())
                set.add(iterator.next().toString());
            return set;
        });

        assertPass(new JSONObject("{\"f1\": 1, \"f2\": 1}"), "is Bean");
    }

    @Setter
    @Accessors(chain = true)
    public static class Bean {
        public String f1, f2;
    }

    @Setter
    @Accessors(chain = true)
    public static class BeanWithLessProperty {
        public String f1;
    }

    @Setter
    @Accessors(chain = true)
    public static class BeanWithMoreProperty {
        public String f1, f2, f3;
    }

    @Setter
    @Accessors(chain = true)
    public static class AnotherBean {
        public String f1, f2;
    }
}
