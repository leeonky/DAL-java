package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.runtime.ArrayAccessor;
import org.json.JSONArray;
import org.json.JSONException;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;

import static java.util.Arrays.asList;

class VerifyList extends Base {

    @Test
    void should_support_java_array_as_schema_list() {
        assertPass(new Object[0], "is List");
        assertFailed(new Object(), "is List");
    }

    @Test
    void should_support_java_list_as_schema_list() {
        assertPass(new ArrayList<String>(), "is List");
    }

    @Test
    void should_support_access_item_by_const_index() {
        assertPass(new Object[]{1}, "[0] = 1");
        assertPass(asList("hello"), "[0] = 'hello'");
    }

    @Test
    void should_support_access_item_by_negative_index() {
        assertPass(new Object[]{1, 2}, "[-1] = 2");
    }

    @Test
    void should_support_get_size() {
        assertPass(new Object[]{1}, ".size = 1");
        assertPass(asList("hello"), ".size = 1");
    }

    @Test
    void should_support_customer_array_type() throws JSONException {
        dal.getRuntimeContextBuilder().registerListAccessor(JSONArray.class, new ArrayAccessor<JSONArray>() {
            @Override
            public Object get(JSONArray jsonArray, int index) {
                try {
                    return jsonArray.get(index);
                } catch (JSONException e) {
                    throw new IllegalArgumentException(e);
                }
            }

            @Override
            public int size(JSONArray jsonArray) {
                return jsonArray.length();
            }
        });

        assertPass(new JSONArray("[2]"), "is List which .size = 1 and [0] = 2");
    }
}
