package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.cucumber.JSONArrayDALCollectionFactory;
import org.json.JSONArray;
import org.json.JSONException;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;

import static java.util.Arrays.asList;

class VerifyList extends Base {

    @Test
    void should_support_java_array_as_schema_list() {
        assertPass(new Object[0], "= []");
        assertFailed(new Object(), "= []");
    }

    @Test
    void should_support_java_list_as_schema_list() {
        assertPass(new ArrayList<String>(), "= []");
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
        assertPass(new Object[]{1}, "::size = 1");
        assertPass(asList("hello"), "::size = 1");
    }

    @Test
    void should_support_customer_array_type() throws JSONException {
        dal.getRuntimeContextBuilder()
                .registerDALCollectionFactory(JSONArray.class, new JSONArrayDALCollectionFactory());

        assertPass(new JSONArray("[2]"), "= [*]");
    }
}
