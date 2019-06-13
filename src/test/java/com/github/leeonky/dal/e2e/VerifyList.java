package com.github.leeonky.dal.e2e;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;

class VerifyList extends BasicVerify {

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
    }
}
