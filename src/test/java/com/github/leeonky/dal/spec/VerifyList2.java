package com.github.leeonky.dal.spec;

import org.junit.jupiter.api.Test;

import static java.util.Collections.emptyList;

class VerifyList2 extends Base {

    @Test
    void empty_list_equal_to_empty_list() {
        assertPass(emptyList(), "= []");
    }

    @Test
    void empty_list_matches_empty_list() {
        assertPass(emptyList(), ": []");
    }


    //TODO match
    //TODO equal
    //TODO skip element
    //TODO ignore tails
    //TODO element match
    //TODO element equal
    //TODO nested object
    //TODO sub alias
}
