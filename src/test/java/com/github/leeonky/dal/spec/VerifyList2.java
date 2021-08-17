package com.github.leeonky.dal.spec;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;

class VerifyList2 extends Base {

    @Nested
    class MatchesSize {

        @Test
        void empty_list_equal_to_or_matches_empty_list() {
            assertPass(emptyList(), "= []");
            assertPass(emptyList(), ": []");
        }

        @Test
        void list_size_should_matches() {
            assertPass(singletonList(1), ": [{}]");
            assertPass(asList(1, 2), ": [{} {}]");
            assertFailed(singletonList(1), ": []");
        }
    }

    @Nested
    class ListEqual {

        @Test
        void element_equal_to_element() {
            assertPass(singletonList(1), "= [1]");
            assertFailed(singletonList(2), "= [1]");
        }

        @Test
        void element_matches_element() {
            assertPass(singletonList(1), ": [1]");
            assertFailed(singletonList(2), ": [1]");
            assertPass(singletonList('a'), ": ['a']");
        }


        @Test
        void nested_list() {
            assertPass(singletonList(singletonList(1)), "= [[1]]");
            assertFailed(singletonList(singletonList(1)), "= [[2]]");
        }
    }

    //TODO match
    //TODO equal
    //TODO skip element
    //TODO ignore tails
    //TODO element match
    //TODO element equal
    //TODO nested object
    //TODO sub alias
    //TODO support comma
}
