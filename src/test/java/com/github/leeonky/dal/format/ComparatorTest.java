package com.github.leeonky.dal.format;

import org.junit.jupiter.api.Test;

import static com.github.leeonky.util.function.Comparator.*;
import static org.assertj.core.api.Assertions.assertThat;

class ComparatorTest {

    @Test
    void less_than() {
        assertThat(lessThan(1).compareTo(0)).isTrue();
        assertThat(lessThan("A").compareTo("B")).isFalse();
        assertThat(lessThan(1.0).compareTo(1.0)).isFalse();
    }

    @Test
    void greater_than() {
        assertThat(greaterThan(1).compareTo(0)).isFalse();
        assertThat(greaterThan("A").compareTo("B")).isTrue();
        assertThat(greaterThan(1.0).compareTo(1.0)).isFalse();
    }

    @Test
    void less_or_equal_to() {
        assertThat(lessOrEqualTo(1).compareTo(0)).isTrue();
        assertThat(lessOrEqualTo("A").compareTo("B")).isFalse();
        assertThat(lessOrEqualTo(1.0).compareTo(1.0)).isTrue();
    }

    @Test
    void greater_or_equal_to() {
        assertThat(greaterOrEqualTo(1).compareTo(0)).isFalse();
        assertThat(greaterOrEqualTo("A").compareTo("B")).isTrue();
        assertThat(greaterOrEqualTo(1.0).compareTo(1.0)).isTrue();
    }

    @Test
    void equal_to() {
        assertThat(equalTo(1).compareTo(0)).isFalse();
        assertThat(equalTo("A").compareTo("B")).isFalse();
        assertThat(equalTo(1.0).compareTo(1.0)).isTrue();
    }
}
