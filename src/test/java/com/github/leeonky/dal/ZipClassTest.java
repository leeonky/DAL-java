package com.github.leeonky.dal;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static com.github.leeonky.dal.Zipped.zip;
import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;

public class ZipClassTest {
    @Nested
    public class ZipExactly {
        List<String> left = asList("a", "b", "c");
        List<String> right = asList("1", "2", "3");
        List<String> zippedList = new ArrayList<>();

        Zipped<String, String> zipped = zip(left, right);

        @Test
        void zip_and_for_each_lambda() {
            for (Zipped<String, String>.ZippedEntry ze : zipped) {
                zippedList.add(ze.left() + ze.right() + ze.index());
            }

            assertThat(zippedList).containsExactly("a10", "b21", "c32");
            assertThat(zipped.hasLeft()).isFalse();
            assertThat(zipped.hasRight()).isFalse();

            assertThat(zipped.left()).isEmpty();
            assertThat(zipped.right()).isEmpty();
        }

        @Test
        void foreach_lambda() {
            zipped.forEachElement((l, r) -> zippedList.add(l + r));

            assertThat(zippedList).containsExactly("a1", "b2", "c3");
        }

        @Test
        void foreach_index_lambda() {
            zipped.forEachElementWithIndex((i, l, r) -> zippedList.add(l + r + i));

            assertThat(zippedList).containsExactly("a10", "b21", "c32");
        }
    }

    @Test
    void left_has_more_elements() {
        List<String> left = asList("a", "b", "c");
        List<String> right = asList("1", "2");
        List<String> zippedList = new ArrayList<>();

        Zipped<String, String> zipped = zip(left, right);
        for (Zipped<String, String>.ZippedEntry ze : zipped)
            zippedList.add(ze.left() + ze.right() + ze.index());

        assertThat(zippedList).containsExactly("a10", "b21");
        assertThat(zipped.hasLeft()).isTrue();
        assertThat(zipped.hasRight()).isFalse();

        assertThat(zipped.left()).containsExactly("c");
        assertThat(zipped.right()).isEmpty();
    }

    @Test
    void right_has_more_elements() {
        List<String> left = asList("a", "b");
        List<String> right = asList("1", "2", "3");
        List<String> zippedList = new ArrayList<>();

        Zipped<String, String> zipped = zip(left, right);
        for (Zipped<String, String>.ZippedEntry ze : zipped) {
            zippedList.add(ze.left() + ze.right() + ze.index());
        }

        assertThat(zippedList).containsExactly("a10", "b21");
        assertThat(zipped.hasLeft()).isFalse();
        assertThat(zipped.hasRight()).isTrue();

        assertThat(zipped.left()).isEmpty();
        assertThat(zipped.right()).containsExactly("3");
    }
}
