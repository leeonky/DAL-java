package com.github.leeonky.dal.runtime;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.Assertions.expect;
import static java.util.Arrays.asList;

class DALCollectionTest {

    @Nested
    class Filter {

        @Nested
        class CollectionList {

            @Test
            void filter() {
                CollectionDALCollection<Integer> collection = new CollectionDALCollection<>(asList(1, 2, 3));

                expect(collection.filter(i -> i == 2)).should("values= [2]");
            }

            @Test
            void should_has_same_first_index() {
                CollectionDALCollection<Integer> collection = new CollectionDALCollection<Integer>(asList(1, 2, 3)) {
                    @Override
                    public int firstIndex() {
                        return 1;
                    }
                };

                expect(collection.filter(i -> i == 2)).should(": { values= [2] firstIndex: 1}");
            }
        }

        @Nested
        class IterableList {

            @Nested
            class NonInfiniteList {

                @Test
                void filter() {
                    IterableDALCollection<Integer> collection = new IterableDALCollection<>(asList(1, 2, 3));

                    expect(collection.filter(i -> i == 2)).should("values= [2]");
                }

                @Test
                void should_has_same_first_index() {
                    IterableDALCollection<Integer> collection = new IterableDALCollection<Integer>(asList(1, 2, 3)) {
                        @Override
                        public int firstIndex() {
                            return 1;
                        }
                    };

                    expect(collection.filter(i -> i == 2)).should(": { values= [2] firstIndex: 1}");
                }
            }

            @Nested
            class InfiniteList {
                int seed = 0;

                @Test
                void filter() {
                    InfiniteDALCollection<Integer> collection = new InfiniteDALCollection<>(() -> seed++);

                    expect(collection.filter(i -> i < 2)).should(": [0 1 ...]");
                }
            }
        }
    }
}