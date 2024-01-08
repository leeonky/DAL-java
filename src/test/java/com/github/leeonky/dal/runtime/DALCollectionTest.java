package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.extensions.MapPropertyAccessor;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.Collection;
import java.util.Map;

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

                @Test
                void should_has_same_first_index() {
                    InfiniteDALCollection<Integer> collection = new InfiniteDALCollection<Integer>(() -> seed++) {
                        @Override
                        public int firstIndex() {
                            return 1;
                        }
                    };

                    expect(collection.filter(i -> i < 2)).should(": { values= [0 1 ...] firstIndex: 1}");
                }
            }
        }

        @Nested
        class DataListList {

            @Test
            void filter() {
                RuntimeContextBuilder builder = new RuntimeContextBuilder()
                        .registerPropertyAccessor(Map.class, new MapPropertyAccessor())
                        .registerDALCollectionFactory(Collection.class, CollectionDALCollection::new);
                RuntimeContextBuilder.DALRuntimeContext context = builder.build((Object) null);

                Data.DataList collection = context.wrap(asList(1, 2, 3)).list();

                expect(collection.filter(i -> (int) i == 2)).should("values= [2]");
            }

            @Test
            void should_has_same_first_index() {
                RuntimeContextBuilder builder = new RuntimeContextBuilder()
                        .registerPropertyAccessor(Map.class, new MapPropertyAccessor())
                        .registerDALCollectionFactory(Collection.class, collection -> new CollectionDALCollection<Object>(collection) {
                            @Override
                            public int firstIndex() {
                                return 1;
                            }
                        });
                RuntimeContextBuilder.DALRuntimeContext context = builder.build((Object) null);

                Data.DataList collection = context.wrap(asList(1, 2, 3)).list();

                expect(collection.filter(i -> (int) i == 2)).should(": { values= [2] firstIndex: 1}");
            }
        }
    }
}