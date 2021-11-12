package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.ListAccessor;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;

import static com.github.leeonky.dal.ast.InputNode.INSTANCE;
import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static java.util.Collections.emptyList;
import static org.assertj.core.api.Assertions.assertThat;

class PropertyNodeTest {

    @Test
    void support_first_index_of_list() {
        RuntimeContextBuilder.RuntimeContext runtimeContext = new RuntimeContextBuilder()
                .registerListAccessor(ArrayList.class, new ListAccessor<ArrayList<?>>() {
                    @Override
                    public Iterable<?> toIterable(ArrayList<?> instance) {
                        return instance;
                    }

                    @Override
                    public int firstIndex() {
                        return 1;
                    }
                }).build(new ArrayList<>(Arrays.asList(1, 2)));

        assertThat(new PropertyNode(INSTANCE, 1, BRACKET).evaluate(runtimeContext)).isEqualTo(1);
        assertThat(new PropertyNode(INSTANCE, -1, BRACKET).evaluate(runtimeContext)).isEqualTo(2);
    }

    public static class CustomizedList {
        public int value = 100;
    }

    @Test
    void access_customized_list_property() {
        RuntimeContextBuilder.RuntimeContext runtimeContext = new RuntimeContextBuilder()
                .registerListAccessor(CustomizedList.class, instance -> emptyList())
                .build(new CustomizedList());

        assertThat(new PropertyNode(INSTANCE, "value", BRACKET).evaluate(runtimeContext)).isEqualTo(100);
    }
}