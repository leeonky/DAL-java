package com.github.leeonky.dal.util;

import com.github.leeonky.dal.runtime.ClassKeyMap;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ClassKeyMapTest {
    @Test
    void return_empty_when_given_null() {
        ClassKeyMap<String> classKeyMap = new ClassKeyMap<>();

        assertThat(classKeyMap.tryGetData(null)).isEmpty();
    }

    @Test
    void get_data_from_father_class() {
        ClassKeyMap<String> classKeyMap = new ClassKeyMap<>();

        classKeyMap.put(Father.class, "hello");

        assertThat(classKeyMap.getData(new Son())).isEqualTo("hello");
    }

    @Test
    void get_data_from_son_class() {
        ClassKeyMap<String> classKeyMap = new ClassKeyMap<>();

        classKeyMap.put(Father.class, "hello");
        classKeyMap.put(Son.class, "world");

        assertThat(classKeyMap.getData(new Son())).isEqualTo("world");
    }

    public static class Father {
    }

    public static class Son extends Father {
    }
}