package com.github.leeonky.util;

public interface Comparator<V extends Comparable<V>> {

    static <V extends Comparable<V>> Comparator<V> lessThan(V target) {
        return value -> value.compareTo(target) < 0;
    }

    static <V extends Comparable<V>> Comparator<V> equalTo(V target) {
        return value -> value.compareTo(target) == 0;
    }

    static <V extends Comparable<V>> Comparator<V> greaterThan(V target) {
        return not(lessOrEqualTo(target));
    }

    static <V extends Comparable<V>> Comparator<V> lessOrEqualTo(V target) {
        return lessThan(target).or(equalTo(target));
    }

    static <V extends Comparable<V>> Comparator<V> greaterOrEqualTo(V target) {
        return not(lessThan(target));
    }

    boolean compareTo(V value);

    default Comparator<V> or(Comparator<V> another) {
        return value -> compareTo(value) || another.compareTo(value);
    }

    static <V extends Comparable<V>> Comparator<V> not(Comparator<V> another) {
        return value -> !another.compareTo(value);
    }
}
