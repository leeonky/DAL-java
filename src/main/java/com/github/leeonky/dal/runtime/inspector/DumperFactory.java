package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

import java.util.function.Function;

public interface DumperFactory extends Function<Data, Dumper> {
}
