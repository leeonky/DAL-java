package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

public interface Dumper {
    Dumper STRING_DUMPER = new StringDumper(),
            VALUE_INSPECTOR = new ValueDumper();

    void dumpDetail(Data data, DumpingContext dumpingContext);

    default void dump(Data data, DumpingContext dumpingContext) {
        dumpDetail(data, dumpingContext);
    }
}
