package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

public interface Dumper {
    Dumper STRING_DUMPER = new StringDumper(),
            VALUE_INSPECTOR = new ValueDumper(),
            LIST_DUMPER = new ListDumper(),
            MAP_DUMPER = new MapDumper();

    void dump(Data data, DumpingBuffer dumpingBuffer);

    default void dumpValue(Data data, DumpingBuffer dumpingBuffer) {
        dump(data, dumpingBuffer);
    }

    interface Cacheable extends Dumper {

        @Override
        default void dump(Data data, DumpingBuffer context) {
            context.cached(data, () -> cachedInspect(data, context));
        }

        @Override
        default void dumpValue(Data data, DumpingBuffer context) {
            context.cached(data, () -> cachedDump(data, context));
        }

        default void cachedDump(Data data, DumpingBuffer context) {
            cachedInspect(data, context);
        }

        void cachedInspect(Data data, DumpingBuffer context);
    }
}
