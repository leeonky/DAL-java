package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

public interface Dumper {
    Dumper STRING_DUMPER = new StringDumper(),
            VALUE_INSPECTOR = new ValueDumper(),
            LIST_DUMPER = new ListDumper(),
            MAP_DUMPER = new MapDumper();

    void dumpDetail(Data data, DumpingContext dumpingContext);

    default void dump(Data data, DumpingContext dumpingContext) {
        dumpDetail(data, dumpingContext);
    }

    interface Cacheable extends Dumper {

        @Override
        default void dumpDetail(Data data, DumpingContext context) {
            context.cached(data, () -> cachedInspect(data, context));
        }

        @Override
        default void dump(Data data, DumpingContext context) {
            context.cached(data, () -> cachedDump(data, context));
        }

        default void cachedDump(Data data, DumpingContext context) {
            cachedInspect(data, context);
        }

        void cachedInspect(Data data, DumpingContext context);
    }
}
