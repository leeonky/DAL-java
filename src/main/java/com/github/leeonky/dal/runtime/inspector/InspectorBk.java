package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

@Deprecated
public interface InspectorBk {
    InspectorBk MAP_INSPECTOR_BK = new InspectorBk() {

        @Override
        public String inspect(Data data, InspectorContextBk context) {
            Dumper.MAP_DUMPER.dumpDetail(data, context.dumpingContext());
            return "";
        }

        @Override
        public String dump(Data data, InspectorContextBk context) {
            Dumper.MAP_DUMPER.dump(data, context.dumpingContext());
            return "";
        }
    };
    InspectorBk LIST_INSPECTOR_BK = new InspectorBk() {
        @Override
        public String inspect(Data data, InspectorContextBk context) {
            Dumper.LIST_DUMPER.dumpDetail(data, context.dumpingContext());
            return "";
        }

        @Override
        public String dump(Data data, InspectorContextBk context) {
            Dumper.LIST_DUMPER.dump(data, context.dumpingContext());
            return "";
        }
    };

    //    TODO rename
    String inspect(Data data, InspectorContextBk context);

    default String dump(Data data, InspectorContextBk context) {
        return inspect(data, context);
    }

    interface Cacheable extends InspectorBk {

        @Override
        default String dump(Data data, InspectorContextBk context) {
            return context.cached(data, () -> cachedDump(data, context));
        }

        @Override
        default String inspect(Data data, InspectorContextBk context) {
            return context.cached(data, () -> cachedInspect(data, context));
        }

        default String cachedDump(Data data, InspectorContextBk context) {
            return cachedInspect(data, context);
        }

        String cachedInspect(Data data, InspectorContextBk context);
    }
}
