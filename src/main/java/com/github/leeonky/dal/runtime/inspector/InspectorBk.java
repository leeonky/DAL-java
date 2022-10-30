package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

@Deprecated
public interface InspectorBk {
    InspectorBk MAP_INSPECTOR_BK = new InspectorBk() {

        @Override
        public String inspect(Data data, DumpingContext context) {
            Dumper.MAP_DUMPER.dumpDetail(data, context);
            return "";
        }

        @Override
        public String dump(Data data, DumpingContext context) {
            Dumper.MAP_DUMPER.dump(data, context);
            return "";
        }
    };
    InspectorBk LIST_INSPECTOR_BK = new InspectorBk() {
        @Override
        public String inspect(Data data, DumpingContext context) {
            Dumper.LIST_DUMPER.dumpDetail(data, context);
            return "";
        }

        @Override
        public String dump(Data data, DumpingContext context) {
            Dumper.LIST_DUMPER.dump(data, context);
            return "";
        }
    };

    //    TODO rename
    String inspect(Data data, DumpingContext context);

    default String dump(Data data, DumpingContext context) {
        return inspect(data, context);
    }

}
