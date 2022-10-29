package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.function.Supplier;

import static java.lang.String.format;
import static java.util.Collections.nCopies;

@Deprecated
public class InspectorContextBk {
    private final String path;
    private final InspectorCache cache;
    private final DALRuntimeContext dalRuntimeContext;
    private DumpingContext dumpingContext = new DumpingContext();

    public InspectorContextBk(String path, InspectorCache cache, DALRuntimeContext dalRuntimeContext) {
        this.path = path;
        this.cache = cache;
        this.dalRuntimeContext = dalRuntimeContext;
    }

    public String getPath() {
        return path;
    }

    public DALRuntimeContext getDalRuntimeContext() {
        return dalRuntimeContext;
    }

    public String inspect(Data data) {
        return dalRuntimeContext.fetchInspector(data).inspect(data, this);
    }

    public String dump(Data data) {
        return dalRuntimeContext.fetchInspector(data).dump(data, this);
    }

    public InspectorContextBk index(int index) {
        InspectorContextBk inspectorContextBk = new InspectorContextBk(format("%s[%d]", path, index), cache, dalRuntimeContext);
        inspectorContextBk.dumpingContext = dumpingContext.indent(0);
        return inspectorContextBk;
    }

    public InspectorContextBk sub(Object property) {
        InspectorContextBk inspectorContextBk = new InspectorContextBk(format("%s.%s", path, property), cache, dalRuntimeContext);
        inspectorContextBk.dumpingContext = dumpingContext.indent(0);
        return inspectorContextBk;
    }

    public String cached(Data data, Supplier<String> action) {
        return cache.act(path, data, action, this);
    }

    public DumpingContext dumpingContext() {
        return dumpingContext;
    }

    public void setDumpingContext(DumpingContext dumpingContext) {
        this.dumpingContext = dumpingContext;
    }

    public class DumpingContext {
        public DumpingContext() {
        }

        private StringBuilder stringBuilder = new StringBuilder();
        private String then;
        private StringBuilder thens = new StringBuilder();
        private int indent = 0;
        private int length = 0;

        public void append(String s) {
            if (thens.length() != 0) {
                stringBuilder.append(thens);
                thens = new StringBuilder();
            }
            stringBuilder.append(s);
            length = stringBuilder.length();
        }

        public String content() {
            return stringBuilder.toString();
        }

        public void appendThen(String then) {
            this.then = then;
            thens.append(then);
        }

        public DumpingContext newLine() {
            appendThen("\n" + String.join("", nCopies(indent, "    ")));
            return this;
        }

        public DumpingContext indent(int i) {
            DumpingContext dumpingContext = new DumpingContext();
            dumpingContext.stringBuilder = stringBuilder;
            dumpingContext.indent = indent + i;
            dumpingContext.thens = thens;
            thens = new StringBuilder();
            return dumpingContext;
        }

        public boolean hasContent() {
            return length != stringBuilder.length();
        }
    }
}
