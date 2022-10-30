package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import static java.lang.String.format;
import static java.util.Collections.nCopies;

public class DumpingContext {
    private final String path;
    private final InspectorCache cache;
    private final DALRuntimeContext dalRuntimeContext;
    private StringBuilder stringBuilder = new StringBuilder();
    private StringBuilder splits = new StringBuilder();
    private int indent = 0;
    private int length = 0;

    private DumpingContext(String path, InspectorCache cache, DALRuntimeContext dalRuntimeContext) {
        this.path = path;
        this.cache = cache;
        this.dalRuntimeContext = dalRuntimeContext;
    }

    public static DumpingContext rootContext(DALRuntimeContext context) {
        return new DumpingContext("root", InspectorCache.cache(), context);
    }

    public String getPath() {
        return path;
    }

    public DALRuntimeContext getDalRuntimeContext() {
        return dalRuntimeContext;
    }

    @Deprecated
    public String inspect(Data data) {
        Dumper dumper = dalRuntimeContext.fetchDumper(data);
        if (dumper != null) {
            dumper.dumpDetail(data, this);
            return "";
        }
        dalRuntimeContext.fetchInspector(data).inspect(data, this);
        return "";
    }

    @Deprecated
    public String dump(Data data) {
        Dumper dumper = dalRuntimeContext.fetchDumper(data);
        if (dumper != null) {
            dumper.dump(data, this);
            return "";
        }
        return dalRuntimeContext.fetchInspector(data).dump(data, this);
    }

    public DumpingContext index(int index) {
        return createSub(format("%s[%d]", path, index), 0);
    }

    public DumpingContext sub(Object property) {
        return createSub(format("%s.%s", path, property), 0);
    }

    private DumpingContext createSub(String subPath, int indent) {
        DumpingContext dumpingContext = new DumpingContext(subPath, cache, dalRuntimeContext);
        dumpingContext.stringBuilder = stringBuilder;
        dumpingContext.indent = this.indent + indent;
        dumpingContext.splits = splits;
        splits = new StringBuilder();
        return dumpingContext;
    }

    public DumpingContext indent() {
        return createSub(path, 1);
    }

    public void cached(Data data, Runnable runnable) {
        cache.act(path, data, this, runnable);
    }

    public DumpingContext append(String s) {
        if (splits.length() != 0) {
            stringBuilder.append(splits);
            splits = new StringBuilder();
        }
        stringBuilder.append(s);
        length = stringBuilder.length();
        return this;
    }

    public String content() {
        return stringBuilder.toString();
    }

    public void appendThen(String then) {
        splits.append(then);
    }

    public DumpingContext newLine() {
        appendThen("\n" + String.join("", nCopies(indent, "    ")));
        return this;
    }

    public DumpingContext optionalNewLine() {
        if (length != stringBuilder.length())
            newLine();
        return this;
    }
}
