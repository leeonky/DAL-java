package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.HashMap;
import java.util.Map;

import static java.lang.String.format;
import static java.util.Collections.nCopies;

public class DumpingContext {
    private final Map<DumpingCacheKey, String> caches;
    private final String path;
    private final DALRuntimeContext runtimeContext;
    private final StringBuilder stringBuilder;
    private final int indent;
    private StringBuilder splits;
    private int length = 0;

    private DumpingContext(String path, DALRuntimeContext runtimeContext, StringBuilder stringBuilder, int indent,
                           Map<DumpingCacheKey, String> caches, StringBuilder splits) {
        this.path = path;
        this.runtimeContext = runtimeContext;
        this.stringBuilder = stringBuilder;
        this.indent = indent;
        this.caches = caches;
        this.splits = splits;
    }

    public static DumpingContext rootContext(DALRuntimeContext context) {
        return new DumpingContext("root", context, new StringBuilder(), 0, new HashMap<>(), new StringBuilder());
    }

    public String getPath() {
        return path;
    }

    public DALRuntimeContext getRuntimeContext() {
        return runtimeContext;
    }

    public DumpingContext dump(Data data) {
        runtimeContext.fetchDumper(data).dump(data, this);
        return this;
    }

    public DumpingContext dumpValue(Data data) {
        runtimeContext.fetchDumper(data).dumpValue(data, this);
        return this;
    }

    public DumpingContext index(int index) {
        return createSub(format("%s[%d]", path, index), 0);
    }

    public DumpingContext sub(Object property) {
        return createSub(format("%s.%s", path, property), 0);
    }

    public DumpingContext indent() {
        return createSub(path, 1);
    }

    public DumpingContext sub() {
        return createSub(path, 0);
    }

    private DumpingContext createSub(String subPath, int indent) {
        return new DumpingContext(subPath, runtimeContext, stringBuilder, this.indent + indent, caches, takeSplits());
    }

    private StringBuilder takeSplits() {
        StringBuilder temp = splits;
        splits = new StringBuilder();
        return temp;
    }

    public void cached(Data data, Runnable runnable) {
        act(path, data, runnable);
    }

    public DumpingContext append(String s) {
        if (splits.length() != 0)
            stringBuilder.append(takeSplits());
        stringBuilder.append(s);
        length = stringBuilder.length();
        return this;
    }

    public String content() {
        return stringBuilder.toString();
    }

    public DumpingContext appendThen(String then) {
        splits.append(then);
        return this;
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

    private void act(String path, Data data, Runnable runnable) {
        DumpingCacheKey key = new DumpingCacheKey(data);
        String reference = caches.get(key);
        if (reference == null) {
            caches.put(key, path);
            runnable.run();
        } else
            append("*reference* " + reference);
    }
}
