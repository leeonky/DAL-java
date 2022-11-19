package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import static java.lang.String.format;
import static java.util.Collections.nCopies;

public class DumpingBuffer {
    private final String path;
    private final int indent;
    private final LineBuffer lineBuffer;
    private StringBuilder splits;
    private int length = 0;
    private final DALRuntimeContext runtimeContext;

    private DumpingBuffer(String path, int indent, StringBuilder splits, LineBuffer buffer, DALRuntimeContext context) {
        this.path = path;
        lineBuffer = buffer;
        runtimeContext = context;
        this.indent = indent;
        this.splits = splits;
    }

    public static DumpingBuffer rootContext(DALRuntimeContext context) {
        return new DumpingBuffer("root", 0, new StringBuilder(), new LineBuffer(context), context);
    }

    public String getPath() {
        return path;
    }

    public DALRuntimeContext getRuntimeContext() {
        return runtimeContext;
    }

    public DumpingBuffer dump(Data data) {
        try {
            runtimeContext.fetchDumper(data).dump(data, this);
        } catch (Exception e) {
            append("*dump throw* " + e);
        }
        return this;
    }

    public DumpingBuffer dumpValue(Data data) {
        try {
            runtimeContext.fetchDumper(data).dumpValue(data, this);
        } catch (Exception e) {
            append("*dump throw* " + e);
        }
        return this;
    }

    public DumpingBuffer index(int index) {
        return createSub(format("%s[%d]", path, index), 0);
    }

    public DumpingBuffer sub(Object property) {
        return createSub(format("%s.%s", path, property), 0);
    }

    public DumpingBuffer indent() {
        return createSub(path, 1);
    }

    public DumpingBuffer sub() {
        return createSub(path, 0);
    }

    private DumpingBuffer createSub(String subPath, int indent) {
        return new DumpingBuffer(subPath, this.indent + indent, takeSplits(), lineBuffer, runtimeContext);
    }

    private StringBuilder takeSplits() {
        StringBuilder temp = splits;
        splits = new StringBuilder();
        return temp;
    }

    public void cached(Data data, Runnable runnable) {
        lineBuffer.cached(path, data, runnable, p -> append("*reference* " + p));
    }

    public DumpingBuffer append(String s) {
        length = lineBuffer.append(takeSplits(), s).length();
        return this;
    }

    public String content() {
        return lineBuffer.toString();
    }

    public DumpingBuffer appendThen(String then) {
        splits.append(then);
        return this;
    }

    public DumpingBuffer newLine() {
        appendThen("\n" + String.join("", nCopies(indent, "    ")));
        return this;
    }

    public DumpingBuffer optionalNewLine() {
        if (length != lineBuffer.length())
            newLine();
        return this;
    }

    public static class LineBuffer {
        private final Map<DumpingCacheKey, String> caches = new HashMap<>();
        private final DALRuntimeContext runtimeContext;
        private final StringBuilder stringBuilder = new StringBuilder();
        private int lineCount = 0;
        private boolean finished = false;

        public LineBuffer(DALRuntimeContext runtimeContext) {
            this.runtimeContext = runtimeContext;
        }

        public void cached(String path, Data data, Runnable dumpAction, Consumer<String> refAction) {
            DumpingCacheKey key = new DumpingCacheKey(data);
            String reference = caches.get(key);
            if (reference == null) {
                caches.put(key, path);
                dumpAction.run();
            } else {
                refAction.accept(reference);
            }
        }

        public int length() {
            return stringBuilder.length();
        }

        @Override
        public String toString() {
            return stringBuilder.toString();
        }

        public LineBuffer append(StringBuilder splits, String content) {
            if (!finished) {
                if (splits.length() != 0) {
                    if ((lineCount += splits.chars().filter(c -> c == '\n').count())
                            >= runtimeContext.maxDumpingLineCount()) {
                        stringBuilder.append("\n...");
                        finished = true;
                        return this;
                    }
                    stringBuilder.append(splits);
                }
                stringBuilder.append(content);
            }
            return this;
        }
    }
}
