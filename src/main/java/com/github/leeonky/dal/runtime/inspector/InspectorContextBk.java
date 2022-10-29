package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.function.Supplier;

import static java.lang.String.format;

@Deprecated
public class InspectorContextBk {
    private final String path;
    private final InspectorCache cache;
    private final DALRuntimeContext dalRuntimeContext;

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
        return new InspectorContextBk(format("%s[%d]", path, index), cache, dalRuntimeContext);
    }

    public InspectorContextBk sub(Object property) {
        return new InspectorContextBk(format("%s.%s", path, property), cache, dalRuntimeContext);
    }

    public String cached(Data data, Supplier<String> action) {
        return cache.act(path, data, action);
    }

    public DumpingContext dumpingContext() {
        return new DumpingContext();
    }

    public class DumpingContext {
        private StringBuilder stringBuilder = new StringBuilder();
        private String then;

        public void append(String s) {
            if (then != null) {
                stringBuilder.append(then);
                then = null;
            }
            stringBuilder.append(s);
        }

        public String content() {
            return stringBuilder.toString();
        }

        public void appendThen(String then) {
            this.then = then;
        }
    }
}
