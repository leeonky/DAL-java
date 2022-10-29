package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import static java.util.Collections.nCopies;

public class DumpingContext extends InspectorContextBk {

    //    TODO private
    public StringBuilder stringBuilder = new StringBuilder();
    public StringBuilder thens = new StringBuilder();
    public int indent = 0;
    private int length = 0;

    public DumpingContext(String path, InspectorCache cache, RuntimeContextBuilder.DALRuntimeContext dalRuntimeContext) {
        super(path, cache, dalRuntimeContext);
    }

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
        thens.append(then);
    }

    public DumpingContext newLine() {
        appendThen("\n" + String.join("", nCopies(indent, "    ")));
        return this;
    }

    public DumpingContext indent(int i) {
        DumpingContext dumpingContext = new DumpingContext(path, cache, dalRuntimeContext);
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
