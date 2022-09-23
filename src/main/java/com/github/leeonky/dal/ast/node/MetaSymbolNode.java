package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.MetaData;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.RuntimeException;
import com.github.leeonky.interpreter.InterpreterException;

import java.util.function.Function;

public class MetaSymbolNode extends SymbolNode {
    public MetaSymbolNode(String content) {
        super(content, Type.SYMBOL);
    }

    @Override
    public Data getValue(DALNode left, RuntimeContextBuilder.DALRuntimeContext context) {
        Function<MetaData, Object> function = context.fetchMetaFunction(this);
        try {
            return context.wrap(function.apply(new MetaData(left, this, context)));
        } catch (InterpreterException e) {
            throw e;
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage(), getPositionBegin());
        }
    }
}
