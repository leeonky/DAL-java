package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.compiler.DALProcedure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.OperatorParser;

import java.util.ArrayList;
import java.util.Optional;

import static java.lang.String.join;

public class RowPrefixNode extends DALNode {
    private final Optional<Integer> index;
    private final Optional<Clause<DALRuntimeContext, DALNode>> rowSchema;
    private final Optional<DALOperator> rowOperator;

    public RowPrefixNode(Optional<Integer> index, Optional<Clause<DALRuntimeContext, DALNode>> rowSchema,
                         Optional<DALOperator> rowOperator) {
        this.index = index;
        this.rowSchema = rowSchema;
        this.rowOperator = rowOperator;
    }

    boolean hasIndex() {
        return index.isPresent();
    }

    @Override
    public String inspect() {
        String indexAndSchema = join(" ", new ArrayList<String>() {{
            index.map(Object::toString).ifPresent(this::add);
            rowSchema.map(clause -> clause.makeExpression(null).inspect()).ifPresent(this::add);
        }});
        return rowOperator.map(dalOperator -> dalOperator.inspect(indexAndSchema, "").trim()).orElse(indexAndSchema);
    }

    public DALExpression transformToExpression(DALNode input, DALOperator defaultOperator, DALNode data) {
        DALNode inputWithIndex = index.map(i -> (DALNode) new DALExpression(InputNode.INSTANCE,
                new DALOperator.PropertyImplicit(), new SymbolNode(i, SymbolNode.Type.BRACKET))).orElse(input);
        return new DALExpression(rowSchema.map(clause -> clause.makeExpression(inputWithIndex)).orElse(inputWithIndex),
                rowOperator.orElse(defaultOperator), data);
    }

    public OperatorParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> rowOperator() {
        return procedure -> rowOperator;
    }
}