package com.github.leeonky.dal.ast.node;

public class WildcardNode extends DALNode {
    private final String code;

    public WildcardNode(String code) {
        this.code = code;
    }
   
    @Override
    public String inspect() {
        return code;
    }
}
