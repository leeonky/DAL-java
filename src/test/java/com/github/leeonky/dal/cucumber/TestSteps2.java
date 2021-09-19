package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.DAL;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;

import java.util.HashMap;
import java.util.Map;

import static com.github.leeonky.dal.cucumber.Parser.CONST;

public class TestSteps2 {
    private final Map<String, Parser> parserMap = new HashMap<String, Parser>() {{
        put("const", CONST);
    }};
    private final DAL dal = new DAL();
    private SourceCode sourceCode;
    private String code;

    @Given("the following dal code xx:")
    public void the_following_dal_code_xx(String code) {
        sourceCode = new SourceCode(this.code = parseTabAndSpace(code));
    }

    @Then("got the following {string} node xx:")
    public void got_the_following_node_xx(String factory, String assertion) {
        dal.assertData(parserMap.get(factory).fetch(sourceCode.leftTrim()).orElse(null), assertion);
    }

    @Then("evaluate result is xx:")
    public void evaluate_result_is_xx(String assertion) {
        dal.assertData(dal.evaluate(null, code), assertion);
    }

    private String parseTabAndSpace(String code) {
        return code.replace("`TAB", "\t").replace("`SPACE", " ");
    }
}
