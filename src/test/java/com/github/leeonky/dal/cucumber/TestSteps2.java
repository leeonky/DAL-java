package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.DalException;
import com.github.leeonky.dal.ast.Node;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;

import java.util.HashMap;
import java.util.Map;

import static com.github.leeonky.dal.cucumber.Parser.CONST;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class TestSteps2 {
    private final Map<String, Parser> parserMap = new HashMap<String, Parser>() {{
        put("const", CONST);
    }};
    private final DAL dal = new DAL();
    private SourceCode sourceCode;
    private String code;
    private DalException dalException;
    private Node node;

    @Given("the following dal code xx:")
    public void the_following_dal_code_xx(String code) {
        sourceCode = new SourceCode(this.code = parseTabAndSpace(code));
    }

    @Then("got the following {string} node xx:")
    public void got_the_following_node_xx(String factory, String assertion) {
        dal.assertData(node = parserMap.get(factory).fetch(sourceCode.leftTrim()).orElse(null), assertion);
    }

    @Then("evaluate result is xx:")
    public void evaluate_result_is_xx(String assertion) {
        dal.assertData(node.evaluate(dal.getRuntimeContextBuilder().build(null)), assertion);
    }

    @Then("evaluate as {string} result is:")
    public void evaluate_as_result_is(String factory, String assertion) {
        dal.assertData(parserMap.get(factory).fetch(new SourceCode(code).leftTrim()).orElse(null)
                .evaluate(dal.getRuntimeContextBuilder().build(null)), assertion);
    }

    @Then("failed to get the following {string} node with the following message xx:")
    public void failed_to_get_the_following_node_with_the_following_message_xx(String factory, String message) {
        dalException = assertThrows(DalException.class, () ->
                parserMap.get(factory).fetch(new SourceCode(code).leftTrim()));
        assertThat(dalException).hasMessage(message);
    }

    @Then("got the following source code information xx:")
    public void got_the_following_source_code_information_xx(String sourceCodePosition) {
        assertThat(dalException.show(code)).isEqualTo(sourceCodePosition);
    }

    private String parseTabAndSpace(String code) {
        return code.replace("`TAB", "\t").replace("`SPACE", " ");
    }
}
