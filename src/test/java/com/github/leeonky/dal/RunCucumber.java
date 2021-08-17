package com.github.leeonky.dal;

import org.junit.jupiter.api.Test;

import static io.cucumber.core.cli.Main.run;
import static java.lang.Thread.currentThread;
import static org.assertj.core.api.Assertions.assertThat;

public class RunCucumber {

    @Test
    void run_cucumber() {
        assertThat(run(new String[]{"--plugin", "pretty", "--glue", "com.github.leeonky",
                "src/test/resources/features"}, currentThread().getContextClassLoader())).isEqualTo(Byte.valueOf("0"));
    }
}
