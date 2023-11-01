Feature: customized schema

  Scenario: customized schema verification
    Given the following schema class:
    """
    @Partial
    public class SchemaVerify implements Schema {
        public void verify(Data data) throws SchemaAssertionFailure {
            throw new SchemaAssertionFailure((String)data.getValue("message").instance());
        }
    }
    """
    And the following json:
    """
    {
      "message": "a message"
    }
    """
    When evaluate by:
    """
    is SchemaVerify
    """
    Then failed with the message:
    """
    Expected to match schema `SchemaVerify` but was not
        a message
    """
    And got the following notation:
    """
    is SchemaVerify
       ^
    """
