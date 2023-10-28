Feature: list mapping

  Scenario: Map list element
    Given the following json:
    """
      {
        "items": [{
          "id": 100
        }, {
          "id": 200
        }]
      }
    """
    When evaluate by:
    """
      items.id[]
    """
    Then the result should:
    """
    : [100 200]
    """
    And the inspect should:
    """
    items.id[]
    """

  Scenario: Map list element for implicit property
    Given the following json:
    """
    [{
        "id": 100
      }, {
        "id": 200
      }]
    """
    When evaluate by:
    """
      id[]
    """
    Then the result should:
    """
    : [100 200]
    """
    And the inspect should:
    """
    id[]
    """

  Scenario: Map list element for bracket property
    Given the following json:
    """
    [{
        "id": 100
      }, {
        "id": 200
      }]
    """
    When evaluate by:
    """
      ['id'][]
    """
    Then the result should:
    """
    : [100 200]
    """
    And the inspect should:
    """
    ['id'][]
    """

  Scenario: List mapping chain
    Given the following json:
    """
      {
        "products": [{
          "catalog": {
            "sub": {
              "value": {
                "string": "001"
              }
            }
          }
        }, {
          "catalog": {
            "sub": {
              "value": {
                "string": "002"
              }
            }
          }
        }]
      }
    """
    When evaluate by:
    """
      products.catalog[].sub.value.string
    """
    Then the result should:
    """
    : ["001" "002"]
    """
    And the inspect should:
    """
    products.catalog[].sub.value.string
    """

  Scenario: disable implicit list mapping
    Given the following json:
    """
      {
        "items": [{
          "id": 100
        }]
      }
    """
    When evaluate by:
    """
    items.id= [100]
    """
    Then failed with the message:
    """
    Get property `id` failed, property can be:
      1. public field
      2. public getter
      3. public no args method
      4. Map key value
      5. customized type getter
      6. static method extension
    Method or property `id` does not exist in `java.util.ArrayList`
    Implicit list mapping is not allowed in current version of DAL, use `id[]` instead
    """
    And got the following notation:
    """
    items.id= [100]
          ^
    """

  Scenario: raise error when access invalid property
    Given the following java class:
    """
    public class DataItem extends java.util.ArrayList<Object>{
    }
    """
    Given the following java class:
    """
    public class Data {
      public DataItem list = new DataItem() {{
        add("item0");
      }};
    }
    """
    And set the first element index to 1 of list "DataItem"
    When use a instance of java class "Data" to evaluate:
    """
      list.invalid[]= [1]
    """
    Then failed with the message:
    """
    Mapping element[1]:
    Get property `invalid` failed, property can be:
      1. public field
      2. public getter
      3. public no args method
      4. Map key value
      5. customized type getter
      6. static method extension
    Method or property `invalid` does not exist in `java.lang.String`
    """
    And got the following notation:
    """
      list.invalid[]= [1]
                  ^
    """
    When use a instance of java class "Data" to evaluate:
    """
      list.trim[].invalid= [1]
    """
    Then failed with the message:
    """
    Mapping element[1]:
    Get property `invalid` failed, property can be:
      1. public field
      2. public getter
      3. public no args method
      4. Map key value
      5. customized type getter
      6. static method extension
    Method or property `invalid` does not exist in `java.lang.String`
    """
    And got the following notation:
    """
      list.trim[].invalid= [1]
                  ^
    """

  Scenario: raise error when instance is not list
    Given the following java class:
    """
    public class Data {
      public String list = "not list";
    }
    """
    When use a instance of java class "Data" to evaluate:
    """
      list.invalid[]
    """
    Then failed with the message:
    """
    Invalid input value, expect a List but: java.lang.String
    <not list>
    """
    And got the following notation:
    """
      list.invalid[]
                  ^
    """

  Scenario: should inherit first index in list mapping
    Given the following java class:
    """
    public class DataItem extends java.util.ArrayList<Object>{
    }
    """
    Given the following java class:
    """
    public class Data {
      public DataItem list = new DataItem() {{
        add(new java.util.HashMap<Object, Object>() {{
          put("key1", new java.util.HashMap<Object, Object>() {{
            put("key2", 100);
          }});
        }});
      }};
    }
    """
    And set the first element index to 1 of list "DataItem"
    Then the following verification for the instance of java class "Data" should pass:
    """
    : {
      list[1].key1.key2= 100
      list.key1[]= [{key2= 100}]
      list.key1[][1].key2= 100
      list.key1[].key2[1]= 100
      list.key1[].key2= [100]
    }
    """

  Scenario: inherit schema after mapping
    Given the following java class:
    """
    public class Bean {
      public final String id;
      public Bean(String v) {
        this.id = v;
      }
    }
    """
    Given the following java class:
    """
    public class BeanRef {
      public final Bean bean;
      public BeanRef(Bean b) {
        this.bean = b;
      }
    }
    """
    Given the following java class:
    """
    public class BeanList extends ArrayList<BeanRef> {
      public BeanList() {
        add(new BeanRef(new Bean("a")));
        add(new BeanRef(new Bean("b")));
      }
    }
    """
    And the following schema class:
    """
    public class BeanRefSchema implements Schema{
      public BeanSchema bean;
    }
    """
    And the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfId", field = "id")
    })
    public class BeanSchema implements Schema{
    }
    """
    Then the following verification for the instance of java class "BeanList" should pass:
    """
    is [BeanRefSchema]: {
      bean[].aliasOfId= [a b]
    }
    """

  Scenario: raise error when get element error
    Given the following java class:
    """
    public class Money {
      public int value() {
        throw new java.lang.RuntimeException("Error");
      }
    }
    """
    Given the following java class:
    """
    public class Product {
      public Money price = new Money();
    }
    """
    Given the following java class:
    """
    public class Order {
      public Product product = new Product();
    }
    """
    Given the following java class:
    """
    public class Data {
      public List<Order> orders = new ArrayList() {{
        add(new Order());
      }};
    }
    """
    When use a instance of java class "Data" to evaluate:
    """
    orders.product[].price.value.invalid.invalid= []
    """
    Then failed with the message:
    """
    Mapping element[0]:
    Get property `value` failed, property can be:
      1. public field
      2. public getter
      3. public no args method
      4. Map key value
      5. customized type getter
      6. static method extension
    java.lang.RuntimeException: Error
    """
    And got the following notation:
    """
    orders.product[].price.value.invalid.invalid= []
                                         ^
    """
    When use a instance of java class "Data" to evaluate:
    """
    orders.product[].price.value.invalid.invalid[0]: {...}
    """
    Then failed with the message:
    """
    Mapping element[0]:
    Get property `value` failed, property can be:
      1. public field
      2. public getter
      3. public no args method
      4. Map key value
      5. customized type getter
      6. static method extension
    java.lang.RuntimeException: Error
    """
    And got the following notation:
    """
    orders.product[].price.value.invalid.invalid[0]: {...}
                                                ^
    """
