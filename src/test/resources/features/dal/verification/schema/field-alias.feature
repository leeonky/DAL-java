Feature: define field alias in schema

  Scenario: define field alias in schema
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfId", field = "id")
    })
    public class Order implements Schema {
    }
    """
    And the following json:
    """
      {
        "id": 0
      }
    """
    Then the following verification should pass:
    """
      is Order which .aliasOfId = 0
    """

  Scenario: recursive alias
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfId", field = "id"),
            @FieldAlias(alias = "aliasOfAliasId", field = "aliasOfId")
    })
    public class Order implements Schema {
    }
    """
    And the following json:
    """
      {
        "id": 0
      }
    """
    Then the following verification should pass:
    """
      is Order which .aliasOfAliasId = 0
    """

  Scenario: alias of list element accessing
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "firstLine", field = "lines[0]"),
    })
    public class Order implements Schema {
    }
    """
    And the following json:
    """
      {
        "lines": [{
          "amount": 100
        }]
      }
    """
    Then the following verification should pass:
    """
      is Order which .firstLine.amount = 100
    """

  Scenario: alias of field chain
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "userName", field = "user.name")
    })
    public class Order implements Schema {
    }
    """
    And the following json:
    """
      {
        "user": {
          "name": "Tom"
        }
      }
    """
    Then the following verification should pass:
    """
      is Order which .userName = 'Tom'
    """

  Scenario: alias in sub schema
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfAge", field = "age"),
    })
    public class User implements Schema {
    }
    """
    And the following schema class:
    """
    @Partial
    public class Order implements Schema {
        public User user;
    }
    """
    And the following json:
    """
      {
        "user": {
          "age": 18
        }
      }
    """
    Then the following verification should pass:
    """
      is Order which .user.aliasOfAge = 18
    """

  Scenario: provide schema via is and use alias in object verification
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfId", field = "id")
    })
    public class Order implements Schema {
    }
    """
    And the following json:
    """
      {
        "id": 1
      }
    """
    Then the following verification should pass:
    """
      is Order: {
        aliasOfId: 1
      }
    """
    Then the following verification should pass:
    """
      is Order= {
        aliasOfId: 1
      }
    """

  Scenario: provide schema via is and use alias alias in nested object verification
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfAge", field = "age"),
    })
    public class User implements Schema {
    }
    """
    And the following schema class:
    """
    public class Order implements Schema {
        public User user;
    }
    """
    And the following json:
    """
      {
        "user": {
          "age": 10
        }
      }
    """
    Then the following verification should pass:
    """
      is Order: {
        user: {
          aliasOfAge: 10
        }
      }
    """
    Then the following verification should pass:
    """
      is Order: {
        user= {
          aliasOfAge: 10
        }
      }
    """

  Scenario: provide schema via is and use alias in list verification
    Given the following schema class:
    """
    public class Order implements Schema {
      public List<Product> products;
    }
    """
    And the following schema class:
    """
    @FieldAliases({
            @FieldAlias(alias = "aliasOfName", field = "name"),
    })
    public class Product implements Schema {
        public String name;
    }
    """
    And the following json:
    """
      {
        "products": [{
          "name": "ipad"
        }, {
          "name": "iphone"
        }]
      }
    """
    Then the following verification should pass:
    """
      is Order: {
        products: [
          {aliasOfName: 'ipad'}
          {aliasOfName: 'iphone'}
        ]
      }
    """
    When the following json:
    """
      {
        "products": [{
          "name": 100
        }]
      }
    """

  Scenario: provide schema via is and use alias in nested list verification
    Given the following schema class:
    """
    public class Image implements Schema {
      public List<List<Pixel>> pixels;
    }
    """
    And the following schema class:
    """
    @FieldAliases({
            @FieldAlias(alias = "aliasOfColor", field = "color"),
    })
    public class Pixel implements Schema {
      public String color;
    }
    """
    And the following json:
    """
      {
        "pixels": [
          [{"color": "red"}],
          [{"color": "black"}]
        ]
      }
    """
    Then the following verification should pass:
    """
      is Image: {
        pixels: [
          [{aliasOfColor: 'red'}],
          [{aliasOfColor: 'black'}]
        ]
      }
    """

  Scenario: provide schema via is and use alias in list mapping
    Given the following schema class:
    """
    public class Order implements Schema {
      public List<Product> products;
    }
    """
    And the following schema class:
    """
    public class Product implements Schema {
        public Catalog catalog;
    }
    """
    And the following schema class:
    """
    @FieldAliases({
            @FieldAlias(alias = "aliasOfName", field = "name"),
            @FieldAlias(alias = "aliasOfDescription", field = "description"),
            @FieldAlias(alias = "aliasOfSubType", field = "sub"),
    })
    @Partial
    public class Catalog implements Schema {
      public SubType sub;
    }
    """
    And the following schema class:
    """
    @FieldAliases({
            @FieldAlias(alias = "aliasOfId", field = "id"),
            @FieldAlias(alias = "aliasOfValue", field = "value"),
    })
    @Partial
    public class SubType implements Schema {
      public SubTypeValue value;
    }
    """
    And the following schema class:
    """
    @FieldAliases({
            @FieldAlias(alias = "aliasOfString", field = "string"),
    })
    @Partial
    public class SubTypeValue implements Schema {
    }
    """
    And the following json:
    """
      {
        "products": [{
          "catalog": {
            "name": "c1",
            "description": "catalog c1",
            "sub": {
              "id": 1,
              "value": {
                "string": "001"
              }
            }
          }
        }, {
          "catalog": {
            "name": "c2",
            "description": "catalog c2",
            "sub": {
              "id": 2,
              "value": {
                "string": "002"
              }
            }
          }
        }]
      }
    """
    Then the following verification should pass:
    """
      is Order: {
        products.catalog[]: [
          { aliasOfName: 'c1' }
          { aliasOfName: 'c2' }
        ]
        products.catalog[].aliasOfDescription: [ 'catalog c1' 'catalog c2' ]

        products.catalog[].aliasOfSubType: [
          { aliasOfId: 1 }
          { aliasOfId: 2 }
        ]
        products.catalog[].aliasOfSubType.aliasOfId: [ 1 2 ]

        products.catalog[].aliasOfSubType.aliasOfValue: [
          { aliasOfString: "001" }
          { aliasOfString: "002" }
        ]
        products.catalog[].aliasOfSubType.aliasOfValue.aliasOfString: [ "001" "002" ]

        products.catalog[].aliasOfSubType.value: [
          { aliasOfString: "001" }
          { aliasOfString: "002" }
        ]
        products.catalog[].aliasOfSubType.value.aliasOfString: [ "001" "002" ]
      }
    """

  Scenario: alias in super class and interface
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "nameInSuper", field = "name")
    })
    public class Super implements Schema {
    }
    """
    And the following schema class:
    """
    @FieldAliases({
            @FieldAlias(alias = "nameInInterface", field = "name")
    })
    public interface Interface extends Schema {
    }
    """
    And the following schema class:
    """
    @Partial
    public class SubSchema extends Super implements Interface {
    }
    """
    And the following json:
    """
      { "name": "hello" }
    """
    Then the following verification should pass:
    """
      is SubSchema: {
        nameInInterface: 'hello'
        nameInSuper: 'hello'
      }
    """

  Scenario: provide schema via object verification key
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfId", field = "id")
    })
    public class Order implements Schema {
    }
    """
    And the following json:
    """
      {
        "data": {
          "id": 0
        }
      }
    """
    Then the following verification should pass:
    """
    : {
      data is Order: {
        aliasOfId: 0
      }
    }
    """

  Scenario: provide schema via list verification element
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfId", field = "id")
    })
    public class Order implements Schema {
    }
    """
    And the following json:
    """
      [{
          "id": 0
      }]
    """
    Then the following verification should pass:
    """
    : [
        is Order: {
          aliasOfId: 0
        }
      ]
    """

  Scenario: provide element schema via is and use alias in which clause
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfId", field = "id")
    })
    public class Order implements Schema {
    }
    """
    And the following json:
    """
      [{
          "id": 0
      }]
    """
    Then the following verification should pass:
    """
    is [Order] which [0].aliasOfId: 0
    """

  Scenario: provide element schema via object verification key
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfId", field = "id")
    })
    public class Order implements Schema {
    }
    """
    And the following json:
    """
      {
        "data": [{
          "id": 0
        }]
      }
    """
    Then the following verification should pass:
    """
      : {
        data is [Order]: [{aliasOfId: 0}]
      }
    """

  Scenario: provide element schema via list verification key
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfId", field = "id")
    })
    public class Order implements Schema{
    }
    """
    And the following json:
    """
      [
        [{
          "id": 0
        }]
      ]
    """
    Then the following verification should pass:
    """
      : [
        is [Order]: [{aliasOfId: 0}]
      ]
    """

  Scenario: list contains should support field alias
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfId", field = "id")
    })
    public class Order implements Schema{
    }
    """
    And the following json:
    """
      [
        { "id": 0 },
        { "id": 1 },
        { "id": 2 },
        { "id": 3 }
      ]
    """
    Then the following verification should pass:
    """
      is [Order]: [
        ...
        {aliasOfId: 1}
        {aliasOfId: 2}
        ...
      ]
    """
