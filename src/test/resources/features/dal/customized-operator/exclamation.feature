Feature: exclamation

  Scenario: support exclamation after property with some !
    Given the following java class:
    """
    public class Bean {
      public Bean(String s) {
        this.value = s;
      }
      public String value;
    }
    """
    Given the following java class:
    """
    public class BeanRef {
      public Bean bean;
      public BeanRef(Bean b){
        this.bean = b;
      }
    }
    """
    Given the following java class:
    """
    public class Beans {
      public Bean hello = new Bean("hello");
      public List<BeanRef> beanRefs = Arrays.asList(new BeanRef(new Bean("b1")), new BeanRef(new Bean("b2")));
      public List<Bean> beans = Arrays.asList(new Bean("b1"), new Bean("b2"));
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerExclamation(Bean.class, rd-> rd.data().map(i->((Bean)i).value + "!"));
    """
    When the following verification for the instance of java class "Beans" should pass:
    """
    : {
      hello! = hello!,
      ['hello']! = hello!

      beanRefs: | bean! |
                | b1!   |
                | b2!   |

      beanRefs: >>| bean! | b1! | b2! |

      {}:     | toUpperCase |
       hello! | HELLO!      |

      beans: | toUpperCase |
           ! | B1!         |
           ! | B2!         |

      beans: | >>          | !   | !   |
             | toUpperCase | B1! | B2! |

      beans: | toUpperCase |
        0!   | B1!         |
        1!   | B2!         |

      beans: | >>          | 0!  | 1!  |
             | toUpperCase | B1! | B2! |

      beans: | toUpperCase |
        [0]! | B1!         |
        [1]! | B2!         |

      beans: | >>          | [0]! | [1]! |
             | toUpperCase | B1!  | B2!  |
    }
    """

  Scenario: raise error when not register
    Given the following json:
    """
    [{
      "key": "k1",
      "value": "a"
    }, {
      "key": "k2",
      "value": "b"
    }]
    """
    When evaluate by:
    """
    : | key! | value |
      | k1   | a     |
      | k2   | b     |
    """
    Then failed with the message:
    """
    Not implement operator ! of java.lang.String
    """
    And got the following notation:
    """
    : | key! | value |
           ^
      | k1   | a     |
    ^^^^^^^^^^^^^^^^^^^
      | k2   | b     |
    """

  Scenario: exclamation chain
    When register DAL:
    """
    dal.getRuntimeContextBuilder()
      .registerExclamation(Map.class, rd-> rd.data().map(m->((Map)m).get("key")))
      .registerExclamation(String.class, rd-> rd.data().map(m->((String)m).toUpperCase()));
    """
    And the following json:
    """
    {
      "data": {
        "key": "hello"
      }
    }
    """
    When evaluate by:
    """
    data! !
    """
    Then the result should:
    """
    = HELLO
    """

  Scenario: exclamation in property chain
    When register DAL:
    """
    dal.getRuntimeContextBuilder()
      .registerExclamation(Map.class, rd-> rd.data().map(m->((Map)m).get("key")))
      .registerExclamation(String.class, rd-> rd.data().map(m->((String)m).toUpperCase()));
    """
    And the following json:
    """
    {
      "data": {
        "key": {
          "subKey": "hello"
        }
      }
    }
    """
    When evaluate by:
    """
    data!.subKey!
    """
    Then the result should:
    """
    = HELLO
    """
