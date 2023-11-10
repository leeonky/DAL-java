Feature: data remark

  Scenario: support remark after property with ()
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
    dal.getRuntimeContextBuilder().registerDataRemark(Bean.class, rd-> rd.data().map(i->((Bean)i).value+rd.remark()));
    """
    When the following verification for the instance of java class "Beans" should pass:
    """
    : {
      hello(World)= helloWorld,
      ['hello'](World)= helloWorld

      beanRefs: | bean(remark) |
                | b1remark     |
                | b2remark     |

      beanRefs: >>| bean(remark) | b1remark | b2remark |

      {}:           | toUpperCase |
       hello(World) | HELLOWORLD  |

      beans:    | toUpperCase |
        (hello) | B1HELLO     |
        (World) | B2WORLD     |

      beans: | >>          | (hello) | (World) |
             | toUpperCase | B1HELLO | B2WORLD |

      beans:     | toUpperCase |
        0(HELLO) | B1HELLO     |
        1(World) | B2WORLD     |

      beans: | >>          | 0(hello) | 1(World) |
             | toUpperCase | B1HELLO  | B2WORLD  |

      beans:       | toUpperCase |
        [0](HELLO) | B1HELLO     |
        [1](World) | B2WORLD     |

      beans: | >>          | [0](hello) | [1](World) |
             | toUpperCase | B1HELLO    | B2WORLD    |
    }
    """

  Scenario: raise error when no remark register
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
    : | key(xx) | value |
      | k1      | a     |
      | k2      | b     |
    """
    Then failed with the message:
    """
    Not implement operator () of java.lang.String
    """
    And got the following notation:
    """
    : | key(xx) | value |
           ^
      | k1      | a     |
    ^^^^^^^^^^^^^^^^^^^^^^
      | k2      | b     |
    """

  Scenario: data-remark-chain
    When register DAL:
    """
    dal.getRuntimeContextBuilder()
      .registerDataRemark(Map.class, rd-> rd.data().map(m->((Map)m).get(rd.remark())))
      .registerDataRemark(String.class, rd-> rd.data().map(m->m+rd.remark()));
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
    data(key)(world)
    """
    Then the result should:
    """
    = helloworld
    """

  Scenario: data-remark in property chain
    When register DAL:
    """
    dal.getRuntimeContextBuilder()
      .registerDataRemark(Map.class, rd-> rd.data().map(m->((Map)m).get(rd.remark())));
    """
    And the following json:
    """
    {
      "data": {
        "key": {
          "sub": {
            "value": "hello"
          }
        }
      }
    }
    """
    When evaluate by:
    """
    data(key).sub(value)
    """
    Then the result should:
    """
    = hello
    """
