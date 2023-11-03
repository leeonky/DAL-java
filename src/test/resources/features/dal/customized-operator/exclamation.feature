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
      hello!= hello!,
      ['hello']!= hello!

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
