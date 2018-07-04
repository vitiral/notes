
class HelloWorld
{
    public static void main(String[] args)
    {
        HelloWorld hi = new HelloWorld();
        hi.hello();
        hi.basicTypes();
        hi.weirdFloats();
        hi.casting();
        hi.ifStatement();
        hi.whileStatement();
        hi.forStatement();
        hi.tryClassVar();
    }

    void hello()
    {
        print("Hello world!");
    }

    void basicTypes()
    {
        print("\n# basicTypes:");
        int a = 4;
        int b = 7;
        print("a=" + a);
        print("a+b=" + (a+b));

        double x = 2.3234235;
        print("x=" + x);

        boolean bool = true;
        print("b=" + bool);

        int new_int = a + b;
        print("new_int=" + new_int);
    }

    void weirdFloats()
    {
        print("\n# wierdFloats:");
        double a = 3.3;

        print("Okay, these work...");
        print("a * 42 = " + (a * 42));
        print("a / 42 = " + (a / 42));

        print("\nBut this is weird:");
        a = 42 / 30;
        print("a = 42 / 30 => " + a);
        a = 42.0 / 30;
        print("a = 42.0 / 30 => " + a);

        print("\nBasically be cautios like in python2.");
    }

    void casting()
    {
        print("\n# casting:");
        double a = 42.345;
        print("a=" + a);
        print("(int) a" + ((int) a));
    }

    void ifStatement()
    {
        print("\n# ifStatement:");
        if (true) {
            print("got true");
        } else {
            print("AAAAGH");
        }
    }

    void whileStatement()
    {
        print("\n# whileStatement:");
        int x = 0;
        while (x<5) {
            print("x=" + x);
            x++;
        }
    }

    void forStatement()
    {
        print("\n# forStatement:");
        for( int x = 0; x<5; x++) {
            print("x=" + x);
        }
    }

    // CONSTRUCTOR
    //
    // Note: this stuff normally goes at the top
    private boolean bool;

    public HelloWorld() {
        bool = false;
    }

    public void setBool(boolean b) {
        bool = b;
    }

    public boolean getBool() {
        return bool;
    }

    void tryClassVar() {
        print("\n# tryClassVar:");
        print("this.bool=" + this.bool);
        print("bool=" + bool);
        print("getBool()=" + getBool());
        print("-> setBool(true)");
        setBool(true);
        print("this.bool=" + this.bool);
        print("-> setBool(false)");
        setBool(false);
        print("this.bool=" + this.bool);
    }


    // Internal Helpers:

    private void print(String s)
    {
        System.out.println(s);
    }
}
