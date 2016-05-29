class IfFalse {
    public static void main () {
        System.out.println (new IfFalseClass ().run ());
    }
}

class IfFalseClass {

    public int run () {
        int v;
        if (false)
            v = 42;
        else
            v = 99;
        return v;
    }

}
