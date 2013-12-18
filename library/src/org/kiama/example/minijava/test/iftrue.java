class IfTrue {
    public static void main () {
        System.out.println (new IfTrueClass ().run ());
    }
}

class IfTrueClass {

    public int run () {
        int v;
        if (true)
            v = 42;
        else
            v = 99;
        return v;
    }

}
