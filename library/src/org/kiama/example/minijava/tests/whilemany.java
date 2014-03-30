class WhileMany {
    public static void main () {
        System.out.println (new WhileManyClass ().run ());
    }
}

class WhileManyClass {

    public int run () {
        int v;
        v = 0;
        while (v < 10) {
            v = v + 1;
        }
        return v;
    }

}
