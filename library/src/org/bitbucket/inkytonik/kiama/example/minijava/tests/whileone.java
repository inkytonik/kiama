class WhileOne {
    public static void main () {
        System.out.println (new WhileOneClass ().run ());
    }
}

class WhileOneClass {

    public int run () {
        int v;
        v = 0;
        while (v < 1) {
            v = v + 1;
        }
        return v;
    }

}
