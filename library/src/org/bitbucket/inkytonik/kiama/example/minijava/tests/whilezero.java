class WhileZero {
    public static void main () {
        System.out.println (new WhileZeroClass ().run ());
    }
}

class WhileZeroClass {

    public int run () {
        int v;
        v = 0;
        while (false) {
            v = v + 1;
        }
        return v;
    }

}
