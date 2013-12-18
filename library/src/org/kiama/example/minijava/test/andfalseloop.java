class AndFalseLoop {
    public static void main () {
        System.out.println (false && (new LoopClass ().run ()));
    }
}

class LoopClass {

    public boolean run () {
        while (true) {}
        return false;
    }

}
