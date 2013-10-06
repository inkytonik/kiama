class ArrayLength {
    public static void main () {
        System.out.println (new ArrayLengthClass ().run ());
    }
}

class ArrayLengthClass {

    public int run () {
        int[] arr;
        arr = new int[10];
        return arr.length;
    }

}
