package me.fetsh.adventofcode2020.day15;

public class Part1 {
    public static void main(String[] args) {
        var input = new int[]{2,0,1,9,5,19};
        System.out.println(play(input, 30000000));
    }

    private static int play(int[] input, int rounds) {
        var storage = new int[rounds];
        for (int i = 0; i < input.length - 1; i++) {
            storage[input[i]] = i + 1;
        }
        var prev = input[input.length - 1];
        for (var turn = input.length; turn < rounds; turn++) {
            var prevTurn = storage[prev];
            storage[prev] = turn;
            prev = prevTurn != 0 ? turn - prevTurn : 0;
        }
        return prev;
    }
}
