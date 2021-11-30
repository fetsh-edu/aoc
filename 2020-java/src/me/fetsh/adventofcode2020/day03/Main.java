package me.fetsh.adventofcode2020.day03;

import me.fetsh.adventofcode2020.utils.File;

import java.io.IOException;

import java.util.stream.IntStream;

class Forest {

    static class Pair {
        public final int index;
        public final String line;
        public Pair(int index, String line) {
            this.index = index;
            this.line = line;
        }
    }

    public static long treesEncountered(String[] forest, int down, int right){
        return IntStream.range(0, forest.length)
                .mapToObj(i -> new Pair(i, forest[i]))
                .filter(s -> s.index % down == 0)
                .map(s -> s.line.charAt( ((s.index / down) * right) % s.line.length() ))
                .filter(s -> s == '#').count();
    }
}


public class Main {
    public static void main(String[] args) throws IOException {
        var forest = File.readAllBytes(Main.class.getResource("input.txt").getPath()).split("\n");

        var r1 = Forest.treesEncountered(forest,1, 1);
        var r2 = Forest.treesEncountered(forest,1, 3);
        var r3 = Forest.treesEncountered(forest,1, 5);
        var r4 = Forest.treesEncountered(forest,1, 7);
        var r5 = Forest.treesEncountered(forest,2, 1);

        System.out.printf("%d * %d * %d * %d * %d = %d", r1, r2, r3, r4, r5, r1*r2*r3*r4*r5);
    }
}
