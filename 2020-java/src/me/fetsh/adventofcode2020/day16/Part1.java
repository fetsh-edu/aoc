package me.fetsh.adventofcode2020.day16;

import me.fetsh.adventofcode2020.utils.File;

import java.io.IOException;
import java.util.Arrays;
import java.util.stream.IntStream;

public class Part1 {
    public static void main(String[] args) throws IOException {
        var input = File.readAllBytes(Part1.class.getResource("input.txt").getPath()).split("\n\n");
        var ranges = Arrays.stream(input[0].split("\n"))
                .map(s ->
                        Arrays.stream(s.split(": ")[1].split(" or "))
                                .map(r -> Arrays.stream(r.split("-")).mapToInt(i -> Integer.parseInt(i)).toArray())
                                .toArray(int[][]::new)
                )
                .toArray(int[][][]::new);

        var tickets = Arrays.stream(input[2].split("\n"))
                .skip(1)
                .map(s -> Arrays.stream(s.split(",")).mapToInt(Integer::parseInt).toArray())
                .toArray(int[][]::new);

        var errorRate = Arrays.stream(tickets).flatMapToInt(IntStream::of)
                .filter(value -> !inRanges(value, ranges))
                .sum();

        System.out.println(errorRate);
    }

    private static boolean inRanges(int value, int[][][] ranges) {
        return Arrays.stream(ranges).flatMap(Arrays::stream).anyMatch(r -> value >= r[0] && value <= r[1]);
    }
}
