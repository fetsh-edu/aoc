package me.fetsh.adventofcode2020.day11;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.stream.IntStream;

public class Part1 {
    public static void main(String[] args) throws IOException {
        String[][] input = Files.lines(Path.of(Part1.class.getResource("input.txt").getPath())).map(s -> s.split("")).toArray(String[][]::new);
        System.out.println(
                Arrays.stream(flipUntilStabilized(input))
                        .flatMap(Arrays::stream)
                        .filter(s -> s.equals("#"))
                        .count()
        );
    }

    public static String[][] flipUntilStabilized(String[][] input){
        var inputCopy = Arrays.stream(input).map(String[]::clone).toArray(String[][]::new);
        String[][] result;
        while(true) {
            result = flipSeats(inputCopy);
            if (Arrays.deepEquals(inputCopy, result)) break;
            inputCopy = result;
        }
        return result;
    }

    public static String[][] flipSeats(String[][] input) {
        return IntStream.range(0, input.length)
                .mapToObj(i ->
                        IntStream.range(0, input[i].length)
                                .mapToObj(j -> flipSeat(input, i, j)).toArray(String[]::new)
                ).toArray(String[][]::new);
    }

    public static String flipSeat(String[][] input, int i, int j){
        if (input[i][j].equals(".")) return ".";
        if (input[i][j].equals("L") && adjacents(input, i, j).stream().noneMatch(s -> s.equals("#"))) return "#";
        if (input[i][j].equals("#") && adjacents(input, i, j).stream().filter(s -> s.equals("#")).count() >= 4) return "L";
        return input[i][j];
    }

    public static LinkedList<String> adjacents(String[][] input, int i, int j){
        var result = new LinkedList<String>();
        if (i - 1 >= 0) {
            if (j - 1 >= 0) {
                result.push(input[i -1][j - 1]);
            }
            result.push(input[i - 1][j]);
            if (j + 1 < input[i - 1].length) {
                result.push(input[i - 1][j + 1]);
            }
        }
        if (j - 1 >= 0) {
            result.push(input[i][j - 1]);
        }
        if (j + 1 < input[i].length) {
            result.push(input[i][j + 1]);
        }

        if (i + 1 < input.length) {
            if (j - 1 >= 0) {
                result.push(input[i  + 1][j - 1]);
            }
            result.push(input[i + 1][j]);
            if (j + 1 < input[i + 1].length) {
                result.push(input[i + 1][j + 1]);
            }
        }
        return result;
    }
}
