package me.fetsh.adventofcode2020.day11;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.stream.IntStream;

public class Part2 {
    public static void main(String[] args) throws IOException {
        String[][] input = Files.lines(Path.of(Part2.class.getResource("input.txt").getPath())).map(s -> s.split("")).toArray(String[][]::new);
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
        var a = adjacents(input, i, j);
        if (input[i][j].equals("L") && a.stream().noneMatch(s -> s.equals("#"))) return "#";
        if (input[i][j].equals("#") && a.stream().filter(s -> s.equals("#")).count() >= 5) return "L";
        return input[i][j];
    }

    public static LinkedList<String> adjacents(String[][] input, int i, int j){
        var result = new LinkedList<String>();
        for (int k = i + 1; k < input.length; k++) {
            if (input[k][j].equals(".")) continue;
            result.push(input[k][j]);
            break;
        }
        for (int k = i - 1; k >= 0; k--) {
            if (input[k][j].equals(".")) continue;
            result.push(input[k][j]);
            break;
        }

        for (int l = j + 1; l < input[i].length; l++) {
            if (input[i][l].equals(".")) continue;
            result.push(input[i][l]);
            break;
        }

        for (int l = j - 1; l >= 0; l--) {
            if (input[i][l].equals(".")) continue;
            result.push(input[i][l]);
            break;
        }

        for (int k = i + 1, l = j + 1; k < input.length && l < input[i].length; k++, l++) {
            if (input[k][l].equals(".")) continue;
            result.push(input[k][l]);
            break;
        }
        for (int k = i - 1, l = j - 1; k >= 0  && l >= 0; k--, l--) {
            if (input[k][l].equals(".")) continue;
            result.push(input[k][l]);
            break;
        }
        for (int k = i + 1, l = j - 1; k < input.length && l >= 0; k++, l--) {
            if (input[k][l].equals(".")) continue;
            result.push(input[k][l]);
            break;
        }
        for (int k = i - 1, l = j + 1; k >= 0 && l < input[k].length; k--, l++) {
            if (input[k][l].equals(".")) continue;
            result.push(input[k][l]);
            break;
        }

        return result;
    }
}
