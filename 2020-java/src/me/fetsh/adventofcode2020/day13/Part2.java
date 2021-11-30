package me.fetsh.adventofcode2020.day13;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Part2 {

    public static String[] bussesInput;
    public static int[][] busses;

    public static void main(String[] args) throws IOException {
        bussesInput = Files.lines(Path.of(Part2.class.getResource("input.txt").getPath())).skip(1).findFirst().get().split(",");
        busses = IntStream.range(0, bussesInput.length).filter(i -> !bussesInput[i].equals("x")).mapToObj(i -> new int[]{i, Integer.parseInt(bussesInput[i])}).toArray(int[][]::new);
        System.out.println(getTimestamp(0, 1, 0, busses[0][1]));
    }
    static long getTimestamp(int bus1, int bus2, long timestamp, long step){
        if (isLuckyTimestamp(bus1, bus2, timestamp)) {
            if (bus2 == busses.length - 1) return timestamp;
            return getTimestamp(bus2, bus2 + 1, timestamp, step * busses[bus2][1]);
        } else {
            return getTimestamp(bus1, bus2, timestamp + step, step);
        }
    }

    static boolean isLuckyTimestamp(int bus1, int bus2, long ts) {
        return Stream.of(new int[][]{busses[bus1], busses[bus2]}).allMatch(b -> (ts + b[0]) % b[1] == 0);
    }
}
