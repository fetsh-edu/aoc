package me.fetsh.adventofcode2020.day09;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.IntStream;

public class Part2 {
    public static void main(String[] args) throws IOException {
        var numbers = Files.lines(Path.of(Part2.class.getResource("input.txt").getPath())).mapToLong(Long::parseLong).toArray();
        var invalid = Part1.getInvalid(numbers, 25).get();

        var summary = IntStream.range(0, numbers.length)
                .mapToObj(i -> conSet(numbers, i, invalid, 0, new ArrayList<>()))
                .filter(Optional::isPresent)
                .map(Optional::get)
                .findFirst()
                .get()
                .stream()
                .mapToLong(Long::longValue)
                .summaryStatistics();

        System.out.println(summary.getMin() + summary.getMax()); // 15 ms
    }

    public static Optional<List<Long>> conSet(long[] numbers, int ind, long target, long sum, List<Long> set) {
        if (sum == target) return Optional.of(set);
        if (sum > target) return Optional.empty();
        set.add(numbers[ind]);
        return conSet(numbers, ind + 1, target, sum + numbers[ind], set);
    }
}