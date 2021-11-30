package me.fetsh.adventofcode2020.day09;


import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Optional;
import java.util.stream.IntStream;

public class Part1 {
    public static void main(String[] args) throws IOException {
        var numbers = Files.lines(Path.of(Part1.class.getResource("input.txt").getPath())).mapToLong(Long::parseLong).toArray();
        System.out.println(getInvalid(numbers, 25).get()); // 21 ms
    }

    public static Optional<Long> getInvalid(long[] numbers, int preambleSize) {
        return IntStream.range(preambleSize, numbers.length)
                .filter(i -> !isValid(numbers, preambleSize, i))
                .mapToObj(i -> numbers[i])
                .findFirst();
    }

    public static Boolean isValid(long[] numbers, int preambleSize, int index){
        return Arrays.stream(numbers, index - preambleSize, index)
                .anyMatch(n ->
                        n*2 != numbers[index] &&
                        Arrays.stream(numbers, index - preambleSize, index)
                                .anyMatch(s -> s == numbers[index] - n)
                );
    }

}
