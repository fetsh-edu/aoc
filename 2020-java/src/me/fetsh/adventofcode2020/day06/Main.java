package me.fetsh.adventofcode2020.day06;

import me.fetsh.adventofcode2020.utils.File;

import java.io.IOException;
import java.util.Arrays;
import java.util.stream.Collectors;

public class Main {
    public static void main(String[] args) throws IOException {
        var groupAnswers = File.readAllBytes(Main.class.getResource("input.txt").getPath()).split("\n\n");

        // Part 1 Version 1
        var sum1_0 = Arrays.stream(groupAnswers)
                .mapToInt(ga -> (int) Arrays.stream(ga.split("")).filter(s -> !s.isBlank()).distinct().count())
                .sum();
        System.out.println(sum1_0);

        // Part 1 Version 2
        var sum1_1 = Arrays.stream(groupAnswers)
                .mapToInt(ga -> Arrays.stream(ga.split("")).filter(s -> !s.isBlank()).collect(Collectors.toSet()).size() )
                .sum();
        System.out.println(sum1_1);

        // Part 2
        var sum2 = Arrays.stream(groupAnswers)
                .mapToInt(ga -> {
                    var personAnswers = ga.split("\n");
                    return Arrays.stream(ga.split("")).filter(s -> !s.isBlank()).filter(
                            a -> Arrays.stream(personAnswers).allMatch(pa -> pa.contains(a))
                    ).collect(Collectors.toSet()).size();
                })
                .sum();
        System.out.println(sum2);
    }

}
