package me.fetsh.adventofcode2020.day07;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

public class Main {
    public static void main(String[] args) throws IOException {
        Map<String, Map<String, Integer>> rulesMap = Files.lines(Path.of(Main.class.getResource("input.txt").getPath()))
                .map(s -> s.split("\s+contain\s+"))
                .collect(Collectors.toMap(
                        s -> s[0].substring(0, s[0].indexOf("bag") - 1),
                        s -> s[1].equals("no other bags.")
                                ? Collections.<String, Integer>emptyMap()
                                : Arrays.stream(s[1].split(",\s")).collect(Collectors.toMap(
                                a -> a.substring(2, a.indexOf("bag") - 1),
                                a -> Integer.parseInt(a.substring(0, 1))
                        ))
                ));
        // Part 1
        System.out.println(rulesMap.entrySet().stream().filter(e -> contains(rulesMap, e.getKey(), "shiny gold")).count());

        // Part 2
        System.out.println(bagsInside(rulesMap, "shiny gold"));
    }

    static boolean contains(Map<String, Map<String, Integer>> rulesMap, String source, String target) {
        return rulesMap.get(source).keySet().contains(target)
                || rulesMap.get(source).keySet().stream().anyMatch(s -> contains(rulesMap, s, target));
    }

    static int bagsInside(Map<String, Map<String, Integer>> rulesMap, String color) {
        return rulesMap.get(color).entrySet().stream()
                .mapToInt(es -> es.getValue() + es.getValue() * bagsInside(rulesMap, es.getKey()))
                .sum();
    }
}
