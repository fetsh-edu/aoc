package me.fetsh.adventofcode2020.day10;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Part2 {
    public static void main(String[] args) throws IOException {
        List<Long> input = Files.lines(Path.of(Part2.class.getResource("input.txt").getPath())).mapToLong(Long::parseLong).sorted().boxed().collect(Collectors.toList());
        input.add(0, 0L);
        input.add(input.get(input.size() - 1) + 3);

        System.out.println("--- Top to Bottom RECURSIVE ---");
        System.out.println(countPathsR(input, 3, input.size() - 1, new HashMap<>()));
        System.out.println("--- Bottom to Top ---");
        System.out.println(countPaths(input, 3));
    }

    public static Long countPathsR(List<Long> input, int maxStep, int index, Map<Integer, Long> cache){
        if (cache.containsKey(index)) return cache.get(index);
        var result = (index == 0)
                ? 1L
                : IntStream.rangeClosed(1, maxStep)
                .filter(i -> index - i >= 0)
                .filter(i -> input.get(index) - input.get(index - i) <= maxStep)
                .mapToLong(i -> countPathsR(input, maxStep, index - i, cache)).sum();
        cache.put(index, result);
        return result;
    }

    public static Long countPaths(List<Long> input, int maxStem){
        HashMap<Integer, Long> paths = new HashMap<>();
        paths.put(0, 1L);
        for(var i = 0; i < input.size() - 1; i++) {
            var num = input.get(i);
            for(var j = 1; j <= maxStem; j++) {
                var indToCheck = i + j;
                if(indToCheck < input.size() - 1 && input.get(indToCheck) - num <= maxStem) {
                    paths.merge(indToCheck, paths.get(i), Long::sum);
                }
            }
        }
        return paths.get(paths.size() - 1);
    }
}
