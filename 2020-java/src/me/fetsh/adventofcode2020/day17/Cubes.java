package me.fetsh.adventofcode2020.day17;

import io.vavr.collection.List;
import io.vavr.collection.Stream;
import me.fetsh.adventofcode2020.utils.File;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class Cubes {
    public static void main(String[] args) throws IOException {
        var input = File.readAllBytes(Cubes.class.getResource("input.txt").getPath());

        final int steps = 6;
        final int dimensions = 4;

        var initMap = List.of(input.split("\n")).map(s -> List.of(s.split("")));

        var activeCubes = List.range(0, initMap.length())
                .flatMap(x ->
                        List.range(0, initMap.get(x).length())
                                .filter(y -> initMap.get(x).get(y).equals("#"))
                                .map(y -> List.of(x, y).appendAll(Stream.continually(0).take(dimensions - 2)))
                ).toSet();

        for (int i = 0; i < steps; i++) {
            var finalActiveCubes = activeCubes;
            var ncounter = new HashMap<List<Integer>, Integer>();

            activeCubes.forEach(cube -> List.of(-1,0, 1).crossProduct(dimensions)
                    .map(it -> it.zipWith(cube, Integer::sum))
                    .filter(c -> !c.equals(cube))
                    .forEach(n -> ncounter.merge(n, 1, Integer::sum)));

            var remainsActive = activeCubes
                    .filter(c -> List.of(2,3).contains(ncounter.get(c)));

            var becameActive = List.ofAll(ncounter.entrySet())
                            .filter(e -> !finalActiveCubes.contains(e.getKey()) && e.getValue() == 3)
                            .map(Map.Entry::getKey)
                            .toSet();

            activeCubes = remainsActive.union(becameActive);
        }

        System.out.println(activeCubes.length());
    }
}
