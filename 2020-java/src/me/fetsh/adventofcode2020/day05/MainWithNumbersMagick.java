package me.fetsh.adventofcode2020.day05;


import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ListIterator;
import java.util.OptionalInt;
import java.util.stream.Collectors;

public class MainWithNumbersMagick {
    public static void main(String[] args) throws IOException {
        var seats = Files.lines(Path.of(MainLiterally.class.getResource("input.txt").getPath()))
                .map(MainWithNumbersMagick::seatId)
                .sorted()
                .collect(Collectors.toList());

        System.out.println("Highest seat: " + seats.get(seats.size() - 1));
        System.out.println("Empty seat: " + emptySeat(seats.get(0) - 1, seats.listIterator()));
    }

    public static OptionalInt emptySeat(int initSeat, ListIterator<Integer> iterator) {
        if (!iterator.hasNext()) return OptionalInt.empty();
        var nextSeat = initSeat + 1;
        var nextOccupied = (int) iterator.next();
        return nextSeat != nextOccupied ? OptionalInt.of(nextSeat) : emptySeat(nextOccupied, iterator);
    }

    public static int seatId(String pass) {
        return Integer.parseInt(
                pass.replaceAll("[FL]", "0")
                    .replaceAll("[BR]", "1"),
                2);
    }
}