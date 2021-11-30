package me.fetsh.adventofcode2020.day05;


import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.ListIterator;
import java.util.OptionalInt;
import java.util.stream.Collectors;

public class MainLiterally {
    public static void main(String[] args) throws IOException {
        var seats = Files.lines(Path.of(MainLiterally.class.getResource("input.txt").getPath()))
                .map(MainLiterally::seatId)
                .sorted()
                .collect(Collectors.toList());

        System.out.println("Highest seat: " + seats.get(seats.size() - 1));
        System.out.println("Empty seat: " + emptySeat(seats.get(0) - 1, seats.listIterator()));
    }

    public static OptionalInt emptySeat(int initSeat, ListIterator<Integer> iterator){
        if(!iterator.hasNext()) return OptionalInt.empty();
        var nextSeat =  initSeat + 1;
        var nextOccupied = (int) iterator.next();
        return nextSeat != nextOccupied ? OptionalInt.of(nextSeat) : emptySeat(nextOccupied, iterator);
    }

    public static int seatId(String pass){
        var row = Arrays.stream(pass.substring(0, 7).split(""))
                .reduce(new Range(0,127), Range::apply, (b,c) -> b).min;
        var seat = Arrays.stream(pass.substring(7).split(""))
                .reduce(new Range(0,7), Range::apply, (b,c) -> b).min;
        return row * 8 + seat;
    }
}

class Range{
    public final int min;
    public final int max;
    Range(int min, int max){
        this.min = min;
        this.max = max;
    }
    public Range apply(String ch){
        if (ch.equals("F") || ch.equals("L")){
            return new Range(this.min, this.min + (this.max - this.min) / 2);
        } else {
            return new Range(this.max - (this.max - this.min) / 2, this.max);
        }
    }
}