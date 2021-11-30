package me.fetsh.adventofcode2020.day16;

import me.fetsh.adventofcode2020.utils.File;

import java.io.IOException;
import java.util.*;
import java.util.stream.Stream;

record Range(int min, int max) {
    static Range parseRange(String string) {
        return new Range(Integer.parseInt(string.split("-")[0]), Integer.parseInt(string.split("-")[1]));
    }

    Boolean contains(int value) {
        return value >= min && value <= max;
    }
}

record Rule(String name, Range[] ranges) {
    static Rule parseRule(String string){
        return new Rule(
                string.split(": ")[0],
                Arrays.stream(string.split(": ")[1].split(" or "))
                        .map(Range::parseRange)
                        .toArray(Range[]::new)
                );
    }

    Boolean validForFields(Stream<Integer> fields){
        return fields.allMatch(
                field -> Arrays.stream(ranges)
                        .anyMatch(range -> range.contains(field)));
    }
}

record Ticket(int[] fields) {
    static Ticket parseTicket(String string){
        return new Ticket(Arrays.stream(string.split(",")).mapToInt(Integer::parseInt).toArray());
    }
    Boolean isValid(Rule[] rules){
        return Arrays.stream(fields)
                .allMatch(
                        field -> Arrays.stream(rules)
                                .flatMap(r -> Stream.of(r.ranges()))
                                .anyMatch(range -> range.contains(field))
                );
    }
    int get(int i){ return fields[i]; }
}

public class Part2 {
    public static void main(String[] args) throws IOException {
        var input = File.readAllBytes(Part1.class.getResource("input.txt").getPath()).split("\n\n");

        var rules= Arrays.stream(input[0].split("\n"))
                .map(Rule::parseRule)
                .toArray(Rule[]::new);

        var myTicket = Ticket.parseTicket(input[1].split("\n")[1]);

        var tickets = Arrays.stream(input[2].split("\n"))
                .skip(1)
                .map(Ticket::parseTicket)
                .toArray(Ticket[]::new);

        var validTickets = Arrays.stream(tickets).filter(ticket -> ticket.isValid(rules)).toArray(Ticket[]::new);

        HashMap<Integer, String[]> possiblePositions = new HashMap<>();
        HashMap<Integer, String> positions = new HashMap<>();
        Set<String> foundPositions = new HashSet<>();

        for (int i = 0; i < validTickets[1].fields().length; i++) {
            int finalI = i;
            var ruleNames = Arrays.stream(rules)
                    .filter(rule -> rule.validForFields(Arrays.stream(validTickets).map(t -> t.get(finalI))))
                    .map(Rule::name).toArray(String[]::new);
            possiblePositions.put(finalI, ruleNames);
        }
        possiblePositions
                .entrySet()
                .stream()
                .sorted(Comparator.comparingInt(c -> c.getValue().length))
                .forEach(e -> {
                    var position = Arrays.stream(e.getValue()).filter(p -> !foundPositions.contains(p)).findFirst();
                    foundPositions.add(position.get());
                    positions.put(e.getKey(), position.get());
                });

        var departureFields = positions.entrySet().stream().filter(e -> e.getValue().contains("departure")).mapToInt(Map.Entry::getKey);

        var myDepartureFields = departureFields.mapToLong(myTicket::get).reduce(1, (a, b) -> a * b);
        System.out.println(myDepartureFields);
    }
}
