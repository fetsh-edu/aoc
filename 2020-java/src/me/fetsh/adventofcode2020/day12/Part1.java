package me.fetsh.adventofcode2020.day12;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

enum Direction {
    EAST(0), NORTH(90), WEST(180), SOUTH(270);

    int value;
    Direction(int value) { this.value = value; }

    public static Direction of(char c){
        return switch (c) {
            case 'N' -> NORTH;
            case 'E' -> EAST;
            case 'S' -> SOUTH;
            case 'W' -> WEST;
            default -> throw new IllegalArgumentException("Illegal direction: " + c);
        };
    }
    public static Direction of(int d){
        return Arrays.stream(Direction.values()).filter(s -> s.value == d % 360).findFirst().orElseThrow();
    }
}
record Ferry(Direction dir, int x, int y){
    Ferry instruct(String string){
        var value = Integer.parseInt(string.substring(1));
        return switch (string.charAt(0)){
            case 'N', 'E', 'S', 'W' -> move(Direction.of(string.charAt(0)), value);
            case 'L', 'R' -> turn(string.charAt(0), value);
            case 'F' -> move(dir, value);
            default -> throw new IllegalArgumentException("Illegal instruction: " + string);
        };
    }
    Ferry move(Direction dir, int value) {
        return switch (dir) {
            case EAST -> new Ferry(this.dir, this.x + value, this.y);
            case WEST -> new Ferry(this.dir, this.x - value, this.y);
            case NORTH -> new Ferry(this.dir, this.x, this.y + value);
            case SOUTH -> new Ferry(this.dir, this.x, this.y - value);
        };
    }

    Ferry turn(char c, int value){
        return switch (c) {
            case 'L' -> new Ferry(Direction.of(dir.value + value), x, y);
            case 'R' -> new Ferry(Direction.of(dir.value + 360 - value), x, y);
            default -> throw new IllegalArgumentException("Illegal turn: " + c);
        };
    }
    int manhattanDistance(){
        return Math.abs(x) + Math.abs(y);
    }
}

public class Part1 {
    public static void main(String[] args) throws IOException {
        var instructions = Files.lines(Path.of(Part1.class.getResource("input.txt").getPath()));
        var ferry = instructions.reduce(new Ferry(Direction.EAST, 0, 0), Ferry::instruct, (a,b) -> a);
        System.out.println(ferry.manhattanDistance());
    }
}
