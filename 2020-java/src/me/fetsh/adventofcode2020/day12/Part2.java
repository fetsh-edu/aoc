package me.fetsh.adventofcode2020.day12;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

record WaypointFerry(int wpx, int wpy, int x, int y){
    WaypointFerry instruct(String string){
        var value = Integer.parseInt(string.substring(1));
        return switch (string.charAt(0)){
            case 'F' -> move(value);
            case 'N', 'E', 'S', 'W' -> moveWaypoint(Direction.of(string.charAt(0)), value);
            case 'L', 'R' -> turnWaypoint(string.charAt(0), value);
            default -> throw new IllegalArgumentException("Illegal instruction: " + string);
        };
    }
    WaypointFerry move(int value){
        return new WaypointFerry(wpx, wpy, x + value * wpx, y + value * wpy);
    }
    WaypointFerry moveWaypoint(Direction direction, int value){
        return switch (direction) {
            case EAST -> new WaypointFerry(wpx + value, wpy, x, y);
            case WEST -> new WaypointFerry(wpx - value, wpy, x, y);
            case NORTH -> new WaypointFerry(wpx, wpy + value, x, y);
            case SOUTH -> new WaypointFerry(wpx, wpy - value, x, y);
        };
    }
    WaypointFerry turnWaypoint(char c, int value) {
        value = switch (c) {
            case 'L' -> value;
            case 'R' -> 360 - value;
            default -> throw new IllegalArgumentException("Illegal turn: " + c);
        };
        return switch (value){
            case 90 ->  new WaypointFerry(-wpy, wpx, x, y);
            case 180 ->  new WaypointFerry(-wpx, -wpy, x, y);
            case 270 ->  new WaypointFerry(wpy, -wpx, x, y);
            default -> throw new IllegalArgumentException("Illegal turn value: " + value);
        };
    }
    int manhattanDistance(){
        return Math.abs(x) + Math.abs(y);
    }
}

public class Part2 {
    public static void main(String[] args) throws IOException {
        var instructions = Files.lines(Path.of(Part1.class.getResource("input.txt").getPath()));
        var ferry = instructions.reduce(new WaypointFerry(10, 1, 0, 0), WaypointFerry::instruct, (a,b) -> a);
        System.out.println(ferry.manhattanDistance());
    }
}
