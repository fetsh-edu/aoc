package me.fetsh.adventofcode2020.utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class File {
    public static String readAllBytes(String path) throws IOException {
        return new String(Files.readAllBytes(Path.of(path)));
    }
}
