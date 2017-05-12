package pp.block3.cc.tabular;

import java.io.FileWriter;
import java.io.IOException;

/**
 * Created by Wouter on 12-5-2017.
 */
public class HTMLWriter {

    public HTMLWriter() {

    }

    public static void write(String s, String filename) {
        FileWriter writer;
        try {
            writer = new FileWriter(filename);
            writer.write(s);
            writer.flush();
            writer.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
