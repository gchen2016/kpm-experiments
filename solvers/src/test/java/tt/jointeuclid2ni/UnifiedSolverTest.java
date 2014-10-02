package tt.jointeuclid2ni;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.PrintStream;

public class UnifiedSolverTest {

    private final ByteArrayOutputStream outContent = new ByteArrayOutputStream();
    private final ByteArrayOutputStream errContent = new ByteArrayOutputStream();

    @Before
    public void setUpStreams() {
        System.setOut(new PrintStream(outContent));
        System.setErr(new PrintStream(errContent));
    }

    @After
    public void cleanUpStreams() {
        System.setOut(null);
        System.setErr(null);
    }

    protected void testSolver(String argStr, String expectedOutputRegExp) throws FileNotFoundException {
        String[] args = argStr.split(" ");
        UnifiedSolver.main(args);
        String s = outContent.toString();

        boolean matches = (outContent.toString().matches(expectedOutputRegExp));

        if (!matches) {
            org.junit.Assert.fail("Expected output: " + expectedOutputRegExp + "Actual output: " + s);
        }
    }

    @Test
    public void testORCAOnCrossconflict() throws FileNotFoundException {

        testSolver("-method ORCA -problemfile src/test/resources/problems/cross_conflict.xml -timeout 20000 -maxtime 2500 -gridstep 50 -grid 8 -summary",
                "^2[0-9][0-9][0-9].00;[0-9]*;[0-9]*;SUCCESS;\n");

//        try {
//            Thread.sleep(10000);
//        } catch (InterruptedException e) {
//        }
    }

}
