package tt.jointeuclid2ni;

import java.io.FileNotFoundException;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import tt.jointeuclid2ni.solver.Algorithm;
import tt.jointeuclid2ni.solver.Parameters;
import tt.jointeuclid2ni.solver.impl.AlgorithmORCA;
import tt.util.Args;

public class UnifiedSolver extends Solver {
    public static void main(String[] args) throws FileNotFoundException {
    	Parameters params = parseArguments(args);
    	
    	Algorithm algorithm;
        if (params.method.equals("ORCA")) {
            algorithm = new AlgorithmORCA();
        } else {
            algorithm = getAlgorithm(params.method, params.args);
        }
        
        solve(algorithm, params.problem, params);
    }
}
